module AnimationRunner.Program

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open Tracer.Basics
open Tracer.Animation

type Options =
    { Demo: string option
      Scene: string option
      Clip: string option
      Settings: FrameSettings
      Start: int
      End: int option
      /// Renders only these frames (from one scene load), e.g. for review stills.
      Only: int list option
      Frames: int option
      Seed: int
      FixedNoise: bool
      /// Render settings left unset here fall back to the animation's recommended ones, then to the runner's.
      Spp: int option
      Integrator: IntegratorKind option
      Denoise: bool option
      Transfer: string option
      Bounces: int option
      Clamp: float option
      Bloom: float option
      Vignette: float option
      Exposure: float option
      Saturation: float option
      WhiteBalance: float option
      Output: string option
      Resume: bool
      Video: bool
      ExportGltf: string option
      LightScale: float
      Telegram: bool
      Audio: bool
      AudioOnly: bool }

let private defaults =
    { Demo = None; Scene = None; Clip = None; Settings = FrameSettings.Default; Start = 0; End = None; Only = None; Frames = None
      Seed = 2026; FixedNoise = false; Spp = None; Integrator = None; Denoise = None; Transfer = None; Bounces = None; Clamp = None
      Bloom = None; Vignette = None; Exposure = None; Saturation = None; WhiteBalance = None
      Output = None; Resume = true; Video = true; ExportGltf = None; LightScale = 1.; Telegram = false; Audio = true; AudioOnly = false }

let usage () =
    printfn """Usage: AnimationRunner (--demo NAME | --scene FILE.gltf|.glb [--clip NAME] | --list) [options]
  --fps N              frames per second (default 24)
  --frames N           render N frames from --start (default: the whole animation)
  --start S / --end E  frame range [S, E)
  --only A,B,C         render just these frames, loading the scene once
  --clamp F            cap indirect contributions at F (path integrator; 0 disables)
  --res WxH            resolution (default 640x360)
  --spp N              samples per pixel (default 16, or the animation's recommendation)
  --integrator K       classic | path (default classic, or the animation's recommendation)
  --bounces N          maximum light bounces (default: the animation's)
  --shutter F          shutter as a fraction of the frame, 0 disables motion blur (default 0.5)
  --motion-steps N     poses sampled across the shutter (default 3)
  --seed N             base seed (default 2026); each frame gets its own unless --fixed-noise
  --denoise            run Open Image Denoise (path integrator only); --no-denoise turns off a recommended denoise
  --transfer T         srgb | gamma2 | aces | linear (default srgb, or the animation's recommendation)
  --bloom S            bloom strength, 0 disables (default 0, or the animation's recommendation)
  --vignette V         corner darkening in [0, 1]
  --exposure X         linear exposure multiplier (default 1)
  --saturation S       1 leaves colours alone, 0 is greyscale
  --white-balance W    warm (> 0) or cool (< 0) shift, about -1 to 1
                       (Animations such as dragon-flight recommend path + denoise + aces and a light grade.)
  --out DIR            output directory (default artifacts/anim/NAME)
  --no-resume          re-render frames that already exist
  --no-video           skip the ffmpeg MP4
  --no-audio           leave out the soundtrack (for animations that have one)
  --audio-only         write the soundtrack WAV only, without rendering
  --light-scale F      multiplies the intensity of lights imported from glTF (default 1)
  --telegram           send the finished MP4 to Telegram; needs TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID
                       (exit code 3 if the frames rendered but delivery failed)
  --export-gltf FILE   write the scene and its animation as glTF 2.0 (.gltf or .glb) and exit"""

let parse (argv: string[]) =
    let number name (value: string) =
        match Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, v when Double.IsFinite v -> v
        | _ -> invalidArg name $"--{name} expects a number, got {value}."
    let integer name value =
        let v = number name value
        if v <> floor v then invalidArg name $"--{name} expects an integer, got {value}."
        int v
    let rec go options args =
        match args with
        | [] -> options
        | "--demo" :: v :: rest -> go { options with Demo = Some v } rest
        | "--scene" :: v :: rest -> go { options with Scene = Some v } rest
        | "--clip" :: v :: rest -> go { options with Clip = Some v } rest
        | "--fps" :: v :: rest -> go { options with Settings = { options.Settings with Fps = number "fps" v } } rest
        | "--frames" :: v :: rest -> go { options with Frames = Some (integer "frames" v) } rest
        | "--start" :: v :: rest -> go { options with Start = integer "start" v } rest
        | "--end" :: v :: rest -> go { options with End = Some (integer "end" v) } rest
        | "--only" :: v :: rest -> go { options with Only = Some [ for f in v.Split(',') -> integer "only" f ] } rest
        | "--res" :: v :: rest ->
            match v.ToLowerInvariant().Split('x') with
            | [| w; h |] -> go { options with Settings = { options.Settings with Width = integer "res" w; Height = integer "res" h } } rest
            | _ -> invalidArg "res" "--res expects WIDTHxHEIGHT."
        | "--spp" :: v :: rest -> go { options with Spp = Some (integer "spp" v) } rest
        | "--integrator" :: v :: rest ->
            let kind =
                match v.ToLowerInvariant() with
                | "classic" -> Classic
                | "path" -> Path
                | _ -> invalidArg "integrator" "--integrator expects classic or path."
            go { options with Integrator = Some kind } rest
        | "--shutter" :: v :: rest -> go { options with Settings = { options.Settings with Shutter = number "shutter" v } } rest
        | "--motion-steps" :: v :: rest -> go { options with Settings = { options.Settings with MotionSteps = integer "motion-steps" v } } rest
        | "--seed" :: v :: rest -> go { options with Seed = integer "seed" v } rest
        | "--fixed-noise" :: rest -> go { options with FixedNoise = true } rest
        | "--denoise" :: rest -> go { options with Denoise = Some true } rest
        | "--no-denoise" :: rest -> go { options with Denoise = Some false } rest
        | "--transfer" :: v :: rest -> go { options with Transfer = Some v } rest
        | "--bounces" :: v :: rest -> go { options with Bounces = Some (integer "bounces" v) } rest
        | "--clamp" :: v :: rest -> go { options with Clamp = Some (number "clamp" v) } rest
        | "--bloom" :: v :: rest -> go { options with Bloom = Some (number "bloom" v) } rest
        | "--vignette" :: v :: rest -> go { options with Vignette = Some (number "vignette" v) } rest
        | "--exposure" :: v :: rest -> go { options with Exposure = Some (number "exposure" v) } rest
        | "--saturation" :: v :: rest -> go { options with Saturation = Some (number "saturation" v) } rest
        | "--white-balance" :: v :: rest -> go { options with WhiteBalance = Some (number "white-balance" v) } rest
        | "--out" :: v :: rest -> go { options with Output = Some v } rest
        | "--no-resume" :: rest -> go { options with Resume = false } rest
        | "--no-video" :: rest -> go { options with Video = false } rest
        | "--telegram" :: rest -> go { options with Telegram = true } rest
        | "--no-audio" :: rest -> go { options with Audio = false } rest
        | "--audio-only" :: rest -> go { options with AudioOnly = true } rest
        | "--export-gltf" :: v :: rest -> go { options with ExportGltf = Some v } rest
        | "--light-scale" :: v :: rest -> go { options with LightScale = number "light-scale" v } rest
        | option :: _ -> invalidArg "arguments" $"Unknown or incomplete option {option}."
    let options = go defaults (List.ofArray argv)
    let s = options.Settings
    if s.Fps <= 0. then invalidArg "fps" "--fps must be positive."
    if s.Width <= 0 || s.Height <= 0 then invalidArg "res" "--res must be positive."
    if options.Spp |> Option.exists (fun n -> n <= 0) then invalidArg "spp" "--spp must be positive."
    if options.Bounces |> Option.exists (fun n -> n < 0) then invalidArg "bounces" "--bounces must be non-negative."
    if options.Bloom |> Option.exists (fun v -> v < 0.) then invalidArg "bloom" "--bloom must be non-negative."
    if options.Vignette |> Option.exists (fun v -> v < 0. || v > 1.) then invalidArg "vignette" "--vignette must lie in [0, 1]."
    if options.Exposure |> Option.exists (fun v -> v <= 0.) then invalidArg "exposure" "--exposure must be positive."
    if options.Saturation |> Option.exists (fun v -> v < 0.) then invalidArg "saturation" "--saturation must be non-negative."
    if s.Shutter < 0. || s.Shutter > 1. then invalidArg "shutter" "--shutter must lie in [0, 1]."
    if s.MotionSteps < 2 then invalidArg "motion-steps" "--motion-steps must be at least 2."
    if options.Start < 0 then invalidArg "start" "--start must be non-negative."
    if options.Telegram && not options.Video then invalidArg "telegram" "--telegram sends the MP4, so it cannot be combined with --no-video."
    match options.End, options.Frames with
    | Some _, Some _ -> invalidArg "frames" "Give --frames or --end, not both."
    | None, Some n -> { options with End = Some (options.Start + n) }
    | _ -> options

/// The settings a render actually uses: command-line flags, else the animation's recommendation, else the runner's.
type Effective =
    { Integrator: IntegratorKind
      Denoise: bool
      Transfer: string
      Bounces: int option
      Clamp: float
      Post: Post.PostSettings }

let resolve (options: Options) (scene: AnimatedScene) =
    let recommended = Catalog.renderDefaults scene.Name
    let pick flag (fromDefaults: RenderDefaults -> 'a) fallback =
        match flag with
        | Some v -> v
        | None -> recommended |> Option.map fromDefaults |> Option.defaultValue fallback
    let baseline = recommended |> Option.map (fun r -> r.Post) |> Option.defaultValue Post.PostSettings.Identity
    let spp = pick options.Spp (fun r -> r.SamplesPerPixel) options.Settings.SamplesPerPixel
    let effective =
        { Integrator = pick options.Integrator (fun r -> r.Integrator) Classic
          Denoise = pick options.Denoise (fun r -> r.Denoise) false
          Transfer = pick options.Transfer (fun r -> r.Transfer) "srgb"
          Bounces = options.Bounces |> Option.orElse (recommended |> Option.map (fun r -> r.MaxBounces))
          Clamp = pick options.Clamp (fun r -> r.IndirectClamp) 0.
          Post =
            { baseline with
                Bloom = defaultArg options.Bloom baseline.Bloom
                Vignette = defaultArg options.Vignette baseline.Vignette
                Exposure = defaultArg options.Exposure baseline.Exposure
                Saturation = defaultArg options.Saturation baseline.Saturation
                WhiteBalance = defaultArg options.WhiteBalance baseline.WhiteBalance } }
    effective, { options.Settings with SamplesPerPixel = spp }

/// Everything that affects the pixels of a frame; a resumed run must match it exactly.
let private manifest (options: Options) (effective: Effective) (settings: FrameSettings) (sceneName: string) =
    let s = settings
    dict [
        "scene", sceneName
        "clip", defaultArg options.Clip ""
        "fps", string s.Fps
        "resolution", $"{s.Width}x{s.Height}"
        "spp", string s.SamplesPerPixel
        "shutter", string s.Shutter
        "motionSteps", string s.MotionSteps
        "seed", string options.Seed
        "fixedNoise", string options.FixedNoise
        "integrator", string effective.Integrator
        "denoise", string effective.Denoise
        "transfer", effective.Transfer
        "bounces", (match effective.Bounces with Some n -> string n | None -> "")
        "clamp", string effective.Clamp
        "bloom", $"{effective.Post.Bloom}@{effective.Post.BloomThreshold}"
        "vignette", string effective.Post.Vignette
        "exposure", string effective.Post.Exposure
        "saturation", string effective.Post.Saturation
        "whiteBalance", string effective.Post.WhiteBalance
        "format", "1" ]

let private checkManifest (directory: string) (expected: Collections.Generic.IDictionary<string, string>) resume =
    let path = Path.Combine(directory, "manifest.json")
    let json = JsonSerializer.Serialize(expected, JsonSerializerOptions(WriteIndented = true))
    if resume && File.Exists path then
        let existing = JsonSerializer.Deserialize<Collections.Generic.Dictionary<string, string>>(File.ReadAllText path)
        let differences =
            expected |> Seq.filter (fun pair ->
                match existing.TryGetValue pair.Key with
                | true, value -> value <> pair.Value
                | _ -> true)
            |> Seq.map (fun pair -> pair.Key) |> List.ofSeq
        if not differences.IsEmpty then
            failwithf "%s holds frames rendered with different settings (%s). Use --no-resume or another --out."
                directory (String.Join(", ", differences))
    File.WriteAllText(path, json)

/// Frame seed: decorrelated per frame so noise does not stay glued to the screen.
let frameSeed baseSeed fixedNoise (frame: int) =
    if fixedNoise then baseSeed
    else int (Sampling.mixKey (uint64 (uint32 baseSeed) <<< 32 ||| uint64 (uint32 frame)) &&& 0x7fffffffUL)

/// Builds the animation's soundtrack (if it has one) for [start, start + length) and writes soundtrack.wav.
let soundtrack (options: Options) (scene: AnimatedScene) (directory: string) (start: float) (length: float) =
    match Catalog.soundtrack scene.Name with
    | None -> None
    | Some build ->
        let timer = Stopwatch.StartNew()
        let buffer = build scene (fun name -> Encode.decodeAudio (Film.asset name))
        let path = Path.Combine(directory, "soundtrack.wav")
        Audio.writeWav path (Audio.slice buffer start length)
        printfn "Soundtrack: %s (%.1fs to mix)" path timer.Elapsed.TotalSeconds
        Some path

let render (options: Options) (scene: AnimatedScene) =
    // Check delivery before rendering, so a missing credential is not discovered hours later.
    let telegram =
        if options.Telegram then
            if not (Encode.ffmpegAvailable ()) then invalidArg "telegram" "--telegram needs ffmpeg on PATH to encode the MP4."
            Some (Telegram.fromEnvironment ())
        else None
    let effective, settings = resolve options scene
    let scene = match effective.Bounces with Some n -> { scene with MaxBounces = n } | None -> scene
    IndirectClamp.Limit <- effective.Clamp
    let mutable delivered = true
    let directory = Path.GetFullPath(defaultArg options.Output (Path.Combine("artifacts", "anim", scene.Name)))
    Directory.CreateDirectory directory |> ignore
    checkManifest directory (manifest options effective settings scene.Name) options.Resume
    let last = defaultArg options.End (Frame.frameCount settings scene)
    if last <= options.Start then invalidArg "end" "The frame range is empty."
    let total = last - options.Start
    printfn "%s: frames %d..%d (%d) at %gfps, %dx%d, %dspp, shutter %g -> %s"
        scene.Name options.Start (last - 1) total settings.Fps settings.Width settings.Height
        settings.SamplesPerPixel settings.Shutter directory
    let timer = Stopwatch.StartNew()
    let mutable rendered = 0
    let frames = match options.Only with Some list -> list | None -> [ options.Start .. last - 1 ]
    for frame in frames do
        let path = Path.Combine(directory, $"frame_%05d{frame}.png")
        if options.Resume && File.Exists path && FileInfo(path).Length > 0L then
            printfn "  frame %5d  exists, skipped" frame
        else
            let seed = frameSeed options.Seed options.FixedNoise frame
            Sampling.setRandomSeed seed
            let renderOptions =
                { RenderOptions.Default with
                    Seed = seed
                    Integrator = effective.Integrator
                    Denoise = effective.Denoise
                    Transfer = effective.Transfer
                    Acceleration = Some Acceleration.Acceleration.FlatBVH }
            let frameTimer = Stopwatch.StartNew()
            // Denoising has already happened inside the renderer; bloom and grade work on the clean linear buffer.
            let film = (Frame.render scene settings renderOptions frame).Post effective.Post
            use image = film.ToImage(renderOptions.Transfer, renderOptions.Exposure)
            // Write then rename, so an interrupted run never leaves a truncated frame behind.
            let temporary = path + ".tmp"
            image.SavePng temporary
            File.Move(temporary, path, true)
            rendered <- rendered + 1
            let remaining = last - 1 - frame
            let perFrame = timer.Elapsed.TotalSeconds / float rendered
            printfn "  frame %5d  %6.2fs (build %.0f ms)  ETA %s"
                frame frameTimer.Elapsed.TotalSeconds film.BuildMilliseconds
                (TimeSpan.FromSeconds(perFrame * float remaining).ToString(@"hh\:mm\:ss"))
    printfn "Rendered %d frame(s) in %s." rendered (timer.Elapsed.ToString(@"hh\:mm\:ss"))
    let audio = if options.Audio then soundtrack options scene directory (float options.Start / settings.Fps) (float total / settings.Fps) else None
    if options.Video then
        let video = Path.Combine(directory, scene.Name + ".mp4")
        if Encode.toMp4 directory settings.Fps options.Start audio video then
            printfn "Video: %s" video
            match telegram with
            | Some target ->
                let caption =
                    $"{scene.Name}: frames {options.Start}-{last - 1} at {settings.Fps:g}fps, {settings.Width}x{settings.Height}, {settings.SamplesPerPixel}spp, rendered in {timer.Elapsed:``hh\:mm\:ss``}"
                match Telegram.sendVideo target video caption with
                | Ok () -> printfn "Sent to Telegram chat %s." target.ChatId
                | Error message ->
                    eprintfn "%s" message
                    delivered <- false
            | None -> ()
        elif telegram.IsSome then
            eprintfn "Nothing was sent to Telegram because the video could not be encoded."
            delivered <- false
    delivered

[<EntryPoint>]
let main argv =
    try
        if argv.Length = 0 || argv = [| "--help" |] || argv = [| "-h" |] then
            usage ()
            0
        elif argv = [| "--list" |] then
            for demo in Catalog.all do printfn "%-16s %s" demo.Name demo.Description
            0
        else
            let options = parse argv
            let scene =
                match options.Demo, options.Scene with
                | Some name, None ->
                    match Catalog.tryFind name with
                    | Some demo -> demo.Build ()
                    | None -> invalidArg "demo" $"Unknown demo {name}; see --list."
                | None, Some path ->
                    let defaults = Gltf.ImportOptions.Default
                    let imported =
                        Gltf.load path
                            { defaults with
                                Clip = options.Clip
                                PointLightScale = defaults.PointLightScale * options.LightScale
                                DirectionalLightScale = defaults.DirectionalLightScale * options.LightScale }
                    for warning in imported.Warnings do eprintfn "warning: %s" warning
                    imported.Scene
                | _ -> invalidArg "arguments" "Give exactly one of --demo or --scene."
            let scene = AnimatedScene.validate scene
            match options.ExportGltf with
            | Some path ->
                let warnings = Gltf.save scene path 60.
                for warning in warnings do eprintfn "warning: %s" warning
                printfn "Wrote %s" (Path.GetFullPath path)
            | None when options.AudioOnly ->
                let directory = Path.GetFullPath(defaultArg options.Output (Path.Combine("artifacts", "anim", scene.Name)))
                Directory.CreateDirectory directory |> ignore
                if (soundtrack options scene directory 0. scene.Duration).IsNone then invalidArg "audio-only" $"{scene.Name} has no soundtrack."
            | None -> if not (render options scene) then exit 3
            0
    with
    | :? ArgumentException as error ->
        eprintfn "%s" error.Message
        2
    | error ->
        eprintfn "%s" (error.ToString())
        1
