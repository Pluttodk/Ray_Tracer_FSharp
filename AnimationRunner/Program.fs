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
      Frames: int option
      Seed: int
      FixedNoise: bool
      Integrator: IntegratorKind
      Denoise: bool
      Transfer: string
      Output: string option
      Resume: bool
      Video: bool
      ExportGltf: string option
      LightScale: float }

let private defaults =
    { Demo = None; Scene = None; Clip = None; Settings = FrameSettings.Default; Start = 0; End = None; Frames = None
      Seed = 2026; FixedNoise = false; Integrator = Classic; Denoise = false; Transfer = "srgb"
      Output = None; Resume = true; Video = true; ExportGltf = None; LightScale = 1. }

let usage () =
    printfn """Usage: AnimationRunner (--demo NAME | --scene FILE.gltf|.glb [--clip NAME] | --list) [options]
  --fps N              frames per second (default 24)
  --frames N           render N frames from --start (default: the whole animation)
  --start S / --end E  frame range [S, E)
  --res WxH            resolution (default 640x360)
  --spp N              samples per pixel (default 16)
  --integrator K       classic | path (default classic)
  --shutter F          shutter as a fraction of the frame, 0 disables motion blur (default 0.5)
  --motion-steps N     poses sampled across the shutter (default 3)
  --seed N             base seed (default 2026); each frame gets its own unless --fixed-noise
  --denoise            run Open Image Denoise (path integrator only)
  --transfer T         srgb | gamma2 | aces | linear (default srgb)
  --out DIR            output directory (default artifacts/anim/NAME)
  --no-resume          re-render frames that already exist
  --no-video           skip the ffmpeg MP4
  --light-scale F      multiplies the intensity of lights imported from glTF (default 1)
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
        | "--res" :: v :: rest ->
            match v.ToLowerInvariant().Split('x') with
            | [| w; h |] -> go { options with Settings = { options.Settings with Width = integer "res" w; Height = integer "res" h } } rest
            | _ -> invalidArg "res" "--res expects WIDTHxHEIGHT."
        | "--spp" :: v :: rest -> go { options with Settings = { options.Settings with SamplesPerPixel = integer "spp" v } } rest
        | "--integrator" :: v :: rest ->
            let kind =
                match v.ToLowerInvariant() with
                | "classic" -> Classic
                | "path" -> Path
                | _ -> invalidArg "integrator" "--integrator expects classic or path."
            go { options with Integrator = kind } rest
        | "--shutter" :: v :: rest -> go { options with Settings = { options.Settings with Shutter = number "shutter" v } } rest
        | "--motion-steps" :: v :: rest -> go { options with Settings = { options.Settings with MotionSteps = integer "motion-steps" v } } rest
        | "--seed" :: v :: rest -> go { options with Seed = integer "seed" v } rest
        | "--fixed-noise" :: rest -> go { options with FixedNoise = true } rest
        | "--denoise" :: rest -> go { options with Denoise = true } rest
        | "--transfer" :: v :: rest -> go { options with Transfer = v } rest
        | "--out" :: v :: rest -> go { options with Output = Some v } rest
        | "--no-resume" :: rest -> go { options with Resume = false } rest
        | "--no-video" :: rest -> go { options with Video = false } rest
        | "--export-gltf" :: v :: rest -> go { options with ExportGltf = Some v } rest
        | "--light-scale" :: v :: rest -> go { options with LightScale = number "light-scale" v } rest
        | option :: _ -> invalidArg "arguments" $"Unknown or incomplete option {option}."
    let options = go defaults (List.ofArray argv)
    let s = options.Settings
    if s.Fps <= 0. then invalidArg "fps" "--fps must be positive."
    if s.Width <= 0 || s.Height <= 0 then invalidArg "res" "--res must be positive."
    if s.SamplesPerPixel <= 0 then invalidArg "spp" "--spp must be positive."
    if s.Shutter < 0. || s.Shutter > 1. then invalidArg "shutter" "--shutter must lie in [0, 1]."
    if s.MotionSteps < 2 then invalidArg "motion-steps" "--motion-steps must be at least 2."
    if options.Start < 0 then invalidArg "start" "--start must be non-negative."
    match options.End, options.Frames with
    | Some _, Some _ -> invalidArg "frames" "Give --frames or --end, not both."
    | None, Some n -> { options with End = Some (options.Start + n) }
    | _ -> options

/// Everything that affects the pixels of a frame; a resumed run must match it exactly.
let private manifest (options: Options) (sceneName: string) =
    let s = options.Settings
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
        "integrator", string options.Integrator
        "denoise", string options.Denoise
        "transfer", options.Transfer
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

let render (options: Options) (scene: AnimatedScene) =
    let settings = options.Settings
    let directory = Path.GetFullPath(defaultArg options.Output (Path.Combine("artifacts", "anim", scene.Name)))
    Directory.CreateDirectory directory |> ignore
    checkManifest directory (manifest options scene.Name) options.Resume
    let last = defaultArg options.End (Frame.frameCount settings scene)
    if last <= options.Start then invalidArg "end" "The frame range is empty."
    let total = last - options.Start
    printfn "%s: frames %d..%d (%d) at %gfps, %dx%d, %dspp, shutter %g -> %s"
        scene.Name options.Start (last - 1) total settings.Fps settings.Width settings.Height
        settings.SamplesPerPixel settings.Shutter directory
    let timer = Stopwatch.StartNew()
    let mutable rendered = 0
    for frame in options.Start .. last - 1 do
        let path = Path.Combine(directory, $"frame_%05d{frame}.png")
        if options.Resume && File.Exists path && FileInfo(path).Length > 0L then
            printfn "  frame %5d  exists, skipped" frame
        else
            let seed = frameSeed options.Seed options.FixedNoise frame
            Sampling.setRandomSeed seed
            let renderOptions =
                { RenderOptions.Default with
                    Seed = seed
                    Integrator = options.Integrator
                    Denoise = options.Denoise
                    Transfer = options.Transfer
                    Acceleration = Some Acceleration.Acceleration.FlatBVH }
            let frameTimer = Stopwatch.StartNew()
            let film = Frame.render scene settings renderOptions frame
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
    if options.Video then
        let video = Path.Combine(directory, scene.Name + ".mp4")
        if Encode.toMp4 directory settings.Fps options.Start video then printfn "Video: %s" video

[<EntryPoint>]
let main argv =
    try
        if argv.Length = 0 || argv = [| "--help" |] || argv = [| "-h" |] then
            usage ()
            0
        elif argv = [| "--list" |] then
            for demo in Demos.all do printfn "%-16s %s" demo.Name demo.Description
            0
        else
            let options = parse argv
            let scene =
                match options.Demo, options.Scene with
                | Some name, None ->
                    match Demos.tryFind name with
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
            | None -> render options scene
            0
    with
    | :? ArgumentException as error ->
        eprintfn "%s" error.Message
        2
    | error ->
        eprintfn "%s" (error.ToString())
        1
