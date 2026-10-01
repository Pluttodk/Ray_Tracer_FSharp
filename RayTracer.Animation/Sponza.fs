namespace Tracer.Animation

open System
open System.IO
open System.Text.Json
open Tracer.Basics

/// "Sponza": Intel's Sponza 2022 atrium (CC-BY 4.0) with its add-ons, lit by a low sun and sky. Shots and
/// lighting come from scenes/sponza.json, which the Cycles ground truth (scripts/sponza) reads too, so both
/// renderers see the same scene. Assets: scripts/fetch-sponza-assets.sh, see assets/sponza/SOURCES.md.
module Sponza =
    let assetDirectory = Path.Combine("assets", "sponza")

    let private searchUp (relative: string) =
        let rec search (dir: DirectoryInfo) =
            if isNull dir then None
            else
                let candidate = Path.Combine(dir.FullName, relative)
                if File.Exists candidate then Some candidate else search dir.Parent
        search (DirectoryInfo(Environment.CurrentDirectory))
        |> Option.orElse (search (DirectoryInfo(AppContext.BaseDirectory)))

    let asset (name: string) =
        searchUp (Path.Combine(assetDirectory, name)) |> Option.defaultWith (fun () ->
            invalidOp $"{Path.Combine(assetDirectory, name)} is missing; run scripts/fetch-sponza-assets.sh first.")

    // ------------------------------------------------------------------ shared description

    type Shot = { Name: string; Start: float; YFov: float; Aperture: float; Keys: (float * Vector * Vector) list }

    type Spec =
        { Duration: float
          Parts: (string * string) list
          Include: string list
          SunAzimuth: float
          SunElevation: float
          SunIrradiance: float
          SunColour: Colour
          SkyHdri: string option
          SkyRotation: float
          SkyIntensity: float
          Turbidity: float
          SunToSky: float
          LampsEnabled: bool
          LampIntensity: float
          LampColour: Colour
          LampRadius: float
          LampOffset: Vector
          Shots: Shot list
          Root: JsonElement }

    let specPath () =
        searchUp (Path.Combine("scenes", "sponza.json"))
        |> Option.defaultWith (fun () -> invalidOp "scenes/sponza.json is missing.")

    let private radians degrees = degrees * Math.PI / 180.

    let parseSpec (json: string) =
        let root = (JsonDocument.Parse json).RootElement.Clone()
        let get (e: JsonElement) (name: string) = e.GetProperty name
        let num (e: JsonElement) name = (get e name).GetDouble()
        let vec (e: JsonElement) = let a = e.EnumerateArray() |> Seq.map (fun x -> x.GetDouble()) |> Array.ofSeq in Vector(a.[0], a.[1], a.[2])
        let col (e: JsonElement) = let v = vec e in Colour(v.X, v.Y, v.Z)
        let sun, sky, lamps = get root "sun", get root "sky", get root "lamps"
        let tryString (e: JsonElement) name =
            match e.TryGetProperty(name: string) with
            | true, v when v.ValueKind = JsonValueKind.String -> Some (v.GetString())
            | _ -> None
        { Duration = num root "duration"
          Parts = [ for p in (get root "parts").EnumerateObject() -> p.Name, p.Value.GetString() ]
          Include = [ for p in (get root "include").EnumerateArray() -> p.GetString() ]
          SunAzimuth = radians (num sun "azimuthDeg")
          SunElevation = radians (num sun "elevationDeg")
          SunIrradiance = num sun "irradiance"
          SunColour = col (get sun "colour")
          SkyHdri = tryString sky "hdri"
          SkyRotation = radians (num sky "rotationDeg")
          SkyIntensity = num sky "intensity"
          Turbidity = num sky "turbidity"
          SunToSky = num sky "sunToSky"
          LampsEnabled = (get lamps "enabled").GetBoolean()
          LampIntensity = num lamps "intensity"
          LampColour = col (get lamps "colour")
          LampRadius = num lamps "radius"
          LampOffset = (match lamps.TryGetProperty "offset" with | true, v -> vec v | _ -> Vector.Zero)
          Shots =
            [ for s in (get root "shots").EnumerateArray() ->
                { Name = (get s "name").GetString()
                  Start = num s "start"
                  YFov = radians (num s "yfovDeg")
                  Aperture = (match s.TryGetProperty "aperture" with | true, v -> v.GetDouble() | _ -> 0.)
                  Keys =
                    [ for k in (get s "keys").EnumerateArray() ->
                        let a = k.EnumerateArray() |> Array.ofSeq
                        a.[0].GetDouble(), vec a.[1], vec a.[2] ] } ]
          Root = root }

    let loadSpec () = parseSpec (File.ReadAllText(specPath ()))

    /// Towards the sun, from azimuth atan2(x, z) and elevation.
    let sunDirection (spec: Spec) =
        Vector(cos spec.SunElevation * sin spec.SunAzimuth, sin spec.SunElevation, cos spec.SunElevation * cos spec.SunAzimuth)

    // ------------------------------------------------------------------ geometry

    /// Prefixes every node name, so parts from different files cannot collide.
    let rec private prefixed (prefix: string) (node: Node) =
        { node with Name = prefix + node.Name; Children = node.Children |> List.map (prefixed prefix) }

    /// Keeps a part's geometry only: the files' lights are exported at zero intensity and their cameras are
    /// replaced by the shots.
    let rec private geometryOnly (node: Node) =
        { node with
            Content = node.Content |> List.filter (function LightSource _ | CameraRig _ -> false | _ -> true)
            Children = node.Children |> List.map geometryOnly }

    /// Positions of the atrium's lamps, from the main file's (zero-intensity) lamp lights.
    let private lampPositions (imported: AnimatedScene) =
        let world = AnimatedScene.worldMatrices imported 0.
        AnimatedScene.nodes imported
        |> Seq.filter (fun n -> n.Name.StartsWith "lamp_light")
        |> Seq.map (fun n -> let m = world.[n.Name] in Point(m.Pos1x4, m.Pos2x4, m.Pos3x4))
        |> List.ofSeq

    let private loadPart (spec: Spec) (name: string) =
        let file = spec.Parts |> List.find (fun (n, _) -> n = name) |> snd
        let result = Gltf.load (asset file) Gltf.ImportOptions.Default
        let roots = result.Scene.Roots |> List.map (geometryOnly >> prefixed (name + ":"))
        roots, (if name = "main" then lampPositions result.Scene else [])

    // ------------------------------------------------------------------ cameras

    let private cameras (spec: Spec) =
        let nodes =
            spec.Shots |> List.collect (fun shot ->
                let aim = shot.Name + "-aim"
                let rig =
                    { CameraSpec.Default with
                        YFov = shot.YFov; Target = Some aim; ApertureRadius = shot.Aperture
                        FocusTarget = (if shot.Aperture > 0. then Some aim else None) }
                [ Node.create shot.Name |> Node.withContent [ CameraRig rig ]
                  Node.create aim ])
        let track (keys: (float * Vector) list) =
            match keys with
            | [ single ] -> Sampler.linear [ single; fst single + 1., snd single ]
            | _ -> Smooth.vector keys
        let channels =
            spec.Shots |> List.collect (fun shot ->
                [ Clip.translate shot.Name (track [ for t, p, _ in shot.Keys -> t, p ])
                  Clip.translate (shot.Name + "-aim") (track [ for t, _, a in shot.Keys -> t, a ]) ])
        nodes, Clip.create "cameras" channels

    // ------------------------------------------------------------------ lighting

    let skyModel (spec: Spec) =
        Sky.create
            { SunDirection = sunDirection spec; Turbidity = spec.Turbidity
              SunIrradiance = spec.SunIrradiance; SunToSky = Some spec.SunToSky }

    let private lights (spec: Spec) (lamps: Point list) =
        let sky = skyModel spec
        let sun = DirectionalLight(spec.SunColour, spec.SunIrradiance, sunDirection spec) :> Light
        // --- r2 ---
        // The HDRI's own sun is clamped out of the map (camera rays see the clamped map too) so the
        // spec's analytic sun is the only sun; without an HDRI the Preetham sky stands in.
        let skyLight =
            match spec.SkyHdri with
            | Some file ->
                let image = HdrImage.load (asset file)
                let clamped, hdriSun = HdrEnvironment.extractSun image spec.SkyRotation None 5.
                let d = hdriSun.Direction
                eprintfn "sponza: HDRI sun clamped (%d px, %.0f%% of the map's power) at azimuth %.1f, elevation %.1f deg"
                    hdriSun.Pixels (100. * hdriSun.PowerFraction) (Math.Atan2(d.X, d.Z) * 180. / Math.PI) (Math.Asin d.Y * 180. / Math.PI)
                HdrEnvironment.light clamped None spec.SkyRotation spec.SkyIntensity 2 1024 :> Light
            | None -> Sky.light sky 2 512 :> Light
        let lampLights =
            if spec.LampsEnabled then
                lamps |> List.map (fun p -> SphereLight(spec.LampColour, spec.LampIntensity, p + spec.LampOffset, spec.LampRadius) :> Light)
            else []
        sun :: skyLight :: lampLights
        // --- end r2 ---

    // --- v ---
    /// Haze filling the atrium, for sun shafts through the arches and halos around the lamps. These are the
    /// defaults; an optional "volume" object in scenes/sponza.json overrides them key by key, and
    /// "enabled": false turns the haze off. Coefficients are per metre.
    let hazeDefaults =
        { Volume.Default with
            Min = Point(-18., -0.5, -10.); Max = Point(18., 27., 10.)
            Scattering = Colour(0.006, 0.006, 0.006); Absorption = Colour(0.0006, 0.0006, 0.0006)
            BaseHeight = 0.; ScaleHeight = infinity
            SunAnisotropy = 0.6; LampAnisotropy = 0.2
            SunSamples = 4; LampSamples = 2; ScatterDepth = 0
            Ambient = Colour(0.03, 0.033, 0.039); SunWeight = 1.; LampWeight = 1.; LampClearance = 0.15 }

    let haze (spec: Spec) : Volume option =
        match spec.Root.TryGetProperty "volume" with
        | true, v when v.ValueKind = JsonValueKind.Object ->
            let has (name: string) = match v.TryGetProperty name with | true, e -> Some e | _ -> None
            let num name fallback = has name |> Option.map (fun e -> e.GetDouble()) |> Option.defaultValue fallback
            let int name fallback = has name |> Option.map (fun e -> e.GetInt32()) |> Option.defaultValue fallback
            let triple (e: JsonElement) =
                if e.ValueKind = JsonValueKind.Number then let x = e.GetDouble() in x, x, x
                else let a = e.EnumerateArray() |> Seq.map (fun x -> x.GetDouble()) |> Array.ofSeq in a.[0], a.[1], a.[2]
            let col name (fallback: Colour) =
                has name |> Option.map (fun e -> let r, g, b = triple e in Colour(r, g, b)) |> Option.defaultValue fallback
            let point name (fallback: Point) =
                has name |> Option.map (fun e -> let x, y, z = triple e in Point(x, y, z)) |> Option.defaultValue fallback
            let d = hazeDefaults
            let enabled = has "enabled" |> Option.map (fun e -> e.GetBoolean()) |> Option.defaultValue true
            if not enabled then None
            else
                Some
                    { Min = point "min" d.Min; Max = point "max" d.Max
                      Scattering = col "scattering" d.Scattering; Absorption = col "absorption" d.Absorption
                      BaseHeight = num "baseHeight" d.BaseHeight
                      ScaleHeight = (match has "scaleHeight" with
                                     | Some e when e.ValueKind = JsonValueKind.Number -> e.GetDouble()
                                     | Some _ -> infinity
                                     | None -> d.ScaleHeight)
                      SunAnisotropy = num "sunAnisotropy" d.SunAnisotropy
                      LampAnisotropy = num "lampAnisotropy" d.LampAnisotropy
                      SunSamples = int "sunSamples" d.SunSamples; LampSamples = int "lampSamples" d.LampSamples
                      ScatterDepth = int "scatterDepth" d.ScatterDepth
                      MaxDistance = num "maxDistance" d.MaxDistance
                      Ambient = col "ambient" d.Ambient
                      SunWeight = num "sunWeight" d.SunWeight; LampWeight = num "lampWeight" d.LampWeight
                      LampClearance = num "lampClearance" d.LampClearance }
        | _ -> Some hazeDefaults

    let atmosphere (spec: Spec) =
        haze spec |> Option.map (fun v -> { Atmosphere.Default with Density = 0.; Volume = Some v })
    // --- end v ---

    // --- k ---
    /// The animated knight (Knight.fs), placed from the optional "knight" block of sponza.json:
    /// { "enabled": true, "position": [x, y, z], "yawDeg": 0, "start": 0, "speed": 1, "maxTexture": 2048 }.
    /// Defaults: on the atrium floor (y = 0) at (8, 0, 0), yaw 0, starting at scene time 0. It starts in a
    /// guard stance and later walks ~2.9 m towards -X along the long axis, stopping about 2 m short of the
    /// cypress add-on's ground cover (the 15 m tree stands at the origin, with ground cover to |x|, |z| = 3.1).
    /// Skipped with a message when assets/sponza/knight/knight.cache has not been baked.
    type KnightSetup = { Enabled: bool; Position: Vector; Yaw: float; Start: float; Speed: float; MaxTexture: int }

    let knightSetup (spec: Spec) =
        let fallback = { Enabled = true; Position = Vector(8., 0., 0.); Yaw = 0.; Start = 0.; Speed = 1.; MaxTexture = 2048 }
        match spec.Root.TryGetProperty "knight" with
        | true, k ->
            let num name fallback = match k.TryGetProperty(name: string) with | true, v -> v.GetDouble() | _ -> fallback
            { Enabled = (match k.TryGetProperty "enabled" with | true, v -> v.GetBoolean() | _ -> true)
              Position =
                (match k.TryGetProperty "position" with
                 | true, v -> let a = v.EnumerateArray() |> Seq.map (fun x -> x.GetDouble()) |> Array.ofSeq in Vector(a.[0], a.[1], a.[2])
                 | _ -> fallback.Position)
              Yaw = radians (num "yawDeg" 0.)
              Start = num "start" fallback.Start
              Speed = num "speed" fallback.Speed
              MaxTexture = int (num "maxTexture" (float fallback.MaxTexture)) }
        | _ -> fallback

    /// The knight's placement and clock (scene time to animation time), for camera helpers such as
    /// `Knight.headAt cache placement clock t`.
    let knightPlacement (setup: KnightSetup) =
        Knight.placeAt setup.Position.X setup.Position.Y setup.Position.Z setup.Yaw, Knight.clock setup.Start setup.Speed

    /// The knight cache, if it has been baked (loaded once per process).
    let knightCache = lazy (searchUp (Path.Combine(assetDirectory, "knight", "knight.cache")) |> Option.map Knight.load)

    let private knightNodes (spec: Spec) =
        let setup = knightSetup spec
        match setup.Enabled, knightCache.Force() with
        | false, _ -> []
        | true, None ->
            eprintfn "sponza: knight cache missing (run scripts/fetch-sponza-assets.sh knight); rendering without the knight."
            []
        | true, Some cache ->
            let placement, clock = knightPlacement setup
            [ Knight.node "knight" cache (Knight.textures cache setup.MaxTexture (fun _ p -> p)) placement clock ]
    // --- end k ---

    // ------------------------------------------------------------------ scene

    let build () =
        let spec = loadSpec ()
        let parts = spec.Include |> List.map (loadPart spec)
        let geometry = parts |> List.collect fst
        let lamps = parts |> List.collect snd
        let cameraNodes, cameraClip = cameras spec
        let shots = spec.Shots |> List.sortBy (fun s -> s.Start)
        { Name = "sponza"
          Roots = geometry @ cameraNodes @ knightNodes spec // --- k ---
          Clips = [ cameraClip ]
          ActiveCamera = shots.Head.Name
          Cuts = [ for s in shots.Tail -> s.Start, s.Name ]
          StaticShapes = []
          StaticLights = lights spec lamps
          Ambient = AmbientLight(Colour.White, 0.)
          MaxBounces = 4
          Atmosphere = atmosphere spec
          Duration = spec.Duration }
