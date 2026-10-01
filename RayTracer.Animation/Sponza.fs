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

    type Shot = { Name: string; Start: float; YFov: float; Keys: (float * Vector * Vector) list }

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
          Shots =
            [ for s in (get root "shots").EnumerateArray() ->
                { Name = (get s "name").GetString()
                  Start = num s "start"
                  YFov = radians (num s "yfovDeg")
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
                [ Node.create shot.Name
                  |> Node.withContent [ CameraRig { CameraSpec.Default with YFov = shot.YFov; Target = Some (shot.Name + "-aim") } ]
                  Node.create (shot.Name + "-aim") ])
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
        // Lamps wait for an attenuated point light (workstream R2); PointLight has no falloff.
        ignore lamps
        [ sun; Sky.light sky 2 512 :> Light ]

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
          Atmosphere = None
          Duration = spec.Duration }
