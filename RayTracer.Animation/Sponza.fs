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
                lamps |> List.map (fun p -> SphereLight(spec.LampColour, spec.LampIntensity, p, spec.LampRadius) :> Light)
            else []
        sun :: skyLight :: lampLights
        // --- end r2 ---

    // ------------------------------------------------------------------ scene

    let build () =
        let spec = loadSpec ()
        let parts = spec.Include |> List.map (loadPart spec)
        let geometry = parts |> List.collect fst
        let lamps = parts |> List.collect snd
        let cameraNodes, cameraClip = cameras spec
        let shots = spec.Shots |> List.sortBy (fun s -> s.Start)
        { Name = "sponza"
          Roots = geometry @ cameraNodes
          Clips = [ cameraClip ]
          ActiveCamera = shots.Head.Name
          Cuts = [ for s in shots.Tail -> s.Start, s.Name ]
          StaticShapes = []
          StaticLights = lights spec lamps
          Ambient = AmbientLight(Colour.White, 0.)
          MaxBounces = 4
          Atmosphere = None
          Duration = spec.Duration }
