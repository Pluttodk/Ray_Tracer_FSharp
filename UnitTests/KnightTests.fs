module KnightTests

open System
open System.IO
open System.Text
open Assert
open Tracer.Basics
open Tracer.Animation

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance

/// Writes a cache in convert-knight.py's layout: one triangle (one piece, one material) at x = 0, 1, 3
/// over three frames at 2 fps, with a "pelvis" and a "head" marker.
let private writeTinyCache (path: string) =
    let header =
        """{"fps": 2.0, "firstFrame": 1, "frames": 3, "points": 3,
            "materials": [{"name": "steel", "baseColour": [0.5, 0.5, 0.5], "baseColourMap": null, "metallic": 1.0,
                           "roughness": 0.3, "roughnessMap": null, "normalMap": null, "ior": 1.5}],
            "groups": [{"material": 0, "vertices": 3, "triangles": 1}],
            "pieces": {"blade": [0, 3]}, "markers": ["pelvis", "head"]}"""
    let bytes = Encoding.UTF8.GetBytes header
    let padded = Array.append bytes (Array.create ((4 - bytes.Length % 4) % 4) (byte ' '))
    use writer = new BinaryWriter(File.Create path)
    writer.Write("KNCACHE1"B)
    writer.Write(padded.Length)
    writer.Write(padded)
    for s in [ 0; 1; 2 ] do writer.Write(s)
    for uv in [ 0.f; 0.f; 1.f; 0.f; 0.f; 1.f ] do writer.Write(uv)
    for i in [ 0; 1; 2 ] do writer.Write(i)
    for x in [ 0.f; 1.f; 3.f ] do
        for (px, py) in [ (0.f, 0.f); (1.f, 0.f); (0.f, 1.f) ] do
            writer.Write(x + px); writer.Write(py); writer.Write(0.f)
    for x in [ 0.f; 1.f; 3.f ] do
        writer.Write(x); writer.Write(1.f); writer.Write(0.f)
        writer.Write(x); writer.Write(2.f); writer.Write(-1.f)

let private tinyTests () =
    let path = Path.Combine(Path.GetTempPath(), $"knight-test-{Guid.NewGuid():N}.cache")
    try
        writeTinyCache path
        let cache = Knight.load path
        Assert.True(cache.FrameCount = 3 && cache.PointCount = 3 && cache.Groups.Length = 1 && cache.Materials.Length = 1, "knight-cache-loads")
        Assert.True(near 1e-12 cache.Duration 1.0, "knight-duration-first-to-last-frame")
        let x (t: float) = float (Knight.pointsAt cache t).[0]
        Assert.True(near 1e-6 (x 0.) 0. && near 1e-6 (x 0.25) 0.5 && near 1e-6 (x 0.75) 2. && near 1e-6 (x 1.) 3., "knight-interpolates-between-frames")
        Assert.True(near 1e-6 (x -1.) 0. && near 1e-6 (x 5.) 3., "knight-clamps-outside-range")
        let head = Knight.markerAt cache "head" 0.75
        Assert.True(near 1e-6 head.X 2. && near 1e-6 head.Y 2. && near 1e-6 head.Z -1., "knight-marker-interpolates")
        let placement = Knight.placeAt 10. 0. 0. (Math.PI / 2.)
        let time = Knight.clock 0. 1.
        let root = Knight.rootAt cache placement time 0.
        Assert.True(near 1e-9 root.X 10. && near 1e-9 root.Y 0. && near 1e-9 root.Z 0., "knight-root-world")
        // Heading is pelvis-independent: chestFront/chestBack are absent here, so only check the head.
        let worldHead = Knight.headAt cache placement time 0.
        // A quarter turn about +Y takes local (0, 2, -1) to (-1, 2, 0), then the translation adds 10 to x.
        Assert.True(near 1e-9 worldHead.X 9. && near 1e-9 worldHead.Y 2. && near 1e-9 worldHead.Z 0., "knight-marker-placed")
        let textures = Knight.textures cache 64 (fun _ p -> p)
        let hit (shapes: Shape list) (time: float) (px: float) =
            shapes |> List.exists (fun s -> (s.hitFunction (Ray(Point(px, 0.2, 5.), Vector(0., 0., -1.), time))).DidHit)
        // Keyed across a shutter from scene time 0 to 0.5 (animation x from 0 to 1): rays see their own pose.
        let shapes = Knight.shapesAt cache textures [| 0.; 0.5 |] [| 0.; 0.5 |]
        Assert.True(hit shapes 0. 0.3 && not (hit shapes 0. 1.3), "knight-deforms-at-shutter-open")
        Assert.True(hit shapes 0.5 1.3 && not (hit shapes 0.5 0.3), "knight-deforms-at-shutter-close")
        let still = Knight.shapesAt cache textures [| 0.25 |] [| 0.25 |]
        Assert.True(hit still 0. 0.8 && not (hit still 0. 0.3), "knight-static-pose")
        // Through the scene graph: a node placed at x = 5 with motion steps across the shutter.
        let node = Knight.node "knight" cache textures (Knight.placeAt 5. 0. 0. 0.) time
        let camera = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
        let scene =
            { Name = "k"; Roots = [ node; camera ]; Clips = []; ActiveCamera = "camera"; Cuts = []; StaticShapes = []; StaticLights = []
              Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Atmosphere = None; Duration = 1. }
            |> AnimatedScene.validate
        let frame = (Frame.sceneAt scene 0. 0.5 3).Shapes
        Assert.True(hit frame 0. 5.3 && not (hit frame 0. 6.3) && hit frame 0.5 6.3 && not (hit frame 0.5 5.3), "knight-node-motion-blurs")
    finally
        if File.Exists path then File.Delete path

/// The converted asset, when present (it is downloaded and baked by scripts/fetch-sponza-assets.sh).
let private assetTests () =
    let rec find (dir: DirectoryInfo) =
        if isNull dir then None
        else
            let candidate = Path.Combine(dir.FullName, "assets", "sponza", "knight", "knight.cache")
            if File.Exists candidate then Some candidate else find dir.Parent
    match find (DirectoryInfo(AppContext.BaseDirectory)) with
    | None -> printfn "  (knight cache not found; skipping the asset checks)"
    | Some path ->
        let cache = Knight.load path
        Assert.True(cache.FrameCount = 300 && near 1e-9 cache.Fps 24. && cache.Pieces.Count = 135, "knight-asset-300-frames")
        let lo, hi = Knight.boundsAt cache 0.
        Assert.True(hi.Y - lo.Y > 1.8 && hi.Y - lo.Y < 2.2 && abs lo.Y < 0.05, "knight-asset-knight-sized")
        let travelled = Knight.markerAt cache "pelvis" cache.Duration - Knight.markerAt cache "pelvis" 0.
        Assert.True(travelled.X < -2., "knight-asset-walks-towards-minus-x")

let allTest () =
    tinyTests ()
    assetTests ()
