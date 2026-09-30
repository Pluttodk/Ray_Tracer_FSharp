module GltfTests

open System
open System.IO
open Assert
open Tracer.Basics
open Tracer.Basics.Transformation
open Tracer.Animation

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance
let private values (m: QuickMatrix) =
    [| m.Pos1x1; m.Pos1x2; m.Pos1x3; m.Pos1x4; m.Pos2x1; m.Pos2x2; m.Pos2x3; m.Pos2x4; m.Pos3x1; m.Pos3x2; m.Pos3x3; m.Pos3x4 |]
let private fixture = Path.Combine(AppContext.BaseDirectory, "fixtures", "anim_fixture.gltf")

/// A hand-written file (not produced by our exporter) pins down the spec semantics independently.
let fixtureTests () =
    let result = Gltf.load fixture Gltf.ImportOptions.Default
    let scene = result.Scene
    Assert.True(AnimatedScene.nodes scene |> Seq.map (fun n -> n.Name) |> Set.ofSeq = set [ "parent"; "child"; "cam"; "lamp" ], "fixture-nodes")
    Assert.True(scene.ActiveCamera = "cam" && near 1e-6 scene.Duration 2., "fixture-camera-and-duration")
    let world t = AnimatedScene.worldMatrices scene t
    // CUBICSPLINE with in/value/out ordering: at t = 0.5 (s = 0.25, td = 2) the out-tangent of key 0 is 3 and
    // the in-tangent of key 1 is -5; swapping them would give a different value.
    let s = 0.25
    let h00, h10, h01, h11 = 2.*s*s*s - 3.*s*s + 1., s*s*s - 2.*s*s + s, -2.*s*s*s + 3.*s*s, s*s*s - s*s
    let expected = h00 * 1. + 2. * h10 * 3. + h01 * 4. + 2. * h11 * -5.
    Assert.True(near 1e-6 (world 0.5).["parent"].Pos1x4 expected, "fixture-cubic-tangent-order")
    // The child inherits the parent's translation on top of its own rest offset.
    Assert.True(near 1e-6 (world 2.).["child"].Pos1x4 5., "fixture-hierarchy")
    // STEP holds the first rotation until the next key.
    Assert.True(near 1e-6 (world 0.99).["child"].Pos1x1 1. && near 1e-6 (world 1.).["child"].Pos1x1 0., "fixture-step-rotation")
    let camera = AnimatedScene.nodes scene |> Seq.find (fun n -> n.Name = "cam")
    Assert.True(camera.Content |> List.exists (function CameraRig spec -> near 1e-6 spec.YFov 0.5 | _ -> false), "fixture-camera-fov")
    let frame = Frame.sceneAt scene 0. 0. 2
    let light = frame.Lights |> List.tryPick (function :? PointLight as l -> Some l | _ -> None)
    Assert.True(light.IsSome && near 1e-9 light.Value.Position.Y 5. && near 1e-9 light.Value.Intensity (50. * Gltf.ImportOptions.Default.PointLightScale), "fixture-punctual-light")
    let mesh = frame.Shapes |> List.exists (fun shape -> (shape.hitFunction (Ray(Point(2.5, 0.5, 5.), Vector(0., 0., -1.)))).DidHit)
    Assert.True(mesh, "fixture-mesh-renders")
    Assert.True(result.Warnings.IsEmpty, "fixture-imports-without-warnings")

let roundTripTests () =
    let directory = Path.Combine(Path.GetTempPath(), "raytracer-gltf-tests-" + string Environment.ProcessId)
    Directory.CreateDirectory directory |> ignore
    try
        for demo in Demos.all do
            let original = demo.Build () |> AnimatedScene.validate
            let path = Path.Combine(directory, demo.Name + ".glb")
            Gltf.save original path 60. |> ignore
            let imported = (Gltf.load path Gltf.ImportOptions.Default).Scene
            let names = AnimatedScene.nodes original |> Seq.map (fun n -> n.Name) |> Seq.filter ((<>) original.ActiveCamera) |> List.ofSeq
            let times = [ 0.; 0.3; 0.5; 0.8; 0.99 ] |> List.map ((*) original.Duration)
            let mutable worst = 0.
            for t in times do
                let a, b = AnimatedScene.worldMatrices original t, AnimatedScene.worldMatrices imported t
                for name in names do
                    let d = Array.map2 (fun (x: float) y -> abs (x - y) / (1. + abs x)) (values a.[name]) (values b.[name]) |> Array.max
                    worst <- max worst d
            // float32 storage, and eased tracks resampled at 60 Hz.
            Assert.True(worst < 2e-3, $"round-trip-world-matrices-{demo.Name} (worst {worst:g3})")
            let aims =
                times |> List.forall (fun t ->
                    let p, q = AnimatedScene.cameraPose original t, AnimatedScene.cameraPose imported t
                    let da, db = (p.LookAt - p.Position).Normalise, (q.LookAt - q.Position).Normalise
                    (p.Position - q.Position).Magnitude < 2e-3 && (da - db).Magnitude < 2e-3)
            Assert.True(aims, $"round-trip-camera-{demo.Name}")
    finally
        try Directory.Delete(directory, true) with _ -> ()

let smoothTests () =
    let keys = [ 0., Vector(0., 0., 0.); 1., Vector(1., 2., 0.); 2., Vector(3., 2., 0.); 3., Vector(4., 0., 0.) ]
    let s = Smooth.vector keys
    Assert.True(keys |> List.forall (fun (t, v) -> (Sampler.evaluateVector s t - v).Magnitude < 1e-12), "smooth-passes-through-keys")
    // Auto-clamped: the plateau at y = 2 between t = 1 and 2 is not overshot.
    let peak = Seq.init 301 (fun i -> (Sampler.evaluateVector s (float i / 100.)).Y) |> Seq.max
    Assert.True(peak <= 2. + 1e-12, "smooth-no-overshoot")
    let monotoneX = Seq.init 301 (fun i -> (Sampler.evaluateVector s (float i / 100.)).X) |> Seq.pairwise |> Seq.forall (fun (a, b) -> b >= a - 1e-12)
    Assert.True(monotoneX, "smooth-monotone-where-keys-are")
    let r = Smooth.rotation [ 0., Quaternion.identity; 1., Quaternion.ofAxisAngle (Vector(0., 1., 0.)) 1.; 2., Quaternion.negate (Quaternion.ofAxisAngle (Vector(0., 1., 0.)) 2.) ]
    let mid = Sampler.evaluateRotation r 1.5
    let angle = 2. * acos (min 1. (abs mid.W))
    Assert.True(angle > 1. && angle < 2., "smooth-rotation-short-arc")
    let shifted = Clip.shift 2. (Clip.create "c" [ Clip.translate "n" (Sampler.linear [ 0., Vector.Zero; 1., Vector(1., 0., 0.) ]) ])
    match shifted.Channels.[0].Track with
    | Translation sampler -> Assert.True(sampler.Times = [| 2.; 3. |], "clip-shift")
    | _ -> Assert.Fail "clip-shift"

let allTest () =
    fixtureTests ()
    roundTripTests ()
    smoothTests ()
