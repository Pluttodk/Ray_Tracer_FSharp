module DepthOfFieldTests

open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Animation

let private up = Vector(0., 1., 0.)
let private still t = t, Point(0., 0., 5.), Point.Zero, up
let private slide = [| 0., Point(0., 0., 5.), Point.Zero, up; 1., Point(4., 0., 5.), Point(4., 0., 0.), up |]

let allTest () =
    // Zero aperture reproduces the moving pinhole rays exactly (same view sampler, same shutter times).
    let view = multiJittered 2 83
    let pinhole = MovingPinholeCamera(slide, 1., 2., 2., 4, 4, view, 0., 1.) :> Camera
    let lens0 = MovingThinLensCamera(slide, 1., 2., 2., 4, 4, 0., 7., view, multiJittered 2 89, 0., 1.) :> Camera
    let same =
        [ for x in 0 .. 3 do for y in 0 .. 3 do for s in 0 .. 3 -> x, y, s ]
        |> List.forall (fun (x, y, s) ->
            let a, b = pinhole.CreateRay(x, y, s), lens0.CreateRay(x, y, s)
            (a.GetDirection - b.GetDirection).Magnitude < 1e-12 && (a.GetOrigin - b.GetOrigin).Magnitude < 1e-12 && a.ShutterTime = b.ShutterTime)
    Assert.True(same, "dof-zero-aperture-matches-moving-pinhole")

    // With a still camera and one fixed pixel position, every lens ray of a pixel crosses the focus point.
    let focus = 6.
    let lens = MovingThinLensCamera([| still 0.; still 1. |], 1., 2., 2., 4, 4, 0.5, focus, regular 1, multiJittered 4 89, 0., 1.) :> Camera
    let rays = lens.CreateRays 1 2
    let w = Vector(0., 0., 1.)
    let hits =
        rays |> Array.map (fun r ->
            let t = focus / (-(r.GetDirection * w))
            r.GetOrigin + t * r.GetDirection)
    let converge = hits |> Array.forall (fun p -> (p - hits.[0]).Magnitude < 1e-9)
    let spread = rays |> Array.exists (fun r -> (r.GetOrigin - Point(0., 0., 5.)).Magnitude > 0.05)
    Assert.True(converge && spread && rays.Length = 16, "dof-rays-converge-at-focus")

    // Across the shutter the lens centre follows the pose: every origin is within the aperture of the interpolated position.
    let moving = MovingThinLensCamera(slide, 1., 2., 2., 4, 4, 0.3, 5., multiJittered 2 83, multiJittered 2 89, 0., 1.) :> Camera
    let follows =
        [ 0 .. 3 ] |> List.forall (fun s ->
            let r = moving.CreateRay(1, 1, s)
            let centre = Point(4. * r.ShutterTime, 0., 5.)
            (r.GetOrigin - centre).Magnitude <= 0.3 + 1e-9)
    let times = [ 0 .. 3 ] |> List.map (fun s -> moving.CreateRay(1, 1, s).ShutterTime)
    Assert.True(follows && (List.max times - List.min times) > 0.2, "dof-moving-lens-interpolates-position")

    // Autofocus: the focus distance is the camera-to-target distance; Default stays a pinhole.
    let cam = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig { CameraSpec.Default with Target = Some "t"; ApertureRadius = 0.1; FocusTarget = Some "t" } ]
    let target = Node.create "t" |> Node.at 3. 0. 6.
    let scene =
        { Name = "t"; Roots = [ cam; target ]; Clips = []; ActiveCamera = "camera"; Cuts = []; StaticShapes = []; StaticLights = []
          Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Atmosphere = None; Duration = 1. }
    Assert.True(abs ((AnimatedScene.cameraPose scene 0.).Spec.FocusDistance - 5.) < 1e-9, "dof-autofocus-distance")
    Assert.True(CameraSpec.Default.ApertureRadius = 0. && CameraSpec.Default.FocusTarget.IsNone, "dof-default-is-pinhole")
