module PhysicsTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Transformation
open Tracer.Animation
open Tracer.Animation.Physics

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance
let private floorPlane = Plane(Vector(0., 1., 0.), 0.)
let private unitSphere = SphereShape(Point.Zero, 1., Textures.mkMatTexture (MatteMaterial(Colour.White, 0., Colour.White, 1.))) :> Shape

let freeFallTests () =
    let ball = body "ball" 0.5 (Point(0., 100., 0.))
    let run = (simulate (world [ ball ] []) 2. 10.).[0]
    let final = run.Samples.[run.Samples.Length - 1]
    // Semi-implicit Euler: position error of order g t dt.
    Assert.True(near (9.81 * 2. / 480. + 1e-9) final.Position.Y (100. - 0.5 * 9.81 * 4.), "free-fall-matches-half-g-t-squared")
    Assert.True(near 1e-9 final.Velocity.Y (-9.81 * 2.), "free-fall-velocity")

let bounceTests () =
    let height, e = 2., 0.8
    let ball = { body "ball" 0.25 (Point(0., height + 0.25, 0.)) with Restitution = e; Friction = 0. }
    let w = world [ ball ] [ floorPlane ]
    let run = (simulate w 1.8 1000.).[0]
    let firstImpact = run.Samples |> Array.findIndex (fun s -> s.ImpactSpeed > 0.)
    let apex = run.Samples.[firstImpact..] |> Array.maxBy (fun s -> s.Position.Y)
    Assert.True(near 0.03 (apex.Position.Y - 0.25) (e * e * height), "bounce-apex-is-e-squared-h")
    let energies = run.Samples |> Array.map (energy w ball)
    let increases = energies |> Array.pairwise |> Array.exists (fun (a, b) -> b > a + 1e-6 * abs a + 1e-9)
    Assert.True(not increases, "bounce-energy-never-increases")

let rollingTests () =
    let theta = 15. * Math.PI / 180.
    let slope = Quaternion.ofAxisAngle (Vector(0., 0., 1.)) -theta
    let normal = Quaternion.rotate slope (Vector(0., 1., 0.))
    let radius = 0.3
    // A long inclined plane through the origin, and a ball resting on it.
    let plane = Plane(normal, 0.)
    let start = Point.Zero + (radius + 1e-9) * normal
    let ball = { body "ball" radius start with Friction = 0.9; RollingResistance = 0. }
    let run = (simulate (world [ ball ] [ plane ]) 1.5 100.).[0]
    let final = run.Samples.[run.Samples.Length - 1]
    let downhill = Quaternion.rotate slope (Vector(1., 0., 0.))
    let speed = final.Velocity * downhill
    let expected = 5. / 7. * 9.81 * sin theta * 1.5
    Assert.True(near (0.01 * expected) speed expected, "rolling-acceleration-five-sevenths-g-sin")
    // Rolling without slipping: the contact point is momentarily at rest.
    let contactVelocity = final.Velocity + (final.AngularVelocity % (-radius * normal))
    Assert.True(contactVelocity.Magnitude < 1e-3, "rolling-contact-point-at-rest")
    // The orientation integrates the spin: the ball turns through distance / radius radians.
    let distance = (final.Position - start).Magnitude
    let turned = 2. * acos (min 1. (abs final.Orientation.W))
    Assert.True(near 0.02 turned (distance / radius % (2. * Math.PI)) || distance / radius > 2. * Math.PI, "rolling-orientation-follows-distance")
    // With rolling resistance on flat ground, the ball comes to rest.
    let flat = { body "ball" radius (Point(0., radius, 0.)) with Velocity = Vector(2., 0., 0.); AngularVelocity = Vector(0., 0., -2. / radius); RollingResistance = 0.8 }
    let stop = (simulate (world [ flat ] [ floorPlane ]) 8. 10.).[0]
    Assert.True(stop.Samples.[stop.Samples.Length - 1].Velocity.Magnitude = 0., "rolling-ball-comes-to-rest")

let collisionTests () =
    let box = OrientedBox(Point(0., 0., 0.), Vector(1., 1., 1.), Quaternion.ofAxisAngle (Vector(0., 1., 0.)) 0.5)
    match contact (Point(0., 1.2, 0.)) 0.5 box with
    | Some (n, depth) -> Assert.True(near 1e-12 n.Y 1. && near 1e-12 depth 0.3, "box-contact-top-face")
    | None -> Assert.Fail "box-contact-top-face"
    Assert.True((contact (Point(0., 2., 0.)) 0.5 box).IsNone, "box-contact-separated")
    match contact (Point(0., 0.9, 0.)) 0.5 box with
    | Some (n, depth) -> Assert.True(near 1e-12 n.Y 1. && near 1e-12 depth 0.6, "box-contact-centre-inside")
    | None -> Assert.Fail "box-contact-centre-inside"
    // Equal masses, head-on, elastic: velocities exchange and momentum is conserved.
    let a = { body "a" 0.5 (Point(-2., 10., 0.)) with Velocity = Vector(3., 0., 0.); Restitution = 1.; Friction = 0. }
    let b = { body "b" 0.5 (Point(0., 10., 0.)) with Restitution = 1.; Friction = 0. }
    let run = simulate { world [ a; b ] [] with Gravity = Vector.Zero } 1. 10.
    let va, vb = run.[0].Samples.[10].Velocity, run.[1].Samples.[10].Velocity
    Assert.True(near 1e-9 va.X 0. && near 1e-9 vb.X 3., "elastic-collision-exchanges-velocity")

let bakeTests () =
    let ball = { body "ball" 1. (Point(0., 3., 0.)) with Velocity = Vector(1., 0., 0.); AngularVelocity = Vector(0.3, 0., -1.) }
    let w = world [ ball ] [ floorPlane ]
    let nodes, clip, trajectories = Bake.simulate w (fun _ -> unitSphere) None 2. 60. "physics"
    let camera = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
    let scene =
        { Name = "bake"; Roots = nodes @ [ camera ]; Clips = [ clip ]; ActiveCamera = "camera"; StaticShapes = []
          StaticLights = []; Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Duration = 2. }
        |> AnimatedScene.validate
    let matches =
        trajectories.[0].Samples |> Array.forall (fun sample ->
            let m = (AnimatedScene.worldMatrices scene sample.Time).["ball-spin"]
            let expected = Trs.toMatrix { Translation = Vector(sample.Position.X, sample.Position.Y, sample.Position.Z); Rotation = sample.Orientation; Scale = Vector(1., 1., 1.) }
            let values (q: QuickMatrix) = [| q.Pos1x1; q.Pos1x2; q.Pos1x3; q.Pos1x4; q.Pos2x1; q.Pos2x2; q.Pos2x3; q.Pos2x4; q.Pos3x1; q.Pos3x2; q.Pos3x3; q.Pos3x4 |]
            Array.forall2 (near 1e-9) (values m) (values expected))
    Assert.True(matches, "bake-replays-simulation")

let squashTests () =
    let settings = SquashStretch.defaults
    let volume (scale: Vector) = scale.X * scale.Y * scale.Z
    Assert.True([ 0.6; 1.; 1.3 ] |> List.forall (fun s -> near 1e-12 (volume (SquashStretch.volumeScale s)) 1.), "squash-preserves-volume")
    let first, envelope = SquashStretch.impactFactor settings 0.3 0.
    Assert.True(near 1e-12 first 0.7 && near 1e-12 envelope 1., "squash-starts-at-full-amplitude")
    let late, _ = SquashStretch.impactFactor settings 0.3 2.
    Assert.True(near 1e-3 late 1., "squash-settles")
    let tilt = Vector(0.3, 1., -0.2).Normalise
    Assert.True((Quaternion.rotate (SquashStretch.alignY tilt) (Vector(0., 1., 0.)) - tilt).Magnitude < 1e-12, "align-y-to-axis")
    Assert.True((Quaternion.rotate (SquashStretch.alignY (Vector(0., -1., 0.))) (Vector(0., 1., 0.)) - Vector(0., -1., 0.)).Magnitude < 1e-12, "align-y-antiparallel")
    // Dropped ball with squash: while squashed on the floor its lowest point stays on the floor.
    let radius = 0.5
    let ball = { body "ball" radius (Point(0., 2., 0.)) with Restitution = 0.8 }
    let w = world [ ball ] [ floorPlane ]
    let nodes, clip, trajectories = Bake.simulate w (fun _ -> SphereShape(Point.Zero, radius, Textures.mkMatTexture (MatteMaterial(Colour.White, 0., Colour.White, 1.))) :> Shape) (Some settings) 1.5 240. "physics"
    let camera = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
    let scene =
        { Name = "squash"; Roots = nodes @ [ camera ]; Clips = [ clip ]; ActiveCamera = "camera"; StaticShapes = []
          StaticLights = []; Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Duration = 1.5 }
    let impact = trajectories.[0].Samples |> Array.find (fun s -> s.ImpactSpeed > 1.)
    let m = (AnimatedScene.worldMatrices scene impact.Time).["ball-spin"]
    let determinant =
        m.Pos1x1 * (m.Pos2x2 * m.Pos3x3 - m.Pos2x3 * m.Pos3x2) - m.Pos1x2 * (m.Pos2x1 * m.Pos3x3 - m.Pos2x3 * m.Pos3x1)
        + m.Pos1x3 * (m.Pos2x1 * m.Pos3x2 - m.Pos2x2 * m.Pos3x1)
    Assert.True(near 1e-9 determinant 1., "baked-squash-preserves-volume")
    let lowest = m.Pos2x4 - radius * sqrt (m.Pos2x1 * m.Pos2x1 + m.Pos2x2 * m.Pos2x2 + m.Pos2x3 * m.Pos2x3)
    Assert.True(m.Pos2x2 < 0.9 && near 0.01 lowest 0., "baked-squash-keeps-contact-on-floor")

let allTest () =
    freeFallTests ()
    bounceTests ()
    rollingTests ()
    collisionTests ()
    bakeTests ()
    squashTests ()
