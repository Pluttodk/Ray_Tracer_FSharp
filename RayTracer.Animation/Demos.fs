namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Animation.Stage

/// Built-in example animations.
module Demos =
    type Demo = { Name: string; Description: string; Build: unit -> AnimatedScene }

    let private up = Vector(0., 1., 0.)
    let private floor () = groundNode (floorChecker 1. (matte (rgb 0.8 0.8 0.8)) (matte (rgb 0.35 0.4 0.5)))
    let private daylight () = [ sun (Vector(0.4, 1., 0.6)) 0.85; sky (rgb 0.85 0.9 1.) (rgb 0.3 0.5 0.9) 0.35 2 ]

    /// Three hops across a checkered floor. The height uses cubic-spline keys whose tangents make each arc an
    /// exact parabola; the camera sits on a keyframed pivot that orbits while staying aimed at the ball.
    let hop () =
        let radius = 0.5
        let hops = 3
        let hopTime = 0.8
        let hopLength = 2.
        let height = 1.6
        let g = 8. * height / (hopTime * hopTime)
        let launch = g * hopTime / 2.
        let duration = float hops * hopTime + 0.6
        let heightKeys =
            [ for i in 0 .. hops - 1 do
                let t0 = float i * hopTime
                yield t0, Vector(0., -launch, 0.), Vector(0., radius, 0.), Vector(0., launch, 0.)
                yield t0 + hopTime / 2., Vector.Zero, Vector(0., radius + height, 0.), Vector.Zero
              yield float hops * hopTime, Vector(0., -launch, 0.), Vector(0., radius, 0.), Vector.Zero ]
        let travelKeys =
            [ 0., Vector(-hopLength * float hops / 2., 0., 0.)
              float hops * hopTime, Vector(hopLength * float hops / 2., 0., 0.) ]
        let spinKeys =
            [ 0., Quaternion.identity
              float hops * hopTime / 2., Quaternion.ofAxisAngle (Vector(0., 0., 1.)) -Math.PI
              float hops * hopTime, Quaternion.ofAxisAngle (Vector(0., 0., 1.)) (-2. * Math.PI + 1e-6) ]
        let orbitKeys =
            [ for i in 0 .. 4 ->
                let t = duration * float i / 4.
                t, Quaternion.ofAxisAngle up (-0.35 + 0.7 * float i / 4.) ]
        let ball =
            Node.create "ball"
            |> Node.withChildren
                [ Node.create "ball-lift"
                  |> Node.withChildren
                      [ Node.create "ball-spin"
                        |> Node.withContent [ Geometry(sphere radius (checker 8 4 (plastic (rgb 0.9 0.25 0.1)) (plastic (rgb 0.95 0.9 0.8)))) ] ] ]
        let pivot =
            Node.create "camera-pivot"
            |> Node.withChildren
                [ cameraNode "camera" (Point(0., 2.4, 9.)) { CameraSpec.Default with YFov = 0.6; Target = Some "ball" } ]
        { Name = "hop"
          Roots = [ ball; pivot; floor () ]
          Clips =
            [ Clip.create "hop"
                [ Clip.translate "ball" (Sampler.linear travelKeys |> Sampler.withEase Easing.linear)
                  Clip.translate "ball-lift" (Sampler.cubic heightKeys)
                  Clip.rotate "ball-spin" (Sampler.linear spinKeys)
                  Clip.rotate "camera-pivot" (Sampler.linear orbitKeys |> Sampler.withEase Easing.smoothstep) ] ]
          ActiveCamera = "camera"
          Cuts = []
          StaticShapes = []
          StaticLights = daylight ()
          Ambient = ambient 0.
          MaxBounces = 3; Atmosphere = None
          Duration = duration }

    let private ballTexture (colour: Colour) = checker 8 4 (plastic colour) (plastic (rgb 0.95 0.92 0.85))
    let private physicsGeometry (body: Physics.Body) =
        let colour =
            match body.Name with
            | "ball" -> rgb 0.9 0.25 0.1
            | _ -> rgb 0.15 0.45 0.9
        sphere body.Radius (ballTexture colour)

    /// A ball released at the top of a ramp rolls down under gravity and friction, runs across the floor and
    /// knocks a resting ball. Everything comes from the physics simulation; the checker shows the rolling.
    let rollingBall () =
        let theta = 18. * Math.PI / 180.
        let half = Vector(3.2, 0.3, 1.2)
        let rotation = Quaternion.ofAxisAngle (Vector(0., 0., 1.)) -theta
        let centre =
            let cy = half.X * sin theta - half.Y * cos theta
            let cx = -1.5 - half.X * cos theta - half.Y * sin theta
            Point(cx, cy, 0.)
        let radius = 0.4
        let start =
            let onTop = Quaternion.rotate rotation (Vector(-half.X + 0.7, half.Y, 0.))
            let normal = Quaternion.rotate rotation (Vector(0., 1., 0.))
            centre + onTop + (radius + 1e-3) * normal
        let bodies =
            [ { Physics.body "ball" radius start with Restitution = 0.5; Friction = 0.8; RollingResistance = 0.1 }
              { Physics.body "target" radius (Point(4., radius, 0.)) with Restitution = 0.5; Friction = 0.8; RollingResistance = 0.25 } ]
        let colliders = [ Physics.Plane(Vector(0., 1., 0.), 0.); Physics.OrientedBox(centre, half, rotation) ]
        let duration = 6.
        let nodes, clip, _ =
            Bake.simulate (Physics.world bodies colliders) physicsGeometry None duration 120. "physics"
        let focus = Node.create "focus" |> Node.at 0.5 0.6 0.
        let camera =
            cameraNode "camera" (Point(0.5, 3., 11.)) { CameraSpec.Default with YFov = 0.72; Target = Some "focus" }
        { Name = "rolling-ball"
          Roots = nodes @ [ focus; camera; floor (); colliderNode "ramp" centre half rotation (solid (plastic (rgb 0.75 0.55 0.3))) ]
          Clips = [ clip; Clip.create "camera" [ Clip.translate "focus" (Sampler.linear [ 0., Vector(-2.5, 1., 0.); 3., Vector(1.5, 0.6, 0.); duration, Vector(3.5, 0.5, 0.) ] |> Sampler.withEase Easing.smoothstep) ] ]
          ActiveCamera = "camera"
          Cuts = []
          StaticShapes = []
          StaticLights = daylight ()
          Ambient = ambient 0.
          MaxBounces = 3; Atmosphere = None
          Duration = duration }

    /// A ball thrown onto the floor bounces with cartoon squash on impact and stretch in flight.
    let bouncingBall () =
        let radius = 0.45
        let ball =
            { Physics.body "ball" radius (Point(-3.5, 3.2, 0.)) with
                Velocity = Vector(1.6, 0., 0.); AngularVelocity = Vector(0., 0., -2.); Restitution = 0.78; Friction = 0.5 }
        let duration = 4.5
        let nodes, clip, _ =
            Bake.simulate (Physics.world [ ball ] [ Physics.Plane(Vector(0., 1., 0.), 0.) ]) physicsGeometry
                (Some SquashStretch.defaults) duration 240. "physics"
        let focus = Node.create "focus" |> Node.at 0. 1.2 0.
        let camera = cameraNode "camera" (Point(0., 1.8, 10.)) { CameraSpec.Default with YFov = 0.7; Target = Some "focus" }
        { Name = "bouncing-ball"
          Roots = nodes @ [ focus; camera; floor () ]
          Clips = [ clip ]
          ActiveCamera = "camera"
          Cuts = []
          StaticShapes = []
          StaticLights = daylight ()
          Ambient = ambient 0.
          MaxBounces = 3; Atmosphere = None
          Duration = duration }

    /// Finds a repository asset by walking up from the working directory and the executable.
    let private findAsset (relative: string) =
        let rec search (dir: IO.DirectoryInfo) =
            if isNull dir then None
            else
                let candidate = IO.Path.Combine(dir.FullName, relative)
                if IO.File.Exists candidate then Some candidate else search dir.Parent
        search (IO.DirectoryInfo(Environment.CurrentDirectory))
        |> Option.orElse (search (IO.DirectoryInfo(AppContext.BaseDirectory)))

    /// A still life (the Stanford bunny on a pedestal among glossy spheres) filmed with a crane-and-dolly move:
    /// the camera travels on smooth cubic-spline keys while staying aimed at the subject.
    let cameraDolly () =
        let pedestalTop = 0.6
        let subject =
            match findAsset (IO.Path.Combine("ply", "bunny10k.ply")) with
            | Some path ->
                let shape = (TriangleMesh.drawTriangles path true).toShape (solid (glossy (rgb 0.85 0.8 0.7)))
                let bounds = shape.getBoundingBox ()
                let size = bounds.highPoint.Y - bounds.lowPoint.Y
                let s = 1.6 / size
                let centreX = 0.5 * (bounds.lowPoint.X + bounds.highPoint.X)
                let centreZ = 0.5 * (bounds.lowPoint.Z + bounds.highPoint.Z)
                Node.create "subject"
                |> Node.withRest
                    { Translation = Vector(-s * centreX, pedestalTop - s * bounds.lowPoint.Y, -s * centreZ)
                      Rotation = Quaternion.identity; Scale = Vector(s, s, s) }
                |> Node.withContent [ Geometry shape ]
            | None ->
                Node.create "subject" |> Node.at 0. (pedestalTop + 0.7) 0. |> Node.withContent [ Geometry(sphere 0.7 (solid (glossy (rgb 0.85 0.8 0.7)))) ]
        let pedestal =
            Node.create "pedestal"
            |> Node.withContent [ Geometry(SolidCylinder(Point(0., pedestalTop / 2., 0.), 1., pedestalTop, solid (plastic (rgb 0.9 0.9 0.92)), solid (plastic (rgb 0.9 0.9 0.92)), solid (plastic (rgb 0.9 0.9 0.92)))) ]
        let orb name x z r (c: Colour) = Node.create name |> Node.at x r z |> Node.withContent [ Geometry(sphere r (solid (glossy c))) ]
        let focus = Node.create "focus" |> Node.at 0. 1.3 0.
        let path =
            [ 0., Vector(-6.5, 0.6, 5.5)
              2., Vector(-2.5, 1.2, 6.)
              4., Vector(2.5, 2.8, 5.)
              6., Vector(5.5, 1.8, 1.)
              8., Vector(4., 1., -3.5) ]
        let duration = 8.
        { Name = "camera-dolly"
          Roots =
            [ floor (); pedestal; subject; focus
              orb "orb-red" 2.2 1.2 0.45 (rgb 0.8 0.15 0.1)
              orb "orb-blue" -1.8 1.8 0.6 (rgb 0.15 0.3 0.8)
              orb "orb-gold" 1.2 -2.2 0.7 (rgb 0.85 0.65 0.2)
              cameraNode "camera" (Point(-6.5, 0.6, 5.5)) { CameraSpec.Default with YFov = 0.62; Target = Some "focus" } ]
          Clips =
            [ Clip.create "dolly"
                [ Clip.translate "camera" (Smooth.vector path)
                  Clip.translate "focus" (Smooth.vector [ 0., Vector(0., 1.1, 0.); 4., Vector(0., 1.5, 0.); duration, Vector(0.5, 1.2, 0.) ]) ] ]
          ActiveCamera = "camera"
          Cuts = []
          StaticShapes = []
          StaticLights = daylight ()
          Ambient = ambient 0.
          MaxBounces = 4; Atmosphere = None
          Duration = duration }

    /// A desk lamp in the spirit of Pixar's Luxo Jr.: an articulated hierarchy (base, two arms, head with its own
    /// light) keyframed with anticipation, a hop, a squash on landing and follow-through, then it nudges a ball,
    /// which the physics simulation takes over from.
    let lamp () =
        let degrees d = d * Math.PI / 180.
        // Lean is measured towards +X, i.e. a negative rotation about +Z.
        let lean d = Quaternion.ofAxisAngle (Vector(0., 0., 1.)) (-(degrees d))
        let metal = solid (plastic (rgb 0.92 0.92 0.95))
        let dark = solid (plastic (rgb 0.2 0.2 0.22))
        let armLength = 0.9
        let arm = box (Point(-0.04, 0., -0.04)) (Point(0.04, armLength, 0.04)) metal
        let joint = sphere 0.07 dark
        let bulb = EmissiveMaterial(Colour(1., 0.95, 0.8), 1.6) :> Tracer.Basics.Material
        let head =
            Node.create "head"
            |> Node.at 0. armLength 0.
            |> Node.withContent [ Geometry joint ]
            |> Node.withChildren
                [ Node.create "shade"
                  |> Node.withRest { Translation = Vector(0.18, 0., 0.); Rotation = Quaternion.ofAxisAngle (Vector(0., 0., 1.)) (degrees 90.); Scale = Vector(1., 1., 1.) }
                  |> Node.withContent
                      [ Geometry(HollowCylinder(Point(0., 0., 0.), 0.26, 0.4, metal))
                        Geometry(SolidCylinder(Point(0., 0.19, 0.), 0.1, 0.06, dark, dark, dark)) ]
                  |> Node.withChildren
                      [ Node.create "bulb" |> Node.at 0. 0.08 0. |> Node.withContent [ Geometry(sphere 0.1 (solid bulb)) ]
                        Node.create "lamp-light" |> Node.at 0. -0.35 0. |> Node.withContent [ LightSource(lamp Point.Zero 0.45) ] ] ]
        let upper = Node.create "upper-arm" |> Node.at 0. armLength 0. |> Node.withContent [ Geometry arm; Geometry joint ] |> Node.withChildren [ head ]
        let lower = Node.create "lower-arm" |> Node.at 0. 0.1 0. |> Node.withContent [ Geometry arm; Geometry joint ] |> Node.withChildren [ upper ]
        let luxo =
            Node.create "luxo"
            |> Node.withChildren
                [ Node.create "base"
                  |> Node.withContent [ Geometry(SolidCylinder(Point(0., 0.05, 0.), 0.45, 0.1, metal, metal, metal)) ]
                  |> Node.withChildren [ lower ] ]
        // Timing (seconds): look, anticipate, hop, land, settle, then nudge the ball and watch it go.
        let rest = (-15., 95., -70.)
        let poses =
            [ 0.0, rest
              0.6, (-12., 90., -40.)      // looks at the ball
              1.2, (10., 125., -55.)      // anticipation: crouch
              1.55, (-25., 60., -45.)     // take-off: stretch out
              1.9, (8., 130., -60.)       // landing: absorb
              2.25, (-18., 92., -68.)     // follow-through
              2.6, (-15., 95., -70.)
              3.1, (5., 105., -95.)       // leans in
              3.35, (12., 100., -110.)    // nudge
              3.8, (-10., 90., -60.)      // pulls back, watching
              5.5, (-14., 88., -40.) ]
        let track pick = Smooth.rotation [ for t, p in poses -> t, lean (pick p) ]
        let hop =
            Smooth.vector
                [ 0.0, Vector.Zero; 1.2, Vector.Zero; 1.55, Vector(0.45, 0.55, 0.); 1.9, Vector(1.1, 0., 0.); 5.5, Vector(1.1, 0., 0.) ]
        let squash =
            Smooth.vector
                [ 0.0, Vector(1., 1., 1.); 1.1, Vector(1., 1., 1.); 1.2, Vector(1.08, 0.86, 1.08); 1.45, Vector(0.95, 1.1, 0.95)
                  1.9, Vector(1.12, 0.82, 1.12); 2.15, Vector(0.97, 1.05, 0.97); 2.4, Vector(1., 1., 1.); 5.5, Vector(1., 1., 1.) ]
        let nudge = 3.35
        let radius = 0.3
        let ball =
            { Physics.body "ball" radius (Point(2.75, radius, 0.)) with
                Velocity = Vector(1.6, 0., 0.); AngularVelocity = Vector(0., 0., -1.6 / radius); Restitution = 0.5; RollingResistance = 0.35 }
        let duration = 5.5
        let ballNodes, ballClip, _ =
            Bake.simulate (Physics.world [ ball ] [ Physics.Plane(Vector(0., 1., 0.), 0.) ])
                (fun b -> sphere b.Radius (ballTexture (rgb 0.95 0.75 0.1))) None (duration - nudge) 120. "ball"
        let focus = Node.create "focus" |> Node.at 1.6 0.8 0.
        { Name = "lamp"
          Roots = [ floor (); luxo; focus; cameraNode "camera" (Point(1.6, 1.3, 4.3)) { CameraSpec.Default with YFov = 0.6; Target = Some "focus" } ] @ ballNodes
          Clips =
            [ Clip.create "performance"
                [ Clip.translate "luxo" hop
                  Clip.scale "luxo" squash
                  Clip.rotate "lower-arm" (track (fun (a, _, _) -> a))
                  Clip.rotate "upper-arm" (track (fun (_, b, _) -> b))
                  Clip.rotate "head" (track (fun (_, _, c) -> c))
                  Clip.translate "focus" (Smooth.vector [ 0., Vector(1.2, 0.8, 0.); 2.5, Vector(1.6, 0.8, 0.); duration, Vector(2.6, 0.6, 0.) ]) ]
              Clip.shift nudge ballClip ]
          ActiveCamera = "camera"
          Cuts = []
          StaticShapes = []
          StaticLights = [ sun (Vector(-0.3, 1., 0.8)) 0.55; sky (rgb 0.8 0.85 1.) (rgb 0.25 0.35 0.7) 0.25 2 ]
          Ambient = ambient 0.
          MaxBounces = 3; Atmosphere = None
          Duration = duration }

    /// Volume test room: a hazy stone hall lit by a low sun through slot windows in one wall, plus a lamp in
    /// the far corner, so the bounded haze shows shadowed sun shafts and a lamp halo. Slow push-in.
    let sunShafts () =
        let stone = solid (matte (rgb 0.62 0.58 0.52))
        let wall low high = box low high stone
        let slots = [ -4.6; -2.6; -0.6 ]                 // z of each slot's near edge, 0.45 wide
        let slotWidth, sill, head = 0.45, 0.9, 3.3
        let windowWall =
            [ yield wall (Point(4., 0., -6.)) (Point(4.25, sill, 2.))
              yield wall (Point(4., head, -6.)) (Point(4.25, 4., 2.))
              let edges = [ -6. ] @ (slots |> List.collect (fun z -> [ z; z + slotWidth ])) @ [ 2. ]
              for i in 0 .. 2 .. edges.Length - 2 do
                  yield wall (Point(4., sill, edges.[i])) (Point(4.25, head, edges.[i + 1])) ]
        let room =
            [ wall (Point(-4.25, -0.25, -6.25)) (Point(4.25, 0., 2.25))      // floor
              wall (Point(-4.25, 4., -6.25)) (Point(4.25, 4.25, 2.25))       // ceiling
              wall (Point(-4.25, 0., -6.25)) (Point(4.25, 4., -6.))          // back
              wall (Point(-4.25, 0., 2.)) (Point(4.25, 4., 2.25))            // front
              wall (Point(-4.25, 0., -6.)) (Point(-4., 4., 2.))              // left
              box (Point(-1.2, 0., -3.4)) (Point(-0.4, 1.6, -2.6)) (solid (plastic (rgb 0.7 0.2 0.12))) ]
        let lampAt = Point(-3.3, 2.6, -5.3)
        let bulb =
            Node.create "bulb" |> Node.at lampAt.X lampAt.Y lampAt.Z
            |> Node.withContent [ Geometry(sphere 0.04 (solid (EmissiveMaterial(rgb 1. 0.8 0.5, 3.) :> Material))) ]
        let focus = Node.create "focus" |> Node.at 0. 1.5 -3.5
        // --- v: volume (sun-shafts demo) ---
        let haze =
            { Volume.Default with
                Min = Point(-4., 0., -6.); Max = Point(4., 4., 2.)
                Scattering = rgb 0.09 0.09 0.09; Absorption = rgb 0.01 0.01 0.01
                SunAnisotropy = 0.7; LampAnisotropy = 0.2; SunSamples = 6; LampSamples = 2
                Ambient = rgb 0.02 0.025 0.03; LampClearance = 0.1 }
        { Name = "sun-shafts"
          Roots =
            [ bulb; focus
              cameraNode "camera" (Point(-3.4, 1.5, 1.5)) { CameraSpec.Default with YFov = 0.9; Target = Some "focus" } ]
          Clips =
            [ Clip.create "push-in"
                [ Clip.translate "camera" (Smooth.vector [ 0., Vector(-3.4, 1.5, 1.5); 4., Vector(-2.6, 1.4, 0.4) ]) ] ]
          ActiveCamera = "camera"
          Cuts = []
          StaticShapes = room @ windowWall
          StaticLights =
            [ DirectionalLight(rgb 1. 0.85 0.65, 4., Vector(1., 0.45, -0.35).Normalise) :> Light
              SphereLight(rgb 1. 0.75 0.45, 3., lampAt, 0.05) :> Light
              sky (rgb 0.85 0.9 1.) (rgb 0.3 0.5 0.9) 0.6 2 ]
          Ambient = ambient 0.
          MaxBounces = 4
          Atmosphere = Some { Atmosphere.Default with Density = 0.; Volume = Some haze }
          Duration = 4. }

    let all =
        [ { Name = "hop"; Description = "Keyframed ball hopping on cubic-spline arcs; orbiting look-at camera"; Build = hop }
          { Name = "rolling-ball"; Description = "Physics: ball rolls down a ramp and knocks a resting ball"; Build = rollingBall }
          { Name = "bouncing-ball"; Description = "Physics: bouncing ball with squash and stretch and motion blur"; Build = bouncingBall }
          { Name = "camera-dolly"; Description = "Crane-and-dolly camera move around a still life on smooth spline keys"; Build = cameraDolly }
          { Name = "lamp"; Description = "Luxo-style articulated lamp: anticipation, hop, squash, then nudges a physics ball"; Build = lamp }
          { Name = "sun-shafts"; Description = "Hazy stone room: shadowed sun shafts through slot windows and a lamp halo"; Build = sunShafts } ]

    let tryFind name = all |> List.tryFind (fun demo -> demo.Name = name)
