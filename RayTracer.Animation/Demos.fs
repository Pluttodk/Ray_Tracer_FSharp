namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Animation.Stage

/// Built-in example animations.
module Demos =
    type Demo = { Name: string; Description: string; Build: unit -> AnimatedScene }

    let private up = Vector(0., 1., 0.)

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
          Roots = [ ball; pivot ]
          Clips =
            [ Clip.create "hop"
                [ Clip.translate "ball" (Sampler.linear travelKeys |> Sampler.withEase Easing.linear)
                  Clip.translate "ball-lift" (Sampler.cubic heightKeys)
                  Clip.rotate "ball-spin" (Sampler.linear spinKeys)
                  Clip.rotate "camera-pivot" (Sampler.linear orbitKeys |> Sampler.withEase Easing.smoothstep) ] ]
          ActiveCamera = "camera"
          StaticShapes = [ ground (floorChecker 1. (matte (rgb 0.8 0.8 0.8)) (matte (rgb 0.35 0.4 0.5))) ]
          StaticLights = [ sun (Vector(0.4, 1., 0.6)) 0.85; sky (rgb 0.85 0.9 1.) (rgb 0.3 0.5 0.9) 0.35 2 ]
          Ambient = ambient 0.
          MaxBounces = 3
          Duration = duration }

    let private floor () = ground (floorChecker 1. (matte (rgb 0.8 0.8 0.8)) (matte (rgb 0.35 0.4 0.5)))
    let private daylight () = [ sun (Vector(0.4, 1., 0.6)) 0.85; sky (rgb 0.85 0.9 1.) (rgb 0.3 0.5 0.9) 0.35 2 ]
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
          Roots = nodes @ [ focus; camera ]
          Clips = [ clip; Clip.create "camera" [ Clip.translate "focus" (Sampler.linear [ 0., Vector(-2.5, 1., 0.); 3., Vector(1.5, 0.6, 0.); duration, Vector(3.5, 0.5, 0.) ] |> Sampler.withEase Easing.smoothstep) ] ]
          ActiveCamera = "camera"
          StaticShapes = [ floor (); colliderBox centre half rotation (solid (plastic (rgb 0.75 0.55 0.3))) ]
          StaticLights = daylight ()
          Ambient = ambient 0.
          MaxBounces = 3
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
          Roots = nodes @ [ focus; camera ]
          Clips = [ clip ]
          ActiveCamera = "camera"
          StaticShapes = [ floor () ]
          StaticLights = daylight ()
          Ambient = ambient 0.
          MaxBounces = 3
          Duration = duration }

    let all =
        [ { Name = "hop"; Description = "Keyframed ball hopping on cubic-spline arcs; orbiting look-at camera"; Build = hop }
          { Name = "rolling-ball"; Description = "Physics: ball rolls down a ramp and knocks a resting ball"; Build = rollingBall }
          { Name = "bouncing-ball"; Description = "Physics: bouncing ball with squash and stretch and motion blur"; Build = bouncingBall } ]

    let tryFind name = all |> List.tryFind (fun demo -> demo.Name = name)
