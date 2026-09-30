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

    let all =
        [ { Name = "hop"; Description = "Keyframed ball hopping on cubic-spline arcs; orbiting look-at camera"; Build = hop } ]

    let tryFind name = all |> List.tryFind (fun demo -> demo.Name = name)
