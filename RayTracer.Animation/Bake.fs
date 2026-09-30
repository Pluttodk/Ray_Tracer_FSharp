namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Animation.Physics

/// Cartoon squash and stretch: volume-preserving scaling along an axis.
module SquashStretch =
    type Settings =
        { /// Squash per m/s of impact speed.
          ImpactGain: float
          /// Largest squash, as a fraction of the radius (0.4 flattens to 60% height).
          MaxSquash: float
          /// Natural frequency of the wobble after an impact, in Hz.
          Frequency: float
          /// Damping ratio of the wobble (0 rings forever, 1 settles without overshoot).
          Damping: float
          /// Stretch per m/s of airborne speed, along the direction of travel.
          StretchGain: float
          MaxStretch: float }

    let defaults =
        { ImpactGain = 0.05; MaxSquash = 0.4; Frequency = 5.; Damping = 0.3; StretchGain = 0.025; MaxStretch = 0.3 }

    /// Rotation taking +Y onto `axis`.
    let alignY (axis: Vector) =
        let a = axis.Normalise
        let y = Vector(0., 1., 0.)
        let d = y * a
        if d > 0.999999999 then Quaternion.identity
        elif d < -0.999999999 then Quaternion.ofAxisAngle (Vector(1., 0., 0.)) Math.PI
        else Quaternion.ofAxisAngle (y % a) (acos d)

    /// Scale along local Y by `s`, with the perpendicular axes scaled by 1/sqrt(s) so volume is preserved.
    let volumeScale s = Vector(1. / sqrt s, s, 1. / sqrt s)

    /// Damped oscillation of the impact squash `time` seconds after impact: 1 - A e^(-zeta w t) cos(w_d t).
    let impactFactor (settings: Settings) amplitude time =
        let w = 2. * Math.PI * settings.Frequency
        let envelope = exp (-settings.Damping * w * time)
        let wd = w * sqrt (max 0. (1. - settings.Damping * settings.Damping))
        1. - amplitude * envelope * cos (wd * time), envelope

/// Turns simulated trajectories into ordinary keyframe tracks.
module Bake =
    /// Node hierarchy for a baked body: `name` (translation) -> `name-squash` (impact squash) ->
    /// `name-stretch` (airborne stretch) -> `name-spin` (rolling, carries the geometry). Each squash node rotates
    /// into its axis, scales, and the next node rotates back, so every level stays a plain glTF TRS.
    let bodyNode (name: string) (geometry: Shape) =
        Node.create name
        |> Node.withChildren
            [ Node.create (name + "-squash")
              |> Node.withChildren
                  [ Node.create (name + "-stretch")
                    |> Node.withChildren [ Node.create (name + "-spin") |> Node.withContent [ Geometry geometry ] ] ] ]

    /// Keyframe channels reproducing `trajectory`, optionally with squash and stretch.
    let channels (trajectory: Trajectory) (squash: SquashStretch.Settings option) =
        let name = trajectory.Body.Name
        let radius = trajectory.Body.Radius
        let samples = trajectory.Samples
        let n = samples.Length
        let translations = Array.zeroCreate<float * Vector> n
        let squashRotations = Array.zeroCreate<float * Quaternion> n
        let squashScales = Array.zeroCreate<float * Vector> n
        let stretchRotations = Array.zeroCreate<float * Quaternion> n
        let stretchScales = Array.zeroCreate<float * Vector> n
        let spins = Array.zeroCreate<float * Quaternion> n
        let mutable impactTime = -infinity
        let mutable impactAmplitude = 0.
        let mutable impactAxis = Vector(0., 1., 0.)
        let mutable lastContact = -infinity
        for i in 0 .. n - 1 do
            let sample = samples.[i]
            let t = sample.Time
            if sample.Contact.IsSome then lastContact <- t
            let squashAxis, squashFactor, stretchAxis, stretchFactor =
                match squash with
                | None -> Vector(0., 1., 0.), 1., Vector(0., 1., 0.), 1.
                | Some settings ->
                    let amplitude = min settings.MaxSquash (settings.ImpactGain * sample.ImpactSpeed)
                    if sample.ImpactSpeed > 0. && amplitude > 0.01 then
                        impactTime <- t
                        impactAmplitude <- amplitude
                        impactAxis <- sample.ImpactNormal.Normalise
                    let factor, envelope =
                        if Double.IsNegativeInfinity impactTime then 1., 0.
                        else SquashStretch.impactFactor settings impactAmplitude (t - impactTime)
                    // Stretch only when airborne, fading in after leaving a surface and while an impact still rings.
                    let speed = sample.Velocity.Magnitude
                    let airborne = if sample.Contact.IsSome then 0. else min 1. ((t - lastContact) / 0.08)
                    let stretch = 1. + airborne * (1. - envelope) * min settings.MaxStretch (settings.StretchGain * speed)
                    // Stretch is symmetric, so use the axis in the upper hemisphere; it then does not flip at the apex.
                    let direction =
                        if speed <= 1e-9 then Vector(0., 1., 0.)
                        elif sample.Velocity.Y < 0. then -sample.Velocity / speed
                        else sample.Velocity / speed
                    impactAxis, factor, direction, stretch
            let squashAlign = SquashStretch.alignY squashAxis
            let stretchAlign = SquashStretch.alignY stretchAxis
            // Keep the contact side of the ball where it was while squashing, so it flattens onto the floor.
            let p = sample.Position
            let centre = Vector(p.X, p.Y, p.Z) - (radius * (1. - squashFactor)) * squashAxis
            translations.[i] <- t, centre
            squashRotations.[i] <- t, squashAlign
            squashScales.[i] <- t, SquashStretch.volumeScale squashFactor
            stretchRotations.[i] <- t, Quaternion.multiply (Quaternion.conjugate squashAlign) stretchAlign
            stretchScales.[i] <- t, SquashStretch.volumeScale stretchFactor
            spins.[i] <- t, Quaternion.multiply (Quaternion.conjugate stretchAlign) sample.Orientation
        let linear (keys: (float * 'T)[]) = Sampler.linear (List.ofArray keys)
        [ Clip.translate name (linear translations)
          Clip.rotate (name + "-squash") (linear squashRotations)
          Clip.scale (name + "-squash") (linear squashScales)
          Clip.rotate (name + "-stretch") (linear stretchRotations)
          Clip.scale (name + "-stretch") (linear stretchScales)
          Clip.rotate (name + "-spin") (linear spins) ]

    /// Simulates `world` for `duration` seconds and returns the body nodes and a clip that replays them.
    /// `geometry` gives each body's shape, centred on the origin. Samples are taken at `sampleRate` Hz, well
    /// above the frame rate, so motion blur follows bounces instead of cutting corners between frames.
    let simulate (world: World) (geometry: Body -> Shape) (squash: SquashStretch.Settings option) duration sampleRate clipName =
        let trajectories = Physics.simulate world duration sampleRate
        let nodes = trajectories |> Array.map (fun trajectory -> bodyNode trajectory.Body.Name (geometry trajectory.Body)) |> List.ofArray
        let clip = Clip.create clipName (trajectories |> Seq.collect (fun trajectory -> channels trajectory squash) |> List.ofSeq)
        nodes, clip, trajectories
