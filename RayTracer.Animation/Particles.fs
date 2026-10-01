namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Basics.Textures

/// Wing-driven snow spindrift. The whole cloud is a pure function of time: emission events happen at fixed
/// instants, every random number is a hash of (seed, event, particle, channel), and a particle's position at
/// time t is closed-form in its age. Any frame can therefore be built independently, with no state carried
/// between frames and nothing that flickers.
module Particles =
    /// What the simulation needs to know about the world (kept abstract so it compiles before the film).
    type Source =
        { /// The flier's body position at time t.
          Position: float -> Vector
          /// Terrain height at (x, z).
          Ground: float -> float -> float
          /// Snow cover at (x, z) in [0, 1].
          Snow: float -> float -> float }

    type Settings =
        { Seed: int
          /// Emission window (seconds).
          Start: float
          Stop: float
          /// Emission events per second.
          Rate: float
          /// Particles attempted per event (thinned by downwash strength and snow cover).
          PerEvent: int
          /// Height above the surface (flier body) where the downwash begins to lift snow, and where it is full.
          ReachHeight: float
          FullHeight: float
          /// Steady wind (units per second, horizontal) the plume drifts on.
          Wind: Vector
          /// Particle life span range (seconds).
          MinLife: float
          MaxLife: float
          /// Nominal particle radius at birth (world units).
          Radius: float }

    /// A single particle at one instant.
    [<Struct>]
    type Particle = { Centre: Vector; Radius: float; Age: float; Life: float }

    let private smoothstep a b x =
        let t = max 0. (min 1. ((x - a) / (b - a)))
        t * t * (3. - 2. * t)

    /// Deterministic uniform number in [0, 1) for (seed, event, particle, channel).
    let hash01 (seed: int) (a: int) (b: int) (c: int) =
        let mutable h = uint64 (uint32 seed) * 0x9E3779B97F4A7C15UL
        for v in [| a; b; c |] do
            h <- (h ^^^ uint64 (uint32 v)) * 0xBF58476D1CE4E5B9UL
            h <- h ^^^ (h >>> 29)
        h <- (h ^^^ (h >>> 31)) * 0x94D049BB133111EBUL
        h <- h ^^^ (h >>> 32)
        float (h >>> 11) / float (1UL <<< 53)

    let defaults =
        { Seed = 5; Start = 15.0; Stop = 18.0; Rate = 30.; PerEvent = 450
          ReachHeight = 110.; FullHeight = 45.
          Wind = Vector(6., 0., 2.5)
          MinLife = 1.; MaxLife = 3.; Radius = 0.13 }

    /// Upper bound on the particles that can exist at once.
    let maxLive (s: Settings) = int (ceil (s.Rate * s.MaxLife)) * s.PerEvent

    /// Every particle alive at time t (empty before emission starts and after the last has faded).
    let at (s: Settings) (source: Source) (t: float) : Particle[] =
        if t < s.Start || t > s.Stop + s.MaxLife then [||]
        else
            let first = max 0 (int (ceil ((t - s.MaxLife - s.Start) * s.Rate)))
            let last = min (int (floor ((s.Stop - s.Start) * s.Rate))) (int (floor ((t - s.Start) * s.Rate)))
            let result = ResizeArray<Particle>()
            for e in first .. last do
                let te = s.Start + float e / s.Rate
                let p = source.Position te
                let velocity = (source.Position (te + 0.05) - source.Position (te - 0.05)) / 0.1
                let heading = Vector(velocity.X, 0., velocity.Z)
                let height = p.Y - source.Ground p.X p.Z
                let strength = smoothstep s.ReachHeight s.FullHeight height
                if heading.Magnitude > 1e-6 && strength > 0. then
                    let forward = heading.Normalise
                    let side = Vector(-forward.Z, 0., forward.X)
                    for k in 0 .. s.PerEvent - 1 do
                        let r c = hash01 s.Seed e k c
                        // Footprint of the downwash: under and behind the body, spread across the wings.
                        let along = -26. + 34. * r 0
                        let across = (r 1 + r 2 + r 3 - 1.5) * 2. * (16. + 0.2 * height)
                        let x0 = p.X + forward.X * along + side.X * across
                        let z0 = p.Z + forward.Z * along + side.Z * across
                        let cover = source.Snow x0 z0
                        if r 4 < strength * cover then
                            let age = t - te
                            let life = s.MinLife + (s.MaxLife - s.MinLife) * (r 5 * r 5)
                            if age < life then
                                // Blown out sideways and lifted by the downwash, both dying away with drag; the wind then
                                // picks the snow up gradually and carries it off.
                                let outward = (if across < 0. then -1. else 1.) * (2. + 7. * r 6)
                                let lift = 1.5 + 8. * r 7
                                let tauOut, tauLift, tauWind = 0.6, 0.9, 0.9
                                let drag tau = tau * (1. - exp (-age / tau))
                                let carried = Vector(velocity.X, 0., velocity.Z) * (0.12 * drag 0.5)
                                let wind = s.Wind * (0.6 + 0.8 * r 8)
                                let windTravel = age - drag tauWind
                                let wobble = 0.5 * sqrt age
                                let x = x0 + side.X * outward * drag tauOut + carried.X + wind.X * windTravel + wobble * (r 9 - 0.5) * 2.
                                let z = z0 + side.Z * outward * drag tauOut + carried.Z + wind.Z * windTravel + wobble * (r 10 - 0.5) * 2.
                                let rise = lift * drag tauLift
                                let radius0 = s.Radius * (0.6 + 0.8 * r 11)
                                let u = age / life
                                // Puffs swell as they disperse, then shrink to nothing over the last half of their life.
                                let size = radius0 * (0.8 + 0.5 * u) * smoothstep 0. 0.06 u * (1. - smoothstep 0.45 1. u)
                                let y = max (source.Ground x z + size + 0.1) (source.Ground x0 z0 + 0.3 + rise)
                                if size > 1e-3 then
                                    result.Add { Centre = Vector(x, y, z); Radius = size; Age = age; Life = life }
            result.ToArray()

    /// Faintly translucent white snow: back light shows through, so the cloud glows against the low sun.
    let private snowTexture =
        mkMatTexture (PbrMaterial({ PbrSample.defaults with
                                      BaseColour = Colour(0.92, 0.95, 1.)
                                      Roughness = 1.
                                      DiffuseTransmission = 0.75
                                      DiffuseTransmissionColour = Colour(1., 0.97, 0.92) }))

    /// The cloud at time t as renderable spheres.
    let shapes (s: Settings) (source: Source) (t: float) : Shape list =
        [ for p in at s source t -> SphereShape(Point(p.Centre.X, p.Centre.Y, p.Centre.Z), p.Radius, snowTexture) :> Shape ]
