namespace Tracer.Basics

open System

/// Analytic height fog with aerial perspective.
///
/// Extinction falls off exponentially with altitude,
/// sigma(h) = Density * exp(-(h - BaseHeight) / ScaleHeight), scaled per channel by `Tint`, so the optical
/// depth along a straight segment has a closed form and no marching is needed. Light lost along a segment is
/// replaced by in-scattered light: the sky seen in roughly the viewing direction (so distant ranges fade into
/// the horizon they stand against), plus sunlight forward-scattered with a Henyey-Greenstein phase (so haze
/// glows around the sun). The in-scatter is unshadowed, which makes the segment integral exact:
/// L = L_surface * T + (1 - T) * S.
type Atmosphere =
    { /// Extinction coefficient per unit length at `BaseHeight`.
      Density: float
      /// Altitude at which the density equals `Density`.
      BaseHeight: float
      /// Altitude over which the density falls by a factor of e.
      ScaleHeight: float
      /// Relative extinction per channel. Above 1 in blue makes distance bluer and transmitted light warmer.
      Tint: Colour
      /// Henyey-Greenstein asymmetry of the sun scattering; positive scatters forward (glow around the sun).
      Anisotropy: float
      /// Multiplier on the sky in-scatter.
      SkyWeight: float
      /// Multiplier on the sun in-scatter (sun irradiance times phase).
      SunWeight: float
      /// Distance up to which rays that escape to the sky are fogged, so the sky meets the ranges smoothly.
      MaxDistance: float
      /// Sky radiance by direction used for in-scatter. None uses the scene's environment lights.
      Sky: (Vector -> Colour) option
      /// Directions are clamped to at least this elevation (as a y component) before looking up the sky, so
      /// haze seen while looking down takes the horizon's colour rather than the dark ground hemisphere.
      HorizonLift: float
      /// Per-sample radiance cap while building the blurred sky table, so a bright sun disc does not dominate.
      SkyClamp: float }

    static member Default =
        { Density = 1e-3; BaseHeight = 0.; ScaleHeight = 200.; Tint = Colour(1., 1., 1.)
          Anisotropy = 0.7; SkyWeight = 1.; SunWeight = 1.; MaxDistance = 1e4; Sky = None
          HorizonLift = 0.02; SkyClamp = 8. }

module Atmosphere =
    /// Optical depth of the segment origin + s * direction, s in [0, distance], for unit `direction`.
    /// Infinite when the segment is unbounded and does not climb. Monotonic non-decreasing in distance.
    let opticalDepth (atmosphere: Atmosphere) (origin: Point) (direction: Vector) (distance: float) =
        if distance <= 0. || atmosphere.Density <= 0. then 0.
        else
            let h = atmosphere.ScaleHeight
            let start = atmosphere.Density * exp (-(origin.Y - atmosphere.BaseHeight) / h)
            let dy = direction.Y
            if Double.IsPositiveInfinity distance then
                if dy > 1e-12 then start * h / dy else infinity
            else
                // integral_0^D exp(-s dy / h) ds = D * (1 - exp(-x)) / x with x = D dy / h.
                let x = distance * dy / h
                let factor = if abs x < 1e-4 then 1. - x * (0.5 - x / 6.) else (1. - exp (-x)) / x
                let depth = start * distance * factor
                if Double.IsNaN depth then infinity else depth

    /// Per-channel transmittance exp(-tau * Tint), in [0, 1].
    let transmittance (atmosphere: Atmosphere) (origin: Point) (direction: Vector) (distance: float) =
        let tau = opticalDepth atmosphere origin direction distance
        if tau = 0. then Colour(1., 1., 1.)
        else
            let t = atmosphere.Tint
            Colour(exp (-tau * t.R), exp (-tau * t.G), exp (-tau * t.B))

    /// Henyey-Greenstein phase function (per steradian) for the cosine between propagation directions.
    let henyeyGreenstein (g: float) (cosine: float) =
        let denominator = 1. + g * g - 2. * g * cosine
        (1. - g * g) / (4. * Math.PI * denominator * sqrt denominator)

/// An atmosphere bound to a scene's lights: the sky lookup and sun directions resolved once.
type AtmosphereMedium(atmosphere: Atmosphere, lights: Light list) =
    let suns =
        lights |> List.choose (function
            | :? DirectionalLight as sun -> Some (sun.Direction, sun.GetColour Unchecked.defaultof<HitPoint>)
            | _ -> None)
        |> Array.ofList
    let environments = lights |> List.choose (function :? EnvironmentLight as e -> Some e | _ -> None)
    let rawSky : Vector -> Colour =
        match atmosphere.Sky with
        | Some sky -> sky
        | None ->
            if List.isEmpty environments then (fun _ -> Colour(0., 0., 0.))
            else
                fun d ->
                    let mutable c = Colour(0., 0., 0.)
                    for e in environments do c <- c + e.Radiance d
                    c

    // A low-resolution, blurred lat-long table of the sky (about 6 degree cells, each averaging a ~10 degree
    // cone), so in-scatter is cheap per ray and smooth.
    let columns, rows = 64, 32
    let direction (u: float) (v: float) =
        let polar = (1. - v) * Math.PI
        let azimuth = 2. * Math.PI * u
        Vector(sin polar * sin azimuth, cos polar, sin polar * cos azimuth)
    let table =
        lazy (
            let cap = atmosphere.SkyClamp
            let clampC (c: Colour) =
                let m = max c.R (max c.G c.B)
                if m > cap && m > 0. then c * (cap / m) else c
            let offsets =
                [| for i in 0 .. 3 do
                     for j in 0 .. 3 do
                         yield (float i + 0.5) / 4. - 0.5, (float j + 0.5) / 4. - 0.5 |]
            Array.init (rows * columns) (fun k ->
                let row, column = k / columns, k % columns
                let mutable sum = Colour(0., 0., 0.)
                for (du, dv) in offsets do
                    let u = (float column + 0.5 + 1.8 * du) / float columns
                    let v = (float row + 0.5 + 3.6 * dv) / float rows
                    let v = max 0. (min 1. v)
                    sum <- sum + clampC (rawSky (direction u v).Normalise)
                sum * (1. / float offsets.Length)))

    let skyAt (d: Vector) =
        let d = Vector(d.X, max d.Y atmosphere.HorizonLift, d.Z).Normalise
        let longitude = Math.Atan2(d.X, d.Z) / (2. * Math.PI)
        let u = if longitude < 0. then longitude + 1. else longitude
        let v = 1. - Math.Acos(max -1. (min 1. d.Y)) / Math.PI
        let x = u * float columns - 0.5
        let y = max 0. (min (float rows - 1.) (v * float rows - 0.5))
        let x0 = int (floor x)
        let fx = x - float x0
        let y0 = min (rows - 2) (int (floor y))
        let fy = y - float y0
        let t = table.Value
        let at r c = t.[r * columns + ((c % columns + columns) % columns)]
        let top = at y0 x0 * (1. - fx) + at y0 (x0 + 1) * fx
        let bottom = at (y0 + 1) x0 * (1. - fx) + at (y0 + 1) (x0 + 1) * fx
        top * (1. - fy) + bottom * fy

    member _.Settings = atmosphere

    /// In-scattered radiance per unit extinction for light arriving along `direction` (unit, camera to scene).
    member _.InScatter(direction: Vector) =
        let mutable s = if atmosphere.SkyWeight > 0. then skyAt direction * atmosphere.SkyWeight else Colour(0., 0., 0.)
        if atmosphere.SunWeight > 0. then
            for (sunDirection, irradiance) in suns do
                let phase = Atmosphere.henyeyGreenstein atmosphere.Anisotropy (direction * sunDirection)
                s <- s + irradiance * (phase * atmosphere.SunWeight)
        s

    member _.Transmittance(origin: Point, direction: Vector, distance: float) =
        Atmosphere.transmittance atmosphere origin direction distance

    /// Radiance `radiance` leaving the point at `distance` along the unit `direction` from `origin`, as seen
    /// at `origin`.
    member this.Apply(origin: Point, direction: Vector, distance: float, radiance: Colour) =
        let t = this.Transmittance(origin, direction, distance)
        if t.R = 1. && t.G = 1. && t.B = 1. then radiance
        else
            let s = this.InScatter direction
            Colour(radiance.R * t.R + (1. - t.R) * s.R,
                   radiance.G * t.G + (1. - t.G) * s.G,
                   radiance.B * t.B + (1. - t.B) * s.B)

    /// The fog over a ray that escapes to the sky, which is fogged up to `MaxDistance`.
    member this.ApplyEscaped(origin: Point, direction: Vector, radiance: Colour) =
        this.Apply(origin, direction, atmosphere.MaxDistance, radiance)
