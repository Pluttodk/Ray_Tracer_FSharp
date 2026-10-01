namespace Tracer.Basics

open System
open System.Threading

/// A bounded participating medium (dust, haze) that scatters sunlight and lamp light once, WITH shadows, so
/// light streaming through openings carves visible shafts and lamps get soft halos.
///
/// The medium fills an axis-aligned box and is homogeneous, or thins with altitude as
/// rho(y) = exp(-(y - BaseHeight) / ScaleHeight). Rays outside the box, and the sky beyond it, are untouched.
/// Along each ray segment through the box the integrators add the single-scattered radiance
/// integral sigma_s * phase * L_light * T_view * T_light * V over the segment, estimated with a few
/// stratified distance samples, each casting one shadow ray (transmittance-proportional sampling for suns,
/// a mixture with equiangular sampling for lamps), and attenuate whatever lies beyond by the box
/// transmittance. Samples are keyed by the path tracer's per-pixel sample keys, so the noise is white and
/// the denoiser removes it.
type Volume =
    { /// Lower corner of the region holding the medium.
      Min: Point
      /// Upper corner of the region holding the medium.
      Max: Point
      /// Scattering coefficient per unit length, per channel, at `BaseHeight`.
      Scattering: Colour
      /// Absorption coefficient per unit length, per channel, at `BaseHeight`.
      Absorption: Colour
      /// Altitude at which the coefficients apply as given.
      BaseHeight: float
      /// Altitude over which the density falls by a factor of e. Infinity (or <= 0) is homogeneous.
      ScaleHeight: float
      /// Henyey-Greenstein asymmetry for sunlight; dust scatters forward (0.6-0.8).
      SunAnisotropy: float
      /// Henyey-Greenstein asymmetry for point lights; near isotropic gives round halos.
      LampAnisotropy: float
      /// Distance samples (one shadow ray each) per ray segment for each sun.
      SunSamples: int
      /// Distance samples (one lamp, one shadow ray each) per ray segment shared by all point lights.
      LampSamples: int
      /// Deepest path vertex whose outgoing segment gets in-scattered light: 0 is camera rays only. Deeper
      /// segments are still attenuated and receive the ambient term.
      ScatterDepth: int
      /// The medium ends this far along any ray from its origin.
      MaxDistance: float
      /// Unshadowed radiance scattered in isotropically everywhere in the region (sky and bounce fill).
      Ambient: Colour
      /// Artistic multiplier on the sun in-scatter.
      SunWeight: float
      /// Artistic multiplier on the lamp in-scatter.
      LampWeight: float
      /// Lamp shadow rays stop this short of the light, so a bulb inside a lamp housing still lights the haze.
      LampClearance: float }

    // --- v: volume ---
    static member Default =
        { Min = Point(-1., -1., -1.); Max = Point(1., 1., 1.)
          Scattering = Colour(0.02, 0.02, 0.02); Absorption = Colour(0.002, 0.002, 0.002)
          BaseHeight = 0.; ScaleHeight = infinity
          SunAnisotropy = 0.7; LampAnisotropy = 0.2
          SunSamples = 4; LampSamples = 2; ScatterDepth = 0; MaxDistance = 1e4
          Ambient = Colour(0., 0., 0.); SunWeight = 1.; LampWeight = 1.; LampClearance = 0. }

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
      SkyClamp: float
      /// A bounded medium with shadowed single scattering (sun shafts, lamp halos). None for none. Set
      /// `Density` to 0 to use it without the height fog.
      Volume: Volume option }

    static member Default =
        { Density = 1e-3; BaseHeight = 0.; ScaleHeight = 200.; Tint = Colour(1., 1., 1.)
          Anisotropy = 0.7; SkyWeight = 1.; SunWeight = 1.; MaxDistance = 1e4; Sky = None
          HorizonLift = 0.02; SkyClamp = 8.; Volume = None }

module Volume =
    let private homogeneous (volume: Volume) =
        not (volume.ScaleHeight > 0.) || Double.IsPositiveInfinity volume.ScaleHeight

    /// Extinction coefficient sigma_s + sigma_a at `BaseHeight`, per channel.
    let extinction (volume: Volume) = volume.Scattering + volume.Absorption

    /// Relative density at altitude `y`.
    let density (volume: Volume) (y: float) =
        if homogeneous volume then 1. else exp (-(y - volume.BaseHeight) / volume.ScaleHeight)

    /// Integral of the relative density over `length` along a straight line starting at altitude `y` and
    /// climbing `dy` per unit length.
    let densityIntegral (volume: Volume) (y: float) (dy: float) (length: float) =
        if not (length > 0.) then 0.
        elif homogeneous volume then length
        else
            let x = length * dy / volume.ScaleHeight
            let factor = if abs x < 1e-4 then 1. - x * (0.5 - x / 6.) else (1. - exp (-x)) / x
            let integral = density volume y * length * factor
            if Double.IsNaN integral then infinity else integral

    /// Per-channel transmittance through the medium over `length` from altitude `y` climbing `dy` per unit.
    /// Assumes the whole stretch is inside the region.
    let transmittance (volume: Volume) (y: float) (dy: float) (length: float) =
        let tau = densityIntegral volume y dy length
        if tau = 0. then Colour(1., 1., 1.)
        else
            let e = extinction volume
            Colour(exp (-tau * e.R), exp (-tau * e.G), exp (-tau * e.B))

    /// Ray parameters where `origin + t * direction` enters and leaves the region, clipped to [0, maximum].
    let clip (volume: Volume) (origin: Point) (direction: Vector) (maximum: float) =
        let mutable enter = 0.
        let mutable leave = maximum
        let mutable missed = false
        let axis (o: float) (d: float) (low: float) (high: float) =
            if d = 0. then
                if o < low || o > high then missed <- true
            else
                let inverse = 1. / d
                let a = (low - o) * inverse
                let b = (high - o) * inverse
                enter <- max enter (min a b)
                leave <- min leave (max a b)
        axis origin.X direction.X volume.Min.X volume.Max.X
        axis origin.Y direction.Y volume.Min.Y volume.Max.Y
        axis origin.Z direction.Z volume.Min.Z volume.Max.Z
        if missed || not (leave > enter) then ValueNone else ValueSome(struct (enter, leave))

    /// Whether the volume can change any ray at all.
    let isActive (volume: Volume) =
        let e = extinction volume
        (e.R > 0. || e.G > 0. || e.B > 0.) && volume.Max.X > volume.Min.X && volume.Max.Y > volume.Min.Y
        && volume.Max.Z > volume.Min.Z && volume.MaxDistance > 0.

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

    // ---- Bounded volume with shadowed single scattering -------------------------------------------------

    let volume = atmosphere.Volume |> Option.filter Volume.isActive
    let luminance (c: Colour) = 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B
    /// Radiance a point or sphere lamp delivers at `p` (its colour times its distance falloff).
    let lampRadiance (light: Light) (p: Point) =
        let hit = HitPoint(p)
        light.GetColour hit * light.GetGeometricFactor hit
    /// Point and sphere lamps (position, falloff, power, shadow-ray clearance), with how their radiance falls
    /// with distance probed once: lights following an inverse square are importance sampled equiangularly,
    /// constant ones by transmittance.
    let lamps =
        lights |> List.choose (fun light ->
            let lamp =
                match light with
                | :? PointLight as point -> Some (point.Position, 0.)
                | :? SphereLight as sphere -> Some (sphere.Position, sphere.Radius)
                | _ -> None
            match lamp with
            | None -> None
            | Some (position, radius) ->
                let at d = luminance (lampRadiance light (position + Vector(0., d, 0.)))
                let near, far = at (max 1. (2. * radius)), at (4. * max 1. (2. * radius))
                if not (near > 0.) && not (far > 0.) then None
                else
                    let inverseSquare = far < 0.5 * near
                    let scale = max 1. (2. * radius)
                    let power = if inverseSquare then near * scale * scale else max near far
                    Some struct (light, position, inverseSquare, power, radius))
        |> Array.ofList
    let lampWeights = new ThreadLocal<float[]>(fun () -> Array.zeroCreate lamps.Length)

    /// Per-channel transmittance through the medium over a stretch inside the region.
    let transmittanceAlong (v: Volume) (origin: Point) (direction: Vector) (length: float) =
        Volume.transmittance v origin.Y direction.Y length

    /// Distance from a point inside the region to its boundary along `direction`, at most `maximum`.
    let exitDistance (v: Volume) (p: Point) (direction: Vector) (maximum: float) =
        match Volume.clip v p direction maximum with
        | ValueSome (struct (_, leave)) -> leave
        | ValueNone -> 0.

    /// 1 - exp(-x) without cancellation for small x.
    let oneMinusExp (x: float) = if x < 1e-3 then x * (1. - x * (0.5 - x / 6.)) else 1. - exp (-x)

    /// Distance sampled in [0, length] proportionally to exp(-sigma * s), and its density.
    let sampleExponential (sigma: float) (length: float) (u: float) =
        let x = sigma * length
        if x < 1e-6 then struct (u * length, 1. / length)
        else
            let normaliser = oneMinusExp x
            let s = min length (-Math.Log(1. - u * normaliser) / sigma)
            struct (s, sigma * exp (-sigma * s) / normaliser)

    let exponentialPdf (sigma: float) (length: float) (s: float) =
        let x = sigma * length
        if x < 1e-6 then 1. / length else sigma * exp (-sigma * s) / oneMinusExp x

    /// Single-scattered radiance gathered over the segment [enter, leave] of the unit-direction ray, plus the
    /// transmittance across it. `visible` gives the transmittance of a shadow ray up to a distance.
    let scatter (v: Volume) (origin: Point) (direction: Vector) (enter: float) (leave: float) (time: float)
                (key: uint64) (sampled: bool) (visible: Ray -> float -> Colour) =
        let length = leave - enter
        let start = origin + enter * direction
        let sigmaS = v.Scattering
        let sigmaT = Volume.extinction v
        let across = transmittanceAlong v start direction length
        let mutable r = 0.
        let mutable g = 0.
        let mutable b = 0.
        // Unshadowed ambient fill: the integral of T sigma_s A over the segment is A sigma_s / sigma_t (1 - T).
        if not v.Ambient.IsBlack then
            let fill (s: float) (t: float) (transmitted: float) = if t > 0. then s / t * (1. - transmitted) else 0.
            r <- v.Ambient.R * fill sigmaS.R sigmaT.R across.R
            g <- v.Ambient.G * fill sigmaS.G sigmaT.G across.G
            b <- v.Ambient.B * fill sigmaS.B sigmaT.B across.B
        if sampled && not sigmaS.IsBlack && length > 0. then
            let midpoint = Volume.density v (start.Y + 0.5 * length * direction.Y)
            let sigmaBar = (sigmaT.R + sigmaT.G + sigmaT.B) / 3. * midpoint
            let volumeKey = Sampling.mixKey (key ^^^ 0x7F4A7C159E3779B9UL)
            // Adds the light arriving along `toLight` with radiance `incoming`, scattered towards the camera at
            // distance s past `start`, times `weight` (1 / density), attenuated along both legs and shadowed.
            let accumulate (s: float) (incoming: Colour) (toLight: Vector) (lightDistance: float) (anisotropy: float)
                           (weight: float) (shadowLength: float) =
                let p = start + s * direction
                let phase = Atmosphere.henyeyGreenstein anisotropy (toLight * direction)
                let factor = phase * weight * Volume.density v p.Y
                if factor > 0. && Double.IsFinite factor && not incoming.IsBlack then
                    let view = transmittanceAlong v start direction s
                    let light = transmittanceAlong v p toLight (exitDistance v p toLight lightDistance)
                    let cr = incoming.R * sigmaS.R * view.R * light.R * factor
                    let cg = incoming.G * sigmaS.G * view.G * light.G * factor
                    let cb = incoming.B * sigmaS.B * view.B * light.B * factor
                    if cr > 0. || cg > 0. || cb > 0. then
                        let visibility = visible (Ray(p, toLight, time)) shadowLength
                        r <- r + cr * visibility.R
                        g <- g + cg * visibility.G
                        b <- b + cb * visibility.B
            // Suns: transmittance-proportional distances, stratified over the segment.
            if v.SunWeight > 0. && v.SunSamples > 0 then
                let n = v.SunSamples
                for sunIndex = 0 to suns.Length - 1 do
                    let (sunDirection, irradiance) = suns.[sunIndex]
                    let sunKey = Sampling.mixKey (volumeKey ^^^ uint64 (sunIndex + 1))
                    for i = 0 to n - 1 do
                        let jitter, _ = Sampling.sample2D sunKey i
                        let struct (s, pdf) = sampleExponential sigmaBar length ((float i + jitter) / float n)
                        if pdf > 0. then
                            accumulate s irradiance sunDirection infinity v.SunAnisotropy
                                (v.SunWeight / (pdf * float n)) infinity
            // Lamps: one lamp per sample, picked by its unoccluded contribution to the segment, then a
            // distance from a mixture of equiangular (for inverse-square lamps) and transmittance sampling.
            if v.LampWeight > 0. && v.LampSamples > 0 && lamps.Length > 0 then
                let weights = lampWeights.Value
                let mutable total = 0.
                for j = 0 to lamps.Length - 1 do
                    let struct (_, position, inverseSquare, power, radius) = lamps.[j]
                    let w =
                        if inverseSquare then
                            let toLamp = position - start
                            let along = toLamp * direction
                            let d = max (max 1e-4 radius) (sqrt (max 0. (toLamp.MagnitudeSquared - along * along)))
                            power * (atan ((length - along) / d) - atan (-along / d)) / d
                        else power * length
                    let w = if Double.IsFinite w && w > 0. then w else 0.
                    weights.[j] <- w
                    total <- total + w
                if total > 0. then
                    let n = v.LampSamples
                    let lampKey = Sampling.mixKey (volumeKey ^^^ 0xA24BAED4963EE407UL)
                    for i = 0 to n - 1 do
                        let uPick, uStrategy = Sampling.sample2D lampKey (2 * i)
                        let jitter, _ = Sampling.sample2D lampKey (2 * i + 1)
                        let u = (float i + jitter) / float n
                        let mutable j = 0
                        let mutable target = uPick * total
                        while j < lamps.Length - 1 && (target >= weights.[j] || weights.[j] = 0.) do
                            target <- target - weights.[j]
                            j <- j + 1
                        if weights.[j] > 0. then
                            let struct (light, position, inverseSquare, _, radius) = lamps.[j]
                            let pick = weights.[j] / total
                            let toLamp = position - start
                            let along = toLamp * direction
                            let d = max (max 1e-4 radius) (sqrt (max 0. (toLamp.MagnitudeSquared - along * along)))
                            let thetaA = atan (-along / d)
                            let thetaB = atan ((length - along) / d)
                            let equiangularShare = if inverseSquare && thetaB > thetaA then 0.75 else 0.
                            let s =
                                if uStrategy < equiangularShare then
                                    max 0. (min length (along + d * tan (thetaA + u * (thetaB - thetaA))))
                                else
                                    let struct (s, _) = sampleExponential sigmaBar length u
                                    s
                            let pdf =
                                let x = s - along
                                (if equiangularShare > 0. then
                                    equiangularShare * d / ((thetaB - thetaA) * (d * d + x * x))
                                 else 0.)
                                + (1. - equiangularShare) * exponentialPdf sigmaBar length s
                            if pdf > 0. && Double.IsFinite pdf then
                                let p = start + s * direction
                                let offset = position - p
                                let distance = offset.Magnitude
                                if distance > 0. then
                                    let toLight = offset.Normalise
                                    let incoming = lampRadiance light p
                                    accumulate s incoming toLight distance v.LampAnisotropy
                                        (v.LampWeight / (pick * pdf * float n))
                                        (max 0. (distance - max (max v.LampClearance radius) (1e-6 * distance)))
        struct (Colour(r, g, b), across)

    /// Whether a bounded volume is present and can change any ray.
    member _.HasVolume = volume.IsSome

    /// The bounded volume over the segment origin + s * direction, s in [0, distance] (unit `direction`;
    /// distance infinite for an escaping ray), for the path vertex at `depth`: the radiance scattered towards
    /// the origin and the transmittance across the segment. `visible` gives the transmittance of a shadow
    /// ray up to a distance. Black and white when the segment misses the region.
    member _.VolumeSegment(origin: Point, direction: Vector, distance: float, time: float, key: uint64, depth: int,
                           visible: Ray -> float -> Colour) =
        match volume with
        | None -> struct (Colour(0., 0., 0.), Colour(1., 1., 1.))
        | Some v ->
            match Volume.clip v origin direction (min distance v.MaxDistance) with
            | ValueNone -> struct (Colour(0., 0., 0.), Colour(1., 1., 1.))
            | ValueSome (struct (enter, leave)) ->
                let key = Sampling.mixKey (key ^^^ (uint64 depth * 0x632BE59BD9B4E019UL + 0x3C6EF372FE94F82BUL))
                scatter v origin direction enter leave time key (depth <= v.ScatterDepth) visible

    /// `radiance` arriving from `distance` along the segment, seen through the bounded volume.
    member this.ApplyVolume(origin: Point, direction: Vector, distance: float, time: float, key: uint64, depth: int,
                            visible: Ray -> float -> Colour, radiance: Colour) =
        let struct (scattered, transmittance) = this.VolumeSegment(origin, direction, distance, time, key, depth, visible)
        Colour(radiance.R * transmittance.R + scattered.R,
               radiance.G * transmittance.G + scattered.G,
               radiance.B * transmittance.B + scattered.B)

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
