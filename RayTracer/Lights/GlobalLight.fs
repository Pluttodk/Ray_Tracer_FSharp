namespace Tracer.Basics

open System
open Tracer.Basics.Sampling
open Tracer.Basics.Textures

[<Struct>]
type LightSample =
    { Direction: Vector
      Distance: float
      Radiance: Colour
      Weight: float }

/// Piecewise-constant density over the sphere of directions, tabulated on a lat-long grid
/// (PBRT's Distribution2D, but constant per unit *solid angle* inside a cell rather than per unit
/// (u,v), which keeps the density finite at the poles).
///
/// Row j spans polar angle [j, j+1] * pi/height measured from +Y; column i spans azimuth
/// [i, i+1] * 2pi/width with azimuth = atan2(X, Z), the lat-long layout EnvironmentLight uses for
/// its textures. Weights are per cell and need not be normalised. Every cell must have a positive
/// weight, so the density covers the whole sphere and stays unbiased for any radiance.
type EnvironmentDistribution(width: int, height: int, weights: float[]) =
    do
        if width < 1 || height < 1 || weights.Length <> width * height then
            invalidArg (nameof weights) "The distribution needs width*height cell weights."
        if weights |> Array.exists (fun w -> not (Double.IsFinite w) || w <= 0.) then
            invalidArg (nameof weights) "Every cell weight must be finite and positive."
    let cosineAt j = Math.Cos(Math.PI * float j / float height)
    let cellSolidAngle = Array.init height (fun j -> 2. * Math.PI / float width * (cosineAt j - cosineAt (j + 1)))
    // Per-row conditional CDFs (width+1 entries per row) and the marginal CDF over rows.
    let conditional = Array.zeroCreate<float> (height * (width + 1))
    let rowTotals = Array.zeroCreate<float> height
    do
        for j in 0 .. height - 1 do
            let offset = j * (width + 1)
            for i in 0 .. width - 1 do
                conditional.[offset + i + 1] <- conditional.[offset + i] + weights.[j * width + i]
            rowTotals.[j] <- conditional.[offset + width]
    let marginal = Array.scan (+) 0. rowTotals
    let total = marginal.[height]

    /// Index of the interval of the CDF `cdf.[offset .. offset+n]` that contains `value`.
    let search (cdf: float[]) offset n value =
        let mutable lo = 0
        let mutable hi = n - 1
        while lo < hi do
            let mid = (lo + hi + 1) / 2
            if cdf.[offset + mid] <= value then lo <- mid else hi <- mid - 1
        lo

    member _.Width = width
    member _.Height = height

    /// The density (per steradian) of `direction`.
    member _.Pdf(direction: Vector) =
        let d = direction.Normalise
        let polar = Math.Acos(max -1. (min 1. d.Y))
        let j = min (height - 1) (max 0 (int (polar / Math.PI * float height)))
        let azimuth = Math.Atan2(d.X, d.Z)
        let u = (if azimuth < 0. then azimuth + 2. * Math.PI else azimuth) / (2. * Math.PI)
        let i = min (width - 1) (max 0 (int (u * float width)))
        weights.[j * width + i] / total / cellSolidAngle.[j]

    /// Maps two uniform numbers to a direction and its density (per steradian).
    member _.Sample(u1: float, u2: float) =
        let target = u2 * total
        let j = search marginal 0 height target
        let rowFraction = Math.Clamp((target - marginal.[j]) / rowTotals.[j], 0., 1.)
        let offset = j * (width + 1)
        let rowTarget = u1 * rowTotals.[j]
        let i = search conditional offset width rowTarget
        let weight = weights.[j * width + i]
        let columnFraction = Math.Clamp((rowTarget - conditional.[offset + i]) / weight, 0., 1.)
        // Uniform in azimuth and in cos(polar) inside the cell, i.e. constant per steradian.
        let azimuth = 2. * Math.PI * (float i + columnFraction) / float width
        let c0, c1 = cosineAt j, cosineAt (j + 1)
        let cosine = c0 + rowFraction * (c1 - c0)
        let sine = sqrt (max 0. (1. - cosine * cosine))
        let direction = Vector(sine * Math.Sin azimuth, cosine, sine * Math.Cos azimuth)
        struct (direction, weight / total / cellSolidAngle.[j])

    /// Tabulates `luminance` (2x2 supersampled per cell) times the cell's solid angle. A floor of
    /// a small fraction of the mean keeps every direction sampleable.
    static member OfLuminance(width: int, height: int, luminance: Vector -> float) =
        let cosineAt j = Math.Cos(Math.PI * float j / float height)
        let raw =
            Array.Parallel.init (width * height) (fun index ->
                let j, i = index / width, index % width
                let c0, c1 = cosineAt j, cosineAt (j + 1)
                let mutable sum = 0.
                for sj in 0 .. 1 do
                    for si in 0 .. 1 do
                        let cosine = c0 + (float sj + 0.5) / 2. * (c1 - c0)
                        let sine = sqrt (max 0. (1. - cosine * cosine))
                        let azimuth = 2. * Math.PI * (float i + (float si + 0.5) / 2.) / float width
                        let value = luminance (Vector(sine * Math.Sin azimuth, cosine, sine * Math.Cos azimuth))
                        if Double.IsFinite value && value > 0. then sum <- sum + value
                sum / 4. * (2. * Math.PI / float width * (c0 - c1)))
        let mean = Array.average raw
        let floor = if mean > 0. then 1e-3 * mean else 1.
        EnvironmentDistribution(width, height, raw |> Array.map (fun w -> max w floor))

/// Light arriving from infinitely far away in every direction.
///
/// `radiance` is what the environment contributes to *lighting*: light sampling and diffuse or
/// glossy bounces. `visible` is what camera rays and perfectly specular chains see when they
/// escape; it defaults to `radiance`. The two differ when part of the environment is carried by
/// another light: an analytic sky draws its sun disc only in `visible`, because a DirectionalLight
/// already carries the sun's lighting (see Sky.fs).
///
/// With `importanceWidth` > 0, light sampling draws directions from a luminance table of that
/// width (and half that height), mixed with cosine sampling about the normal. With 0 it
/// cosine-samples the hemisphere, as it always did.
///
/// A prebuilt `table` (for example one tabulated straight from an HDR image's pixels, see
/// HdrEnvironment) takes the place of the luminance table and turns importance sampling on.
type EnvironmentLight
    (radius: float, texture: Texture, sampler: Sampler,
     radiance: (Vector -> Colour) option, visible: (Vector -> Colour) option, importanceWidth: int,
     table: EnvironmentDistribution option) =
    inherit Light(Colour.Black, 1.)
    do
        if not (Double.IsFinite radius) || radius <= 0. then
            invalidArg (nameof radius) "Environment radius must be finite and positive."
        if importanceWidth < 0 || importanceWidth = 1 then
            invalidArg (nameof importanceWidth) "The importance table width must be 0 (off) or at least 2."
    let sphere = SphereShape(Point.Zero, radius, texture)

    let textureRadiance (direction: Vector) =
        let direction = direction.Normalise
        let longitude = Math.Atan2(direction.X, direction.Z) / (2. * Math.PI)
        let u = if longitude < 0. then longitude + 1. else longitude
        let v = 1. - Math.Acos(max -1. (min 1. direction.Y)) / Math.PI
        match Textures.getFunc texture u v with
        | :? EmissiveMaterial as material -> material.EmisiveRadience
        | _ -> invalidOp "Environment textures must return emissive materials."

    let radianceOf = defaultArg radiance textureRadiance
    let visibleOf = defaultArg visible radianceOf

    let distribution =
        lazy (
            if table.IsSome then table
            elif importanceWidth = 0 then None
            else
                let luminance (d: Vector) = let c = radianceOf d in 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B
                Some (EnvironmentDistribution.OfLuminance(importanceWidth, max 1 (importanceWidth / 2), luminance)))

    /// Share of light samples drawn from the luminance table. The rest cosine-sample about the
    /// normal, which bounds the variance where the table fits badly (faces turned from the sun).
    static let tableShare = 0.75

    new(radius, texture, sampler, radiance, visible, importanceWidth) =
        EnvironmentLight(radius, texture, sampler, radiance, visible, importanceWidth, None)
    new(radius, texture, sampler) = EnvironmentLight(radius, texture, sampler, None, None, 0, None)

    member _.Radius = radius
    member _.Texture = texture
    member _.Sampler = sampler
    member _.Sphere = sphere
    member _.IsImportanceSampled = importanceWidth > 0 || table.IsSome
    member _.Distribution = distribution.Value

    member _.Radiance(direction: Vector) = radianceOf direction
    member _.Visible(direction: Vector) = visibleOf direction

    /// Density per steradian with which `SampleDirection` picks `direction` at a surface whose
    /// shading normal is `normal`.
    member _.Pdf(normal: Vector, direction: Vector) =
        let cosine = max 0. (direction.Normalise * normal)
        match distribution.Value with
        | None -> cosine / Math.PI
        | Some table -> tableShare * table.Pdf direction + (1. - tableShare) * cosine / Math.PI

    /// A light-sampling direction from two uniform numbers, and its density per steradian. With
    /// the table in use the direction may lie below the surface; such samples contribute nothing.
    member this.SampleDirection(normal: Vector, u1: float, u2: float) =
        match distribution.Value with
        | None ->
            let x, y, z = mapToHemisphere (u1, u2) 1.
            let u, v, w = SurfaceSampling.frame normal
            let direction = (x * u + y * v + z * w).Normalise
            struct (direction, max 0. (direction * normal) / Math.PI)
        | Some table ->
            let direction =
                if u1 < tableShare then
                    let struct (direction, _) = table.Sample(u1 / tableShare, u2)
                    direction
                else
                    let x, y, z = mapToHemisphere ((u1 - tableShare) / (1. - tableShare), u2) 1.
                    let u, v, w = SurfaceSampling.frame normal
                    (x * u + y * v + z * w).Normalise
            struct (direction, this.Pdf(normal, direction))

    member this.SampleAt(hit: HitPoint, key: uint64, index: int) =
        let u1, u2 = sampler.SampleAt(key, index)
        let struct (direction, pdf) = this.SampleDirection(hit.Normal, u1, u2)
        let cosine = direction * hit.Normal
        { Direction = direction; Distance = infinity; Radiance = this.Radiance direction
          Weight = if cosine > 0. && pdf > 0. then 1. / pdf else 0. }

    member _.FlushDirections(_: HitPoint) = ()
    override _.GetGeometricFactor _ = 1.
    override this.GetDirectionFromPoint hit = (this.SampleAt(hit, 0UL, 0)).Direction
    override this.GetProbabilityDensity hit =
        let weight = (this.SampleAt(hit, 0UL, 0)).Weight
        if weight > 0. then 1. / weight else 1.
    override this.GetColour hit = (this.SampleAt(hit, 0UL, 0)).Radiance
    override this.GetShadowRay hit =
        Array.init sampler.SampleCount (fun index -> hit.SpawnRay((this.SampleAt(hit, 0UL, index)).Direction))

type AmbientOccluder(intensity: float, colour: Colour, minIntensity: float, sampler: Sampler) =
    inherit AmbientLight(colour, intensity)
    do
        SurfaceSampling.validateCoefficient (nameof intensity) intensity
        if not (Double.IsFinite minIntensity) || minIntensity < 0. || minIntensity > 1. then
            invalidArg (nameof minIntensity) "Minimum occlusion intensity must be in [0,1]."
    member _.Intensity = intensity
    member _.MinIntensity = minIntensity
    member _.MinIntensityColour = minIntensity * colour * intensity
    member _.Colour = colour * intensity
    member _.Sampler = sampler
