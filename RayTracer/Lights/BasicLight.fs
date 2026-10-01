namespace Tracer.Basics

open System

type PointLight(colour: Colour, intensity: float, position: Point) =
    inherit Light(colour, intensity)
    do SurfaceSampling.validateCoefficient (nameof intensity) intensity
    do
        if not (Double.IsFinite position.X && Double.IsFinite position.Y && Double.IsFinite position.Z) then
            invalidArg (nameof position) "Light position must be finite."
    let radiance = colour * intensity
    member _.Position = position
    override _.GetColour _ = radiance
    override _.GetDirectionFromPoint hit = (position - hit.Point).Normalise
    override _.GetShadowRay hit = [| hit.SpawnRay(position - hit.Point) |]
    override _.GetGeometricFactor _ = 1.
    override _.GetProbabilityDensity _ = 1.

type DirectionalLight(colour: Colour, intensity: float, direction: Vector) =
    inherit Light(colour, intensity)
    do SurfaceSampling.validateCoefficient (nameof intensity) intensity
    let direction = direction.Normalise
    do
        if direction.Magnitude = 0. || not (Double.IsFinite direction.X && Double.IsFinite direction.Y && Double.IsFinite direction.Z) then
            invalidArg (nameof direction) "Light direction must be finite and nonzero."
    let radiance = colour * intensity
    member _.Direction = direction
    override _.GetColour _ = radiance
    override _.GetDirectionFromPoint _ = direction
    override _.GetShadowRay hit = [| hit.SpawnRay direction |]
    override _.GetGeometricFactor _ = 1.
    override _.GetProbabilityDensity _ = 1.

/// A lamp with physical inverse-square falloff (PointLight is unattenuated).
///
/// `colour * intensity` is the radiant intensity in render units per steradian: a surface facing
/// the lamp at distance d receives irradiance colour * intensity / d^2, and the lamp's power is
/// 4 pi * intensity * colour. With `radius` = 0 it is a point; with `radius` > 0 it is a uniformly
/// emitting sphere of radiance colour * intensity / (pi radius^2) (the same intensity seen from
/// outside), sampled uniformly over the cone it subtends, which gives soft shadows.
///
/// It is not geometry: camera and BSDF rays pass through it, and it lights the scene only through
/// light sampling, so the path tracer never needs MIS for it. `samples` is how many shadow rays
/// the classic integrator takes per hit (the path tracer draws one per vertex).
type SphereLight(colour: Colour, intensity: float, position: Point, radius: float, samples: int) =
    inherit Light(colour, intensity)
    do SurfaceSampling.validateCoefficient (nameof intensity) intensity
    do
        if not (Double.IsFinite position.X && Double.IsFinite position.Y && Double.IsFinite position.Z) then
            invalidArg (nameof position) "Light position must be finite."
        if not (Double.IsFinite radius) || radius < 0. then
            invalidArg (nameof radius) "Light radius must be finite and non-negative."
        if samples < 1 then invalidArg (nameof samples) "A sphere light needs at least one sample."
    let intensityColour = colour * intensity
    let surfaceRadiance = if radius > 0. then intensityColour * (1. / (Math.PI * radius * radius)) else Colour.Black

    new(colour, intensity, position, radius) =
        SphereLight(colour, intensity, position, radius, (if radius > 0. then 4 else 1))
    new(colour, intensity, position) = SphereLight(colour, intensity, position, 0., 1)

    member _.Position = position
    member _.Radius = radius
    member _.SampleCount = samples
    /// colour * intensity: radiant intensity per steradian.
    member _.RadiantIntensity = intensityColour
    /// Luminance of the emitted power (4 pi times the intensity), for light selection.
    member _.Power = 4. * Math.PI * intensity * (0.2126 * colour.R + 0.7152 * colour.G + 0.0722 * colour.B)

    /// A light sample towards the lamp from `point`, from two uniform numbers. Weight is the
    /// reciprocal solid-angle density (1 for the point case, whose falloff is in Radiance).
    member _.Sample(point: Point, u1: float, u2: float) =
        let difference = position - point
        let distanceSquared = difference.MagnitudeSquared
        let distance = sqrt distanceSquared
        if distance <= 0. then
            { Direction = Vector(0., 1., 0.); Distance = 0.; Radiance = Colour.Black; Weight = 0. }
        elif radius <= 0. || distance <= radius * 1.0001 then
            // A point, or a receiver inside the bulb: no cone to sample, clamp the falloff.
            { Direction = difference * (1. / distance); Distance = distance
              Radiance = intensityColour * (1. / max distanceSquared (radius * radius)); Weight = 1. }
        else
            let w = difference * (1. / distance)
            let sinSquared = radius * radius / distanceSquared
            let cosMax = sqrt (max 0. (1. - sinSquared))
            // 1 - cos(theta_max) without cancellation for distant lamps.
            let oneMinusCosMax = sinSquared / (1. + cosMax)
            let cosine = 1. - u1 * oneMinusCosMax
            let sine = sqrt (max 0. (1. - cosine * cosine))
            let phi = 2. * Math.PI * u2
            let u, v, _ = SurfaceSampling.frame w
            let direction = (sine * Math.Cos phi * u + sine * Math.Sin phi * v + cosine * w).Normalise
            // Nearest intersection with the sphere along the sampled direction.
            let along = distance * cosine
            let discriminant = radius * radius - distanceSquared * sine * sine
            let toSurface = along - sqrt (max 0. discriminant)
            { Direction = direction; Distance = max 0. toSurface
              Radiance = surfaceRadiance; Weight = 2. * Math.PI * oneMinusCosMax }

    override _.GetColour _ = intensityColour
    override _.GetDirectionFromPoint hit = (position - hit.Point).Normalise
    override _.GetShadowRay hit = [| hit.SpawnRay(position - hit.Point) |]
    override _.GetGeometricFactor hit =
        let d2 = (position - hit.Point).MagnitudeSquared
        1. / max d2 (max (radius * radius) 1e-300)
    override _.GetProbabilityDensity _ = 1.
