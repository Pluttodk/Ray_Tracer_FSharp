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
