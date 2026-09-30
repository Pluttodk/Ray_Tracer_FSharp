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

type EnvironmentLight(radius: float, texture: Texture, sampler: Sampler) =
    inherit Light(Colour.Black, 1.)
    do
        if not (Double.IsFinite radius) || radius <= 0. then
            invalidArg (nameof radius) "Environment radius must be finite and positive."
    let sphere = SphereShape(Point.Zero, radius, texture)
    member _.Radius = radius
    member _.Texture = texture
    member _.Sampler = sampler
    member _.Sphere = sphere

    member _.Radiance(direction: Vector) =
        let direction = direction.Normalise
        let longitude = Math.Atan2(direction.X, direction.Z) / (2. * Math.PI)
        let u = if longitude < 0. then longitude + 1. else longitude
        let v = 1. - Math.Acos(max -1. (min 1. direction.Y)) / Math.PI
        match Textures.getFunc texture u v with
        | :? EmissiveMaterial as material -> material.EmisiveRadience
        | _ -> invalidOp "Environment textures must return emissive materials."

    member this.SampleAt(hit: HitPoint, key: uint64, index: int) =
        let x, y, z = mapToHemisphere (sampler.SampleAt(key, index)) 1.
        let u, v, w = SurfaceSampling.frame hit.Normal
        let direction = (x * u + y * v + z * w).Normalise
        let cosine = max 0. (direction * hit.Normal)
        { Direction = direction; Distance = infinity; Radiance = this.Radiance direction
          Weight = if cosine > 0. then Math.PI / cosine else 0. }

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
