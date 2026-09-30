namespace Tracer.Basics

open System
open Tracer.Basics.Sampling

module SurfaceSampling =
    let frame (direction: Vector) =
        let w = direction.Normalise
        if w.Magnitude = 0. || not (Double.IsFinite w.X && Double.IsFinite w.Y && Double.IsFinite w.Z) then
            invalidArg (nameof direction) "A sampling frame requires a finite nonzero direction."
        let up = if abs w.Y > 0.999 then Vector(1., 0., 0.) else Vector(0., 1., 0.)
        let v = (up % w).Normalise
        let u = w % v
        u, v, w

    let perfect (hit: HitPoint) =
        let incoming = hit.Ray.GetDirection.Normalise
        (incoming - 2. * (incoming * hit.Normal) * hit.Normal).Normalise

    let glossy (hit: HitPoint) exponent (samples: (float * float) array) =
        let u, v, w = frame (perfect hit)
        samples
        |> Array.map (fun sample ->
            let x, y, z = mapToHemisphere sample (float exponent)
            let candidate = x * u + y * v + z * w
            let direction =
                if candidate * hit.Normal > 0. then candidate
                else -x * u - y * v + z * w
            hit.SpawnRay(direction))

    let dielectric (incoming: Vector) (normal: Vector) etaI etaT =
        if not (Double.IsFinite etaI && Double.IsFinite etaT) || etaI <= 0. || etaT <= 0. then
            invalidArg "ior" "Refractive indices must be finite and positive."
        let direction = incoming.Normalise
        let cosine = max 0. (min 1. (-direction * normal))
        let eta = etaI / etaT
        let discriminant = 1. - eta * eta * max 0. (1. - cosine * cosine)
        if etaI = etaT then 0., Some direction
        elif discriminant <= 0. then 1., None
        else
            let transmittedCosine = sqrt discriminant
            let parallel = (etaT * cosine - etaI * transmittedCosine) / (etaT * cosine + etaI * transmittedCosine)
            let perpendicular = (etaI * cosine - etaT * transmittedCosine) / (etaI * cosine + etaT * transmittedCosine)
            let fresnel = 0.5 * (parallel * parallel + perpendicular * perpendicular)
            let transmitted = (eta * direction + (eta * cosine - transmittedCosine) * normal).Normalise
            fresnel, Some transmitted

    let validateCoefficient name value =
        if not (Double.IsFinite value) || value < 0. then
            invalidArg name "Material coefficients must be finite and nonnegative."

type MatteMaterial(ambientColour: Colour, ambientCoefficient: float, matteColour: Colour, matteCoefficient: float) =
    inherit Material()

    do
        SurfaceSampling.validateCoefficient (nameof ambientCoefficient) ambientCoefficient
        SurfaceSampling.validateCoefficient (nameof matteCoefficient) matteCoefficient

    let ambient = ambientColour * ambientCoefficient
    let diffuse = matteColour * (matteCoefficient / Math.PI)

    member _.MatteCoefficient = matteCoefficient
    member _.MatteColour = matteColour
    member _.DiffuseReflectance = diffuse

    default _.AmbientColour(hit, light) = ambient * light.GetColour hit
    default _.ReflectionFactor(_, _) = Colour.White
    default _.BounceMethod _ = [||]
    default _.IsRecursive = false
    default _.Bounce(_, hit, light) =
        let cosine = max 0. (hit.Normal * light.GetDirectionFromPoint hit)
        if cosine = 0. then Colour.Black
        else
            let pdf = light.GetProbabilityDensity hit
            if not (Double.IsFinite pdf) || pdf <= 0. then
                invalidOp "A contributing light sample must have a positive finite PDF."
            diffuse * light.GetColour hit * (cosine * light.GetGeometricFactor hit / pdf)

type PhongMaterial
    (ambientColour: Colour, ambientCoefficient: float, matteColour: Colour, matteCoefficient: float,
     specularColour: Colour, specularCoefficient: float, specularExponent: int) =
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)

    do
        SurfaceSampling.validateCoefficient (nameof specularCoefficient) specularCoefficient
        if specularExponent < 0 then invalidArg (nameof specularExponent) "The specular exponent must be nonnegative."

    member _.SpecularCoefficient = specularCoefficient
    member _.SpecularColour = specularColour
    member _.SpecularExponent = specularExponent

    default this.Bounce(_, hit, light) =
        let direction = light.GetDirectionFromPoint hit
        let cosine = max 0. (hit.Normal * direction)
        if cosine = 0. then Colour.Black
        else
            let reflected = -direction + 2. * cosine * hit.Normal
            let specularCosine = max 0. (reflected * -hit.Ray.GetDirection.Normalise)
            let specular = specularColour * (specularCoefficient * pown specularCosine specularExponent)
            let pdf = light.GetProbabilityDensity hit
            if not (Double.IsFinite pdf) || pdf <= 0. then
                invalidOp "A contributing light sample must have a positive finite PDF."
            (this.DiffuseReflectance + specular) * light.GetColour hit * (cosine * light.GetGeometricFactor hit / pdf)

type RayReflector =
    static member Perfect(hit: HitPoint) = SurfaceSampling.perfect hit

type MatteReflectiveMaterial
    (ambientColour: Colour, ambientCoefficient: float, matteColour: Colour, matteCoefficient: float,
     reflectionColour: Colour, reflectionCoefficient: float) =
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)

    do SurfaceSampling.validateCoefficient (nameof reflectionCoefficient) reflectionCoefficient
    let reflectance = reflectionColour * reflectionCoefficient
    member _.ReflectionColour = reflectionColour
    member _.ReflectionCoefficient = reflectionCoefficient
    default _.ReflectionFactor(_, _) = reflectance
    default _.IsRecursive = true
    default _.BounceMethod hit = [| hit.SpawnRay(SurfaceSampling.perfect hit) |]

type PhongReflectiveMaterial
    (ambientColour: Colour, ambientCoefficient: float, matteColour: Colour, matteCoefficient: float,
     specularColour: Colour, specularCoefficient: float, reflectionColour: Colour,
     reflectionCoefficient: float, specularExponent: int) =
    inherit PhongMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient, specularColour, specularCoefficient, specularExponent)

    do SurfaceSampling.validateCoefficient (nameof reflectionCoefficient) reflectionCoefficient
    let reflectance = reflectionColour * reflectionCoefficient
    member _.ReflectionColour = reflectionColour
    member _.ReflectionCoefficient = reflectionCoefficient
    default _.ReflectionFactor(_, _) = reflectance
    default _.IsRecursive = true
    default _.BounceMethod hit = [| hit.SpawnRay(SurfaceSampling.perfect hit) |]

type MatteGlossyReflectiveMaterial
    (ambientColour: Colour, ambientCoefficient: float, matteColour: Colour, matteCoefficient: float,
     reflectiveColour: Colour, glossyCoefficient: float, glossyExponent: int, sampler: Sampler) =
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)

    do
        SurfaceSampling.validateCoefficient (nameof glossyCoefficient) glossyCoefficient
        if glossyExponent < 0 then invalidArg (nameof glossyExponent) "The glossy exponent must be nonnegative."
    let reflectance = reflectiveColour * glossyCoefficient
    member _.Sampler = sampler
    member _.GlossyExponent = glossyExponent
    member _.ReflectionColour = reflectiveColour
    member _.ReflectionCoefficient = glossyCoefficient
    member _.BounceMethodAt(hit, key) = SurfaceSampling.glossy hit glossyExponent (sampler.SampleSetAt key)
    default _.ReflectionFactor(_, _) = reflectance
    default _.IsRecursive = true
    default this.BounceMethod hit = this.BounceMethodAt(hit, 0UL)

type PhongGlossyReflectiveMaterial
    (ambientColour: Colour, ambientCoefficient: float, matteColour: Colour, matteCoefficient: float,
     specularColour: Colour, specularCoefficient: float, reflectiveColour: Colour,
     glossyCoefficient: float, specularExponent: int, glossyExponent: int, sampler: Sampler) =
    inherit PhongMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient, specularColour, specularCoefficient, specularExponent)

    do
        SurfaceSampling.validateCoefficient (nameof glossyCoefficient) glossyCoefficient
        if glossyExponent < 0 then invalidArg (nameof glossyExponent) "The glossy exponent must be nonnegative."
    let reflectance = reflectiveColour * glossyCoefficient
    member _.Sampler = sampler
    member _.GlossyExponent = glossyExponent
    member _.ReflectionColour = reflectiveColour
    member _.ReflectionCoefficient = glossyCoefficient
    member _.BounceMethodAt(hit, key) = SurfaceSampling.glossy hit glossyExponent (sampler.SampleSetAt key)
    default _.ReflectionFactor(_, _) = reflectance
    default _.IsRecursive = true
    default this.BounceMethod hit = this.BounceMethodAt(hit, 0UL)

type EmissiveMaterial(lightColour: Colour, lightIntensity: float) =
    inherit Material()
    do SurfaceSampling.validateCoefficient (nameof lightIntensity) lightIntensity
    let radiance = lightColour * lightIntensity
    member _.LightColour = lightColour
    member _.LightIntensity = lightIntensity
    member _.EmisiveRadience = radiance
    default _.AmbientColour(_, _) = Colour.Black
    default _.IsRecursive = false
    default _.ReflectionFactor(_, _) = Colour.White
    default _.BounceMethod _ = [||]
    default _.Bounce(_, hit, _) = if hit.FrontFace then radiance else Colour.Black

type TransparentRay(origin: Point, direction: Vector, refracted: bool, isInside: bool) =
    inherit Ray(origin, direction)
    member _.Refracted = refracted
    member _.IsInside = isInside

type TransparentMaterial
    (innerFilterColour: Colour, outerFilterColour: Colour, innerRefractionIndex: float, outerRefractionIndex: float) =
    inherit Material()

    do
        if not (Double.IsFinite innerRefractionIndex && Double.IsFinite outerRefractionIndex)
           || innerRefractionIndex <= 0. || outerRefractionIndex <= 0. then
            invalidArg "ior" "Refractive indices must be finite and positive."
        for filter in [ innerFilterColour; outerFilterColour ] do
            if filter.R > 1. || filter.G > 1. || filter.B > 1. then
                invalidArg "filter" "Transmission filters must be in [0,1]."

    member _.InnerFilterColour = innerFilterColour
    member _.OuterFilterColour = outerFilterColour
    member _.InnerRefractionIndex = innerRefractionIndex
    member _.OuterRefractionIndex = outerRefractionIndex
    member _.Indices(hit: HitPoint) =
        if hit.FrontFace then innerRefractionIndex, outerRefractionIndex
        else outerRefractionIndex, innerRefractionIndex

    member this.ShouldRefract(hit: HitPoint) =
        let etaT, etaI = this.Indices hit
        let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
        let cosine = max 0. (min 1. (-hit.Ray.GetDirection.Normalise * normal))
        let discriminant = 1. - (etaI / etaT) ** 2. * max 0. (1. - cosine * cosine)
        discriminant > 0. || etaI = etaT, cosine, discriminant

    member this.RefractRay(hit: HitPoint) (_, _) =
        let etaT, etaI = this.Indices hit
        let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
        match SurfaceSampling.dielectric hit.Ray.GetDirection normal etaI etaT with
        | _, Some direction -> TransparentRay(hit.OffsetPoint direction, direction, true, hit.FrontFace)
        | _, None -> invalidOp "Total internal reflection has no refracted ray."

    default _.IsRecursive = true
    default _.AmbientColour(_, _) = Colour.Black
    default _.Bounce(_, _, _) = Colour.Black
    default this.ReflectionFactor(hit, outgoing) =
        let etaT, etaI = this.Indices hit
        let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
        let fresnel, transmitted = SurfaceSampling.dielectric hit.Ray.GetDirection normal etaI etaT
        if outgoing.GetDirection * normal < 0. then
            if transmitted.IsSome then Colour.White * ((1. - fresnel) * (etaI / etaT) ** 2.)
            else Colour.Black
        else Colour.White * fresnel

    default this.BounceMethod hit =
        let etaT, etaI = this.Indices hit
        let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
        let incoming = hit.Ray.GetDirection.Normalise
        let reflection = (incoming - 2. * (incoming * normal) * normal).Normalise
        let reflected = TransparentRay(hit.OffsetPoint reflection, reflection, false, not hit.FrontFace) :> Ray
        match SurfaceSampling.dielectric incoming normal etaI etaT with
        | _, None -> [| reflected |]
        | _, Some direction ->
            [| reflected; TransparentRay(hit.OffsetPoint direction, direction, true, hit.FrontFace) :> Ray |]

[<Struct>]
type ScatteredRay =
    { Ray: Ray
      Weight: Colour
      Transmitted: bool }

module MaterialTransport =
    let emission (hit: HitPoint) =
        match hit.Material with
        | :? EmissiveMaterial as material when hit.FrontFace -> material.EmisiveRadience
        | _ -> Colour.Black

    let direct (hit: HitPoint) (direction: Vector) (radiance: Colour) weight =
        let cosine = max 0. (hit.Normal * direction)
        if cosine = 0. || weight = 0. then Colour.Black
        else
            match hit.Material with
            | :? PhongMaterial as material ->
                let reflection = -direction + 2. * cosine * hit.Normal
                let specularCosine = max 0. (reflection * -hit.Ray.GetDirection.Normalise)
                let specular = material.SpecularColour * (material.SpecularCoefficient * pown specularCosine material.SpecularExponent)
                (material.DiffuseReflectance + specular) * radiance * (cosine * weight)
            | :? MatteMaterial as material ->
                material.DiffuseReflectance * radiance * (cosine * weight)
            | :? TransparentMaterial | :? EmissiveMaterial -> Colour.Black
            | material ->
                let sample =
                    { new Light(radiance, 1.) with
                        member _.GetColour _ = radiance * weight
                        member _.GetDirectionFromPoint _ = direction
                        member _.GetShadowRay _ = [||]
                        member _.GetGeometricFactor _ = 1.
                        member _.GetProbabilityDensity _ = 1. }
                material.Bounce(hit.Shape, hit, sample)

    let scatter (hit: HitPoint) key etaI etaT =
        match hit.Material with
        | :? TransparentMaterial ->
            let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
            let incoming = hit.Ray.GetDirection.Normalise
            let fresnel, transmitted = SurfaceSampling.dielectric incoming normal etaI etaT
            let reflection = incoming - 2. * (incoming * normal) * normal
            [| if fresnel > 0. then
                   yield { Ray = hit.SpawnRay reflection; Weight = Colour.White * fresnel; Transmitted = false }
               match transmitted with
               | Some direction when fresnel < 1. ->
                   yield { Ray = hit.SpawnRay direction
                           Weight = Colour.White * ((1. - fresnel) * (etaI / etaT) ** 2.)
                           Transmitted = true }
               | _ -> () |]
        | :? MatteGlossyReflectiveMaterial as material ->
            let rays = material.BounceMethodAt(hit, key)
            let weight = material.ReflectionFactor(hit, hit.Ray) / float rays.Length
            rays |> Array.map (fun ray -> { Ray = ray; Weight = weight; Transmitted = false })
        | :? PhongGlossyReflectiveMaterial as material ->
            let rays = material.BounceMethodAt(hit, key)
            let weight = material.ReflectionFactor(hit, hit.Ray) / float rays.Length
            rays |> Array.map (fun ray -> { Ray = ray; Weight = weight; Transmitted = false })
        | material when material.IsRecursive ->
            material.BounceMethod hit
            |> Array.map (fun ray -> { Ray = ray; Weight = material.ReflectionFactor(hit, ray); Transmitted = false })
        | _ -> [||]

    let absorption (filter: Colour) distance =
        if not (Double.IsFinite distance) || distance < 0. then
            invalidArg (nameof distance) "Absorption distance must be finite and nonnegative."
        Colour(filter.R ** distance, filter.G ** distance, filter.B ** distance)
