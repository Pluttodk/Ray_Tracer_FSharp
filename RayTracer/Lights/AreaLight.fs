namespace Tracer.Basics

open System
open Tracer.Basics.Sampling
open Tracer.BaseShape

[<Struct>]
type SurfaceLightSample =
    { Point: Point
      Normal: Vector
      AreaPdf: float }

[<AbstractClass>]
type AreaLight(surfaceMaterial: Material, sampler: Sampler) =
    inherit Light(Colour.White, 1.)
    let texture = Textures.mkMatTexture surfaceMaterial
    let radiance =
        match surfaceMaterial with
        | :? EmissiveMaterial as material -> material.EmisiveRadience
        | _ -> invalidArg (nameof surfaceMaterial) "An area light requires an emissive material."
    member _.Texture = texture
    member _.SurfaceMaterial = surfaceMaterial
    member _.SampleCount = sampler.SampleCount
    member _.SampleSetCount = sampler.SetCount
    member _.Sampler = sampler
    abstract member Shape: Shape
    abstract member SamplePoint: Point -> Point
    abstract member SamplePointNormal: Point -> Vector
    abstract member SampleSurface: Point * float * float -> SurfaceLightSample
    default this.SampleSurface(receiver, _, _) =
        let point = this.SamplePoint receiver
        { Point = point; Normal = this.SamplePointNormal point
          AreaPdf = this.GetProbabilityDensity(HitPoint(receiver)) }

    member this.SampleAt(hit: HitPoint, key: uint64, index: int) =
        let x, y = sampler.SampleAt(key, index)
        let surface = this.SampleSurface(hit.Point, x, y)
        if not (Double.IsFinite surface.AreaPdf) || surface.AreaPdf <= 0. then
            invalidOp "Area-light sampling returned an invalid density."
        let difference = surface.Point - hit.Point
        let distance = difference.Magnitude
        let direction = difference.Normalise
        let cosine = max 0. (surface.Normal.Normalise * -direction)
        { Direction = direction; Distance = distance; Radiance = radiance
          Weight = if distance > 0. then cosine / (distance * distance * surface.AreaPdf) else 0. }

    override _.GetColour _ = radiance
    override this.GetDirectionFromPoint hit = (this.SampleAt(hit, 0UL, 0)).Direction
    override this.GetShadowRay hit =
        Array.init sampler.SampleCount (fun index -> hit.SpawnRay((this.SampleAt(hit, 0UL, index)).Direction))
    override this.GetGeometricFactor hit =
        let sample = this.SampleAt(hit, 0UL, 0)
        let x, y = sampler.SampleAt(0UL, 0)
        sample.Weight * (this.SampleSurface(hit.Point, x, y)).AreaPdf
    override this.GetProbabilityDensity hit =
        let x, y = sampler.SampleAt(0UL, 0)
        (this.SampleSurface(hit.Point, x, y)).AreaPdf

type DiscAreaLight(surfaceMaterial: Material, disc: BaseDisc, sampler: Sampler) =
    inherit AreaLight(surfaceMaterial, sampler)
    let shape = disc.toShape (Textures.mkMatTexture surfaceMaterial)
    let pdf = 1. / (Math.PI * disc.radius * disc.radius)
    member _.DiscShape = shape :?> Disc
    override _.Shape = shape
    override _.SamplePointNormal _ = Vector(0., 0., 1.)
    override _.SampleSurface(_, x, y) =
        let x, y = mapToDisc(x, y)
        { Point = disc.center + Vector(x * disc.radius, y * disc.radius, 0.)
          Normal = Vector(0., 0., 1.); AreaPdf = pdf }
    override this.SamplePoint receiver =
        let x, y = sampler.Next()
        (this.SampleSurface(receiver, x, y)).Point

type RectangleAreaLight(surfaceMaterial: Material, rect: BaseRectangle, sampler: Sampler) =
    inherit AreaLight(surfaceMaterial, sampler)
    let shape = rect.toShape (Textures.mkMatTexture surfaceMaterial)
    let edgeU = rect.bottomRight - rect.bottomLeft
    let edgeV = rect.topLeft - rect.bottomLeft
    let cross = edgeU % edgeV
    do
        if not (Double.IsFinite cross.Magnitude) || cross.Magnitude <= 0. then
            invalidArg (nameof rect) "Area-light edges must span a nonzero finite area."
    let normal = cross.Normalise
    let pdf = 1. / cross.Magnitude
    member _.RectangleShape = shape :?> Rectangle
    override _.Shape = shape
    override _.SamplePointNormal _ = normal
    override _.SampleSurface(_, x, y) =
        { Point = rect.bottomLeft + x * edgeU + y * edgeV; Normal = normal; AreaPdf = pdf }
    override this.SamplePoint receiver =
        let x, y = sampler.Next()
        (this.SampleSurface(receiver, x, y)).Point

type SphereAreaLight(surfaceMaterial: Material, sphere: BaseShape, sampler: Sampler) =
    inherit AreaLight(surfaceMaterial, sampler)
    let shape = sphere.toShape (Textures.mkMatTexture surfaceMaterial)
    let sphereShape =
        match shape with
        | :? SphereShape as value -> value
        | _ -> invalidArg (nameof sphere) "A sphere area light requires a sphere base shape."
    let pdf = 1. / (4. * Math.PI * sphereShape.radius * sphereShape.radius)
    member _.SphereShape = sphereShape
    override _.Shape = shape
    override _.SamplePointNormal point = (point - sphereShape.origin).Normalise
    override _.SampleSurface(_, x, y) =
        let z = 1. - 2. * x
        let r = sqrt (max 0. (1. - z * z))
        let phi = 2. * Math.PI * y
        let normal = Vector(r * Math.Cos phi, r * Math.Sin phi, z)
        { Point = sphereShape.origin + sphereShape.radius * normal; Normal = normal; AreaPdf = pdf }
    override this.SamplePoint receiver =
        let x, y = sampler.Next()
        (this.SampleSurface(receiver, x, y)).Point

module TransformLight =
    let transformDirectionalLight (light: DirectionalLight, transformation) =
        Transformation.transformVector(light.Direction, Transformation.getMatrix transformation)

    let transformPointLight (light: PointLight, transformation) =
        Transformation.transformPoint(light.Position, Transformation.getMatrix transformation)

    let transformLight (light: Light) transformation =
        match light with
        | :? DirectionalLight as directional ->
            DirectionalLight(directional.BaseColour, directional.Intensity, transformDirectionalLight(directional, transformation)) :> Light
        | :? PointLight as point ->
            PointLight(point.BaseColour, point.Intensity, transformPointLight(point, transformation)) :> Light
        | :? AreaLight as area ->
            let matrix = Transformation.getMatrix transformation
            let inverse = Transformation.getInvMatrix transformation
            let determinant =
                matrix.Pos1x1 * (matrix.Pos2x2 * matrix.Pos3x3 - matrix.Pos2x3 * matrix.Pos3x2)
                - matrix.Pos1x2 * (matrix.Pos2x1 * matrix.Pos3x3 - matrix.Pos2x3 * matrix.Pos3x1)
                + matrix.Pos1x3 * (matrix.Pos2x1 * matrix.Pos3x2 - matrix.Pos2x2 * matrix.Pos3x1)
            let shape = Transform.transform area.Shape transformation
            { new AreaLight(area.SurfaceMaterial, area.Sampler) with
                member _.Shape = shape
                member _.SampleSurface(receiver, x, y) =
                    let localReceiver = Transformation.transformPoint(receiver, inverse)
                    let local = area.SampleSurface(localReceiver, x, y)
                    let transformedNormal = Transform.transformNormal local.Normal transformation
                    // A surface element scales by |det(M)| * |M^-T n| for a unit normal.
                    let areaScale = abs determinant * transformedNormal.Magnitude / local.Normal.Magnitude
                    if not (Double.IsFinite areaScale) || areaScale <= 0. then
                        invalidOp "Area-light transformation has an invalid surface-area scale."
                    { Point = Transformation.transformPoint(local.Point, matrix)
                      Normal = transformedNormal.Normalise; AreaPdf = local.AreaPdf / areaScale }
                member this.SamplePoint receiver =
                    let x, y = area.Sampler.Next()
                    (this.SampleSurface(receiver, x, y)).Point
                member _.SamplePointNormal point =
                    let localPoint = Transformation.transformPoint(point, inverse)
                    (Transform.transformNormal (area.SamplePointNormal localPoint) transformation).Normalise } :> Light
        | _ -> invalidArg (nameof light) "Only point, directional, and area lights support geometric transformations."

module LightSampling =
    let sampleCount (light: Light) =
        match light with
        | :? AreaLight as area -> area.SampleCount
        | :? EnvironmentLight as environment -> environment.Sampler.SampleCount
        | _ -> 1

    let sampleAt (light: Light) (hit: HitPoint) key index =
        match light with
        | :? PointLight as point ->
            let difference = point.Position - hit.Point
            let distance = difference.Magnitude
            { Direction = difference.Normalise; Distance = distance; Radiance = point.GetColour hit
              Weight = if distance > 0. then 1. else 0. }
        | :? DirectionalLight as directional ->
            { Direction = directional.Direction; Distance = infinity; Radiance = directional.GetColour hit; Weight = 1. }
        | :? AreaLight as area -> area.SampleAt(hit, key, index)
        | :? EnvironmentLight as environment -> environment.SampleAt(hit, key, index)
        | :? AmbientLight -> invalidArg (nameof light) "Ambient lights must be supplied through the scene ambient-light parameter."
        | _ ->
            let pdf = light.GetProbabilityDensity hit
            if not (Double.IsFinite pdf) || pdf <= 0. then invalidOp "Light sampling returned an invalid density."
            { Direction = (light.GetDirectionFromPoint hit).Normalise; Distance = infinity
              Radiance = light.GetColour hit; Weight = light.GetGeometricFactor hit / pdf }
