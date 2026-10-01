namespace Tracer.Basics
open System
open Tracer.Basics.Sampling

exception LightException

//- MATERIAL
[<AbstractClass>]
type Material() = 
    
    // Get colour of this material from a hit
    abstract member Bounce: Shape * HitPoint * Light -> Colour

    // Get the reflected ray, if the material is recursive
    abstract member BounceMethod: HitPoint -> Ray[]

    // If enabled, on bounce, cast reflected rays returned by BounceMethod
    abstract member IsRecursive : bool

    // For recursive bounces, multiply the bounce colour by this colour
    abstract member ReflectionFactor : HitPoint * Ray -> Colour

    // Ambient colour
    abstract member AmbientColour : HitPoint * AmbientLight -> Colour
    
    // Reference material (to be removed)
    static member None = BlankMaterial()

// Reference material (To be removed)
and BlankMaterial() = 
    inherit Material()
    default this.AmbientColour(hitPoint, ambientLight) = Colour.Black
    default this.ReflectionFactor(hitPoint,rayOut) = Colour.White
    default this.Bounce(shape, hitPoint, light) = Colour.Black
    default this.BounceMethod hitPoint = [| hitPoint.Ray |]
    default this.IsRecursive = false
      
//- HITPOINT
and HitPoint(ray: Ray, time: float, geometricNormal: Vector, shadingNormal: Vector,
             material: Material, shape: Shape, u: float, v: float,
             barycentricBeta: float, barycentricGamma: float, didHit: bool) =
    do
        if didHit && (not geometricNormal.IsFinite || geometricNormal.IsZero) then
            invalidArg "geometricNormal" "A surface hit requires a finite, nonzero geometric normal."
    let geometric = geometricNormal.Normalise
    let shading =
        let n =
            if shadingNormal.IsFinite && not shadingNormal.IsZero then shadingNormal.Normalise
            else geometric
        if n * geometric < 0. then -n else n
    let frontFace = ray.GetDirection * geometric < 0.
    let normal = if ray.GetDirection * shading > 0. then -shading else shading
    let point = if didHit then ray.PointAtTime time else ray.GetOrigin
    let mutable shadowPoint = point
    let mutable hasShadowPoint = false
    let mutable tangent = Unchecked.defaultof<Vector>
    let mutable bitangent = Unchecked.defaultof<Vector>

    // Ray that hit
    member this.Ray: Ray = ray

    // t at which the ray hit
    member this.Time: float = time

    // Point at which the ray hit
    member this.Point: Point = point

    // Offset along the geometric normal on the outgoing ray's side, then round away from the surface.
    member this.OffsetPoint(outgoing: Vector): Point = this.OffsetFrom(point, outgoing)

    member private this.OffsetFrom(point: Point, outgoing: Vector): Point =
        let n = if outgoing * geometric >= 0. then geometric else -geometric
        let origin = ray.GetOrigin
        let axisError p o = max (abs p) (abs o)
        let errorScale =
            max (axisError point.X origin.X)
                (max (axisError point.Y origin.Y) (axisError point.Z origin.Z))
        let distance = 128. * 2.2204460492503131e-16 * errorScale
        let move value axisNormal =
            let shifted = value + distance * axisNormal
            if axisNormal > 0. then Math.BitIncrement shifted
            elif axisNormal < 0. then Math.BitDecrement shifted
            else value
        Point(move point.X n.X, move point.Y n.Y, move point.Z n.Z)

    /// Hanika shadow-terminator corrected point (defaults to Point). Set by smooth meshes.
    member this.ShadowPoint: Point = shadowPoint
    member this.WithShadowPoint(corrected: Point) =
        shadowPoint <- corrected
        hasShadowPoint <- true
        this

    /// Shadow-ray origin: the terminator-corrected point when the light is on the
    /// shading-normal side, otherwise the plain hit point, nudged off the surface.
    /// Surface tangent (direction of increasing U) and bitangent (increasing V) at the hit, unnormalised and
    /// not necessarily orthogonal to the shading normal; set by meshes that carry a tangent frame, so normal
    /// maps can be applied in the frame they were authored in. Absent (HasTangent = false) otherwise.
    member this.HasTangent = not (obj.ReferenceEquals(tangent, null))
    member this.Tangent: Vector = tangent
    member this.Bitangent: Vector = bitangent
    member this.WithTangent(t: Vector, b: Vector) =
        tangent <- t
        bitangent <- b
        this

    member this.ShadowOrigin(outgoing: Vector): Point =
        if hasShadowPoint && outgoing * shading > 0. then this.OffsetFrom(shadowPoint, outgoing)
        else this.OffsetPoint outgoing

    member this.SpawnRay(outgoing: Vector) =
        if not outgoing.IsFinite || outgoing.IsZero then
            invalidArg (nameof outgoing) "A spawned ray requires a finite, nonzero direction."
        let direction = outgoing.Normalise
        Ray(this.OffsetPoint direction, direction, ray.ShutterTime)

    // Point at which the ray hit, a little above the surface (for reflected rays)
    member this.EscapedPoint: Point = this.OffsetPoint(if frontFace then geometric else -geometric)

    // Point at which the ray hit, a little below the surface (for refracted rays)
    member this.InnerEscapedPoint: Point = this.OffsetPoint(if frontFace then -geometric else geometric)

    // True if this point hit an appropriate shape
    member this.DidHit = didHit

    // Normal at the point where the ray did hit
    member this.Normal = normal
    member this.GeometricNormal = geometric
    member this.ShadingNormal = shading
    member this.FrontFace = frontFace
    member this.BarycentricAlpha = 1. - barycentricBeta - barycentricGamma
    member this.BarycentricBeta = barycentricBeta
    member this.BarycentricGamma = barycentricGamma

    // Material at the point where the ray did hit
    member this.Material = material

    // U-coordinate for the hit (for texture mapping)
    member this.U = u

    // V-coordinate for the hit (for texture mapping)
    member this.V = v

    // [shortcut] UV-coordinates for the hit (for texture mapping)
    member this.UV = (u,v)

    // Shape at which the ray hit
    member this.Shape = shape

    member this.WithShape(newShape: Shape) =
        let copy = HitPoint(ray, time, geometric, shading, material, newShape, u, v,
                            barycentricBeta, barycentricGamma, didHit)
        let copy = if hasShadowPoint then copy.WithShadowPoint shadowPoint else copy
        if this.HasTangent then copy.WithTangent(tangent, bitangent) else copy

    // Constructors for rays that hit
    new(ray: Ray, time:float, normal:Vector, material:Material, shape:Shape, u:float, v:float, didHit:bool) =
        HitPoint(ray, time, normal, normal, material, shape, u, v, 0., 0., didHit)
    new(ray: Ray, time:float, normal: Vector, material: Material, shape: Shape) = HitPoint(ray, time, normal, material, shape, 0., 0., true)
    new(ray: Ray, time:float, normal:Vector, material:Material, shape:Shape, u:float, v:float) = HitPoint(ray, time, normal, material, shape, u, v, true)

    // Constructors for rays that did not hit (to be refactored to Some(...) and None)
    new(ray: Ray) = HitPoint(ray, -0., Vector.Zero, Material.None, Shape.None, 0., 0., false)
    new(point: Point) = HitPoint(Ray(point, Vector.Zero), -0., Vector.Zero, Material.None, Shape.None, 0., 0., false)

//- LIGHT
and [<AbstractClass>] Light(colour: Colour, intensity: float) =

    member this.BaseColour: Colour = colour     // Colour of the light
    member this.Intensity: float = intensity    // Intensity of the the light

    // (l_c) Final colour
    abstract member GetColour: HitPoint -> Colour

    // (l_d) Direction from a point to this light
    abstract member GetDirectionFromPoint: HitPoint -> Vector

    // (_ls) Shadow ray
    abstract member GetShadowRay: HitPoint -> Ray[]

    // (l_G) Geometric factor
    abstract member GetGeometricFactor: HitPoint -> float

    // (l_pdf) Probability density function
    abstract member GetProbabilityDensity: HitPoint -> float

//- AMBIENT LIGHT
and AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)

    default this.GetColour hitPoint = colour * intensity

    // Ambient lights only implements (l_c) colour
    override this.GetDirectionFromPoint hitPoint = raise LightException
    override this.GetShadowRay hitPoint = raise LightException
    override this.GetGeometricFactor hitPoint = raise LightException
    override this.GetProbabilityDensity hitPoint = raise LightException

//- SHAPE
and [<AbstractClass>] Shape() =
    abstract member isInside: Point -> bool
    abstract member getBoundingBox: unit -> BBox
    abstract member Bounds: BBox option
    default this.Bounds = Some(this.getBoundingBox())
    abstract member IsOpaque: bool
    default _.IsOpaque = false
    abstract member hitFunction: Ray -> HitPoint
    static member None = BlankShape() :> Shape

and BlankShape() = 
    inherit Shape()
    override _.Bounds = Some BBox.Empty
    override _.IsOpaque = true
    override this.isInside (p:Point) = failwith "cannot be inside a blank shape"
    override this.getBoundingBox () = failwith "cannot get bounding box for a blank shape"
    default this.hitFunction r = HitPoint(r)

/// Native intersection over the open ray-parameter interval (minimum, maximum).
/// Implementations retain the original ray and can select an exit after an excluded entry.
type IIntervalShape =
    abstract member HitWithin: Ray * minimum: float * maximum: float -> HitPoint