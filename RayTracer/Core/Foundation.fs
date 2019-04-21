namespace Tracer.Basics
open System
open Tracer.Basics.Sampling
open System.Numerics

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
and HitPoint(ray: Ray, time: float, normal: Vector, material: Material, shape: Shape, u: float, v:float, didHit: bool) = 
    
    // Ray that hit
    member this.Ray: Ray = ray

    // t at which the ray hit
    member this.Time: float = time

    // Point at which the ray hit
    member this.Point: Point = ray.PointAtTime time

    // Point at which the ray hit, a little above the surface (for reflected rays)
    member this.EscapedPoint: Point = this.Point + normal * 0.000001

    // Point at which the ray hit, a little below the surface (for refracted rays)
    member this.InnerEscapedPoint: Point = this.Point - normal * 0.000001

    // True if this point hit an appropriate shape
    member this.DidHit = didHit

    // Normal at the point where the ray did hit
    member this.Normal = if ray.GetDirection * normal > 0. then -normal else normal

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

    // Constructors for rays that hit
    new(ray: Ray, time:float, normal: Vector, material: Material, shape: Shape) = HitPoint(ray, time, normal, material, shape, 0., 0., true)
    new(ray: Ray, time:float, normal:Vector, material:Material, shape:Shape, u:float, v:float) = HitPoint(ray, time, normal, material, shape, u, v, true)

    // Constructors for rays that did not hit (to be refactored to Some(...) and None)
    new(ray: Ray) = HitPoint(ray, -0., new Vector(0.,0.,0.), Material.None, Shape.None, 0., 0., false)
    new(point: Point) = HitPoint(Ray.None, -0., point.ToVector, Material.None, Shape.None, 0., 0., false)

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
    abstract member hitFunction: Ray -> HitPoint
    static member None = BlankShape() :> Shape

and BlankShape() = 
    inherit Shape()
    override this.isInside (p:Point) = failwith "cannot be inside a blank shape"
    override this.getBoundingBox () = failwith "cannot get bounding box for a blank shape"
    default this.hitFunction r = HitPoint(r)