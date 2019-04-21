namespace Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Textures
open System

//- ENVIRONMENT LIGHT
type EnvironmentLight(radius: float, texture: Texture, sampler: Sampler) = 
    inherit Light(Colour.Black, 1.)
    
    // Private fields
    let sphere = SphereShape(Point.Zero, radius, texture)
    let mutable sps = []
    let mutable ld = Vector.Zero
    let mutable lpdf = 1.

    // Local methods
    member this.Radius = radius
    member this.Texture = texture
    member this.Sampler = sampler
    member this.Sphere = sphere
    member this.FlushDirections (hitPoint: HitPoint) = 
        let generateSample(sx, sy) = 
            let (x,y,z) = Sampling.mapToHemisphere (sx, sy) 0.
            let sp_local = Point(x/2.,y/2.,z)
            let up = new Vector(0., 1., 0.)
            let w = hitPoint.Normal
            let v = (up % w).Normalise
            let u = w % v
            sp_local.OrthonormalTransform(u, v, w)
        sps <- [for (x, y) in sampler.NextSet() do yield generateSample(x, y)]
        ld <- sps |> List.average
        lpdf <- [for sp in sps do yield (sp * hitPoint.Normal) / Math.PI] |> List.average

    // Overwritten methods
    override this.GetGeometricFactor hitPoint = 1. 
    override this.GetDirectionFromPoint hitPoint = hitPoint.Normal
    override this.GetProbabilityDensity hitPoint = lpdf

    override this.GetColour hitPoint = 
        let getColour (sp:Vector) = 
            let ray = Ray(hitPoint.EscapedPoint, sp.Normalise)
            let hit = sphere.hitFunction(ray)
            hit.Material.Bounce(sphere, hit, this)
        [for sp in sps do yield getColour sp] |> List.average
    
    override this.GetShadowRay (hitPoint:HitPoint) =
        if hitPoint.Material :? EmissiveMaterial then [||]
        else
            let getRay (sp:Vector) = Ray(hitPoint.EscapedPoint + sp * 0.5, sp.Normalise)
            [for sp in sps do yield getRay sp] |> List.toArray


//- AMBIENT OCCLUDER: handled in Render.fs
type AmbientOccluder (intensity: float, c: Colour, min_intensity: float, s: Sampler) = 
    inherit AmbientLight(c, intensity)

    // Local methods
    member this.Intensity = intensity
    member this.MinIntensity = min_intensity
    member this.MinIntensityColour = min_intensity * c * intensity
    member this.Colour = c * intensity
    member this.Sampler = s