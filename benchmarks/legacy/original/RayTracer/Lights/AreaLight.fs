namespace Tracer.Basics

open Tracer.Basics.Sampling
open System
open Tracer.BaseShape

//- AREA LIGHT
[<AbstractClass>]
type AreaLight(surfaceMaterial: Material, sampler: Sampler) = 
    inherit Light(Colour.White, 1.)

    // Pre-defined methods
    member this.Texture = Textures.mkMatTexture surfaceMaterial // Texture of emissive surface
    member this.SurfaceMaterial = surfaceMaterial               // Material of emissive surface
    member this.SampleCount = sampler.SampleCount               // Sampler sample count
    member this.SampleSetCount = sampler.SetCount               // Sampler sample set count
    member this.Sampler = sampler

    // Abstract methods
    abstract member Shape: Shape                                // Shape of the surface
    abstract member SamplePoint: Point -> Point                 // Returns a new sample point
    abstract member SamplePointNormal: Point -> Vector          // Returns the normal of a sample point
    
    // Overwritten methods
    override this.GetColour hitPoint = 

        let getSampleColour() =
            
            // Get a sample point
            let sp = this.SamplePoint hitPoint.Point
            let n = this.SamplePointNormal sp
            if n * (hitPoint.Point - sp).Normalise > 0. then surfaceMaterial.Bounce(hitPoint.Shape, hitPoint, this)
            else Colour.Black

        // Find the average colour
        [for _ in 0..sampler.SampleCount-1 do yield getSampleColour()] |> List.average
        
    override this.GetDirectionFromPoint hitPoint = 
        [for i in 0..sampler.SampleCount-1 do yield (this.SamplePoint hitPoint.Point - hitPoint.Point).Normalise] |> List.average
        
    override this.GetShadowRay hitPoint = 
        // EmissiveMaterial does not cast or recieve shadows
        if hitPoint.Material :? EmissiveMaterial then [||]
        else
            // Generate shadow rays for the samples
            let shadowRays = Array.create sampler.SampleCount Ray.None
            for i=0 to sampler.SampleCount-1 do 
                let sp = this.SamplePoint hitPoint.Point
                let shadowRayOrigin = hitPoint.Point + hitPoint.Normal * 0.00001
                let direction = (sp - shadowRayOrigin).Normalise
                shadowRays.[i] <- Ray(shadowRayOrigin, direction)
            shadowRays
    
    // Geometric factor
    override this.GetGeometricFactor hitPoint = 
        let p = hitPoint.Point
        let sp = this.SamplePoint p
        let sp_n = this.SamplePointNormal sp
        (sp_n * (p - sp).Normalise) / ((p - sp) * (p - sp))
    

//- DISC (AREA LIGHT)
type DiscAreaLight(surfaceMaterial: Material, disc: BaseDisc, sampler: Sampler) = 
    inherit AreaLight (surfaceMaterial, sampler)
    
    // Local methods
    member this.DiscShape = this.Shape :?> Disc

    // Overwritten methods
    override this.Shape = disc.toShape this.Texture
    override this.SamplePointNormal _ = this.DiscShape.normal
    override this.GetProbabilityDensity _ = Math.PI * disc.radius * disc.radius
    override this.SamplePoint _ = 
        let sp = sampler.Next()
        let (x,y) = mapToDisc sp
        Point(x * disc.radius, y * disc.radius, disc.center.Z)
        

//- RECTANGLE (AREA LIGHT)
type RectangleAreaLight(surfaceMaterial: Material, rect: BaseRectangle, sampler: Sampler) = 
    inherit AreaLight (surfaceMaterial, sampler)
    
    // Local methods
    member this.RectangleShape = this.Shape :?> Rectangle

    // Overwritten methods
    override this.GetProbabilityDensity _ = rect.width * rect.height
    override this.SamplePointNormal _ = this.RectangleShape.normal
    override this.Shape = rect.toShape this.Texture
    override this.SamplePoint _ = 
        let (x,y) = sampler.Next()
        Point(x * rect.width, y * rect.height, 0.)


//- SPHERE (AREA LIGHT)
type SphereAreaLight(surfaceMaterial: Material, sphere: BaseShape, sampler: Sampler) = 
    inherit AreaLight (surfaceMaterial, sampler)
    
    // Local methods
    member this.SphereShape = this.Shape :?> SphereShape

    // Overwritten methods
    override this.Shape = sphere.toShape this.Texture
    override this.SamplePointNormal point = (point - this.SphereShape.origin).Normalise
    override this.GetProbabilityDensity _ = 2. * Math.PI * (this.SphereShape.radius * this.SphereShape.radius)
    override this.SamplePoint point = 

        // Hemisphere transform the samples
        let hem_sp = Point((mapToHemisphere (sampler.Next()) 50.))

        // Transform orthonormal coordinate space
        let d_c_p = (point - this.SphereShape.origin).Normalise
        let up = new Vector(0., 1., 0.)
        let w = d_c_p.Normalise
        let v = (up % w).Normalise
        let u = w % v
        let v = hem_sp.OrthonormalTransform (u, v, w)

        // Return the sample point
        Point(v.X/2., v.Y/2., v.Z/2.)

   
module TransformLight = 
    let transformDirectionalLight ((light:DirectionalLight),t) = 
        let matrix = Transformation.vectorToMatrix (light.GetDirectionFromPoint (HitPoint(Point(0.,0.,0.))))
        let transMatrix = Transformation.QuickMatrix.multi (Transformation.getMatrix(t),matrix)
        Transformation.matrixToVector transMatrix

    let transformPointLight ((light:PointLight),t) = 
        let matrix = Transformation.pointToMatrix (light.Position)
        let transMatrix = Transformation.QuickMatrix.multi (Transformation.getMatrix(t),matrix)
        Transformation.matrixToPoint transMatrix

    let transformLight (light:Light) t =
        match light with
        | :? DirectionalLight as d -> DirectionalLight(d.BaseColour, d.Intensity, transformDirectionalLight (d,t)) :> Light
        | :? PointLight as p -> PointLight(p.BaseColour, p.Intensity, transformPointLight (p,t)) :> Light
        | :? AreaLight as a -> 
                let movedShape = Transform.transform a.Shape t
                let movedArea = 
                    {new AreaLight(a.SurfaceMaterial, a.Sampler) with
                        member this.Shape = movedShape
                        member this.SamplePoint p = 
                            let inverted = Transformation.transformPoint (p, Transformation.getInvMatrix t)
                            Transformation.transformPoint (a.SamplePoint inverted, Transformation.getMatrix t)
                        member this.SamplePointNormal p = 
                            let inverted = Transformation.transformPoint (p, Transformation.getInvMatrix t)
                            a.SamplePointNormal inverted
                        member this.GetProbabilityDensity h = a.GetProbabilityDensity h
                    }            
                movedArea :> Light
        | _ -> light