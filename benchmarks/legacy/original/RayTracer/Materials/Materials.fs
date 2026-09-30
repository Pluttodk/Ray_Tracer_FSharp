namespace Tracer.Basics
open System
open Tracer.Basics.Sampling


//- MATTE MATERIAL
type MatteMaterial
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float
    ) = 
    inherit Material()
    
    // Optimization constant
    let pidivided = 1. / Math.PI

    // Local methods
    member this.MatteCoefficient = matteCoefficient
    member this.MatteColour = matteColour
    
    // Overwritten methods
    default this.AmbientColour(hitPoint, ambientLight) = ambientColour * ambientCoefficient * ambientLight.GetColour hitPoint
    default this.ReflectionFactor (_,_) = Colour.White
    default this.BounceMethod hitPoint = [||]
    default this.IsRecursive = false
    default this.Bounce(_, hitPoint, light) = 
        
        // Initialize parameters
        let kd = matteCoefficient                           // Matte coefficient
        let cd = matteColour                                // Matte colour
        let lc = light.GetColour hitPoint                   // Light colour
        let n  = hitPoint.Normal                            // Normal at hit point
        let ld = (light.GetDirectionFromPoint hitPoint)     // Light direction

        // Determine the colour (ambient colour is handled in Render.fs)
        if n * ld > 0. then    
            let diffuse = (kd * cd) * pidivided
            let volume = (light.GetGeometricFactor hitPoint / light.GetProbabilityDensity hitPoint)
            let roundness = lc * (n * ld)
            diffuse * volume * roundness
        else
            Colour.Black


//- SPECULAR REFLECTION MATERIAL (PHONG)     
type PhongMaterial 
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        specularColour: Colour, 
        specularCoefficient: float, 
        specularExponent: int
    ) = 
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)
    
    // Optimization constant
    let pidivided = 1. / Math.PI
    
    // Local methods
    member this.SpecularCoefficient = specularCoefficient
    member this.SpecularColour = specularColour
    member this.SpecularExponent = specularExponent
    
    // Overwritten methods
    default this.Bounce(_, hitPoint, light) = 
        
        // Initialize parameters
        let ld = (light.GetDirectionFromPoint hitPoint)             // Light direction
        let n = hitPoint.Normal                                     // Normal at hit point
        let r1 = -ld + (2. * (n * ld)) * n                          // Light ray direction
        let ray = hitPoint.Ray                                      // Casted ray
        let rd = ray.GetDirection.Normalise                         // Direction of casted ray
        let e = specularExponent                                    // Specular exponent
        let ks = specularCoefficient                                // Specular coefficient
        let cs = specularColour                                     // Specular colour
        let lc  = light.GetColour hitPoint                          // Light colour
        
        // Detemine the colour ()
        if n * ld > 0. then

            // The standard diffuse colour
            let matte = (matteCoefficient * matteColour) * pidivided
            
            // The specular colour
            let specular = 
                if r1 * -rd > 0. then
                    ks * cs * ((r1 * (-rd)) ** float(e))
                else
                    Colour.Black
            let direction = lc * (n * ld)
            
            // The final colour
            (matte + specular) * direction
        else
            Colour.Black


type RayReflector = 
    static member Perfect (hitPoint:HitPoint) = (hitPoint.Ray.GetDirection.Normalise + (-2. * (hitPoint.Normal.Normalise * hitPoint.Ray.GetDirection.Normalise)) * hitPoint.Normal.Normalise).Normalise


//- MATTE REFLECTIVE MATERIAL
type MatteReflectiveMaterial 
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        reflectionColour: Colour, 
        reflectionCoefficient: float
    ) = 
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)
    
    // Local methods
    member this.ReflectionColour = reflectionColour            
    member this.ReflectionCoefficient = reflectionCoefficient   

    // Overwritten methods
    default this.ReflectionFactor (_,_) = reflectionColour * reflectionCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint = [| Ray(hitPoint.EscapedPoint, RayReflector.Perfect hitPoint) |]
    default this.Bounce(shape, hitPoint, light) = base.Bounce(shape, hitPoint, light)

//- PHONG REFLECTIVE MATERIAL
type PhongReflectiveMaterial 
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        specularColour: Colour, 
        specularCoefficient: float, 
        reflectionColour: Colour, 
        reflectionCoefficient: float,
        specularExponent: int
    ) = 
    inherit PhongMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient, specularColour, specularCoefficient, specularExponent)
    
    // Local methods
    member this.ReflectionColour = reflectionColour            
    member this.ReflectionCoefficient = reflectionCoefficient   
       
    // Overwritten methods
    default this.ReflectionFactor (_,_) = reflectionColour * reflectionCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint = [| Ray(hitPoint.EscapedPoint, RayReflector.Perfect hitPoint) |]
    default this.Bounce(shape, hitPoint, light) = base.Bounce(shape, hitPoint, light)

//- MATTE GLOSSY REFLECTIVE MATERIAL
type MatteGlossyReflectiveMaterial     
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        reflectiveColour: Colour,
        glossyCoefficient: float,
        glossyExponent: int,
        sampler: Sampler
    ) = 
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)
    
    // Overwritten methods
    default this.ReflectionFactor (_,_) = reflectiveColour * glossyCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint =
        
        // Prepare for sampling
        let direction = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal.Normalise
        let samples = sampler.NextSet()

        // Sample the outgoing rays
        [|for (x, y) in samples do
            
            // Get a sample point from the sampler, and map it to a hemisphere
            let sp = Tracer.Basics.Point(mapToHemisphere (x,y) (float(glossyExponent)))

            // Reflected ray direction
            let m = RayReflector.Perfect hitPoint

            // Transform orthonormal frame of sample point
            let up = new Vector(0., 1., 0.)
            let w = m.Normalise
            let v = (up % w).Normalise
            let u = w % v
            let transformed_sp = sp.OrthonormalTransform (u, v, w)

            // Add outgoing ray to the array
            yield
                if transformed_sp * normal > 0. then
                    Ray(hitPoint.Point, transformed_sp.Normalise)
                else
                    Ray(hitPoint.Point, (-sp.X * u - sp.Y * v + sp.Z * w).Normalise)|]
        
    default this.Bounce(shape, hitPoint, light) = base.Bounce(shape, hitPoint, light)


//- MATTE GLOSSY REFLECTIVE MATERIAL
type PhongGlossyReflectiveMaterial     
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        specularColour: Colour, 
        specularCoefficient: float, 
        reflectiveColour: Colour,
        glossyCoefficient: float,
        specularExponent: int,
        glossyExponent: int,
        sampler: Sampler
    ) = 
    inherit PhongMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient, specularColour, specularCoefficient, specularExponent)
    
    // Overwritten methods
    default this.ReflectionFactor (_,_) = reflectiveColour * glossyCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint =
        
        // Prepare for sampling
        let direction = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let samples = sampler.NextSet()

        // Sample the outgoing rays
        [|for (x, y) in samples do
            
            // Get a sample point from the sampler, and map it to a hemisphere
            let sp = Tracer.Basics.Point(mapToHemisphere (x, y) (float(glossyExponent)))

            // Reflected ray direction
            let m = RayReflector.Perfect hitPoint

            // Transform orthonormal frame of sample point
            let up = new Vector(0., 1., 0.)
            let w = m.Normalise
            let v = (up % w).Normalise
            let u = w % v
            let transformed_sp = sp.OrthonormalTransform (u, v, w)

            // Add outgoing ray to the array
            yield 
                if transformed_sp * normal > 0. then
                    Ray(hitPoint.Point, transformed_sp.Normalise)
                else
                    Ray(hitPoint.Point, (-sp.X * u - sp.Y * v + sp.Z * w).Normalise)|]

    default this.Bounce(shape, hitPoint, light) = 
        // Bounce the diffuse material
        base.Bounce(shape, hitPoint, light)


//- EMISSIVE MATERIAL
type EmissiveMaterial(lightColour: Colour, lightIntensity: float) = 
    inherit Material()

    // Optimization constant
    let emisiveRadience = lightColour * lightIntensity

    // Local fields
    member this.LightColour = lightColour
    member this.LightIntensity = lightIntensity
    member this.EmisiveRadience = emisiveRadience

    // Overwritten fields
    default this.AmbientColour(_, _) = Colour.Black
    default this.IsRecursive = false
    default this.ReflectionFactor (_,_) = Colour.White
    default this.BounceMethod _ = [||]
    default this.Bounce(_, hitPoint, _) = 
        
        // Only emit light from the front
        if hitPoint.Normal * -hitPoint.Ray.GetDirection > 0. then emisiveRadience
        else Colour.Black


type TransparentRay(origin: Point, direction: Vector, refracted: bool, isInside: bool) = 
    inherit Ray(origin, direction)
    member this.Refracted = refracted
    member this.IsInside = isInside

//- TRANSPARENT MATERIAL
type TransparentMaterial
    (
        innerFilterColour: Colour, 
        outerFilterColour: Colour, 
        innerRefractionIndex: float, 
        outerRefractionIndex: float
    ) = 
    inherit Material()

    // Private methods
    let refrIndex = (innerRefractionIndex / outerRefractionIndex)
    let refrIndexExpr = refrIndex ** -2.

    // Local methods
    member this.ShouldRefract (hitPoint: HitPoint) = 
        let cos_angle_in = -(hitPoint.Normal * hitPoint.Ray.GetDirection)
        let cos_angle_out_exp = 1. - (1. - (cos_angle_in ** 2.)) / (refrIndex ** 2.)
        if cos_angle_out_exp < 0. then (false, cos_angle_in, cos_angle_out_exp)
        else (true,  cos_angle_in, cos_angle_out_exp)

    member this.RefractRay (hitPoint: HitPoint) (cos_angle_in, cos_angle_out_exp) = 
        let cos_angle_out = Math.Sqrt(cos_angle_out_exp)
        let (origin, isInside) = 
            if hitPoint.Ray :? TransparentRay then
                let transRay = hitPoint.Ray :?> TransparentRay
                if transRay.IsInside then
                    hitPoint.EscapedPoint, true
                else
                    hitPoint.InnerEscapedPoint, false
            else 
                hitPoint.EscapedPoint, false
        let direction = (1. / refrIndex) * hitPoint.Ray.GetDirection - (cos_angle_out - (cos_angle_in / refrIndex)) * hitPoint.Normal.Normalise
        TransparentRay(origin, direction.Normalise, true, isInside)

    // Overwritten methods
    default this.IsRecursive = true
    default this.AmbientColour(_,_) = Colour.White

    default this.ReflectionFactor (hitPoint,rayOut) = 
        let (shouldRefract, cos_angle_in, cos_angle_out_exp) = this.ShouldRefract hitPoint
        let cos_angle_out = Math.Sqrt(cos_angle_out_exp)
        let refrAngle = refrIndex * cos_angle_in
        let r_dd = (refrAngle - cos_angle_out) / (refrAngle + cos_angle_out)
        let r_t = (cos_angle_in - refrIndex * cos_angle_out) / (cos_angle_in + refrIndex * cos_angle_out)
        let k_r = ((r_dd ** 2.) + (r_t ** 2.)) / 2.
        let k_t = 1. - k_r

        if shouldRefract then
            if rayOut.GetDirection * hitPoint.Normal < 0. then
                Colour.White * k_t * refrIndexExpr
            else
                Colour.White * k_r
        else
            Colour.White

    default this.BounceMethod hitPoint = 
        let (shouldRefract, cos_angle_in, cos_angle_out_exp) = this.ShouldRefract hitPoint

        let dir = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rayDirection = (dir + (-2. * (normal * dir)) * normal)
        let isInside = 
            if hitPoint.Ray :? TransparentRay then
                (hitPoint.Ray :?> TransparentRay).IsInside
            else
                false
        let perfectOrigin = 
            if isInside then 
                hitPoint.InnerEscapedPoint
            else
                hitPoint.EscapedPoint
        let perfectRay = TransparentRay(perfectOrigin, rayDirection.Normalise, false, isInside)
        let refractRay = this.RefractRay hitPoint (cos_angle_in, cos_angle_out_exp)
        if shouldRefract then
            [| perfectRay; refractRay |]
        else
            [| perfectRay |]

    default this.Bounce(_,_,_) = Colour.Black