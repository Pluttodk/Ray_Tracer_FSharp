namespace Tracer.Basics

open Tracer.Basics.Sampling

type ThinLensCamera
    (
        position : Point, 
        lookat : Point,
        up: Vector,
        zoom: float,
        width: float,
        height: float,
        resX: int,
        resY: int,
        r: float,
        f: float,
        viewSampler : Sampler,
        lensSampler : Sampler
    ) = 
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)   
    
    default this.CreateRays x y =
        let lensSamples = lensSampler.NextSet()
        let viewSamples = viewSampler.NextSet()

        [|for i in 0..lensSampler.SampleCount-1 do
            // Create Ray, setup direction and origin.
            let qx, qy = viewSamples.[i] // Sample unit square for center ray.
            let qx = this.Pw * ((float x - float resX/2.0) + qx)
            let qy = this.Ph * ((float y - float resY/2.0) + qy)
            
            let px, py = (f * qx)/zoom, (f * qy)/zoom

            let lx, ly = lensSamples.[i] // Sample unit disc with respect to r.
            let lx = lx * r
            let ly = ly * r

            // Create the primary lens ray, from the lens disc point to the focal point.
            let rayOrigin = this.Position + lx * this.V + ly * this.U

            let rayDirection = ((px - lx) * this.V + (py - ly) * this.U - f * this.W).Normalise
            yield (new Ray(rayOrigin, rayDirection))|]
