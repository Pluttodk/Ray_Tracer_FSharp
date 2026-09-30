namespace Tracer.Basics

open Tracer.Basics.Sampling

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point,
                    up: Vector, zoom: float, width: float, height: float, 
                    resX: int, resY: int, sampler : Sampler) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)

    default this.CreateRays x y =
        let samples = sampler.NextSet()
        // Create the rays for anti-aliasing.
        [|for (sx, sy) in samples do
            let px = this.Pw * (float(x - (this.ResX/2)) + sx)
            let py = this.Ph * (float(y - (this.ResY/2)) + sy)
            let direction = (px * this.V) + (py * this.U) - (zoom * this.W)
            yield (new Ray(this.Position, direction.Normalise))|]