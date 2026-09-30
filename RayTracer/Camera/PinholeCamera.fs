namespace Tracer.Basics

open Tracer.Basics.Sampling

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point,
                    up: Vector, zoom: float, width: float, height: float, 
                    resX: int, resY: int, sampler : Sampler, ?shutterOpen: float, ?shutterClose: float) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY, ?shutterOpen = shutterOpen, ?shutterClose = shutterClose)

    default this.CreateRays x y =
        this.CreateRaysAt x y (this.PixelKey x y)

    member private this.RayAt(x, y, key, sample) =
        this.CheckPixel x y
        if sample < 0 || sample >= sampler.SampleCount then
            invalidArg (nameof sample) "Camera sample index is out of range."
        let sx, sy = sampler.SampleAt(key, sample)
        let px = this.Pw * (float x - float this.ResX / 2. + sx)
        let py = this.Ph * (float y - float this.ResY / 2. + sy)
        let u, v, w = this.U, this.V, this.W
        let direction =
            Vector(px * v.X + py * u.X - zoom * w.X,
                   px * v.Y + py * u.Y - zoom * w.Y,
                   px * v.Z + py * u.Z - zoom * w.Z).Normalise
        Ray(this.Position, direction, this.ShutterTimeAt(key, sample, sampler.SampleCount))

    override this.CreateRaysAt x y key =
        Array.init sampler.SampleCount (fun sample -> this.RayAt(x, y, key, sample))

    interface ISampledCamera with
        member _.SampleCount = sampler.SampleCount
        member this.CreateRay(x, y, key, sample) = this.RayAt(x, y, key, sample)