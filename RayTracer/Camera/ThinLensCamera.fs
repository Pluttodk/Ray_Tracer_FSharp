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
        lensSampler : Sampler,
        ?shutterOpen : float,
        ?shutterClose : float
    ) = 
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY, ?shutterOpen = shutterOpen, ?shutterClose = shutterClose)
    do
        if not (System.Double.IsFinite r) || r < 0. then
            invalidArg "r" "Lens radius must be finite and nonnegative."
        if not (System.Double.IsFinite f) || f <= 0. then
            invalidArg "f" "Focal distance must be finite and positive."
    let sampleCount = max viewSampler.SampleCount lensSampler.SampleCount
    
    default this.CreateRays x y =
        this.CreateRaysAt x y (this.PixelKey x y)

    member private this.RayAt(x, y, key, sample) =
        this.CheckPixel x y
        if sample < 0 || sample >= sampleCount then
            invalidArg (nameof sample) "Camera sample index is out of range."
        let qx, qy = viewSampler.SampleAt(key, sample % viewSampler.SampleCount)
        let qx = this.Pw * (float x - float resX / 2. + qx)
        let qy = this.Ph * (float y - float resY / 2. + qy)
        let px, py = (f * qx) / zoom, (f * qy) / zoom
        let lx, ly = mapToDisc (lensSampler.SampleAt(mixKey (key ^^^ 0x6c656e73UL), sample % lensSampler.SampleCount))
        let lx, ly = lx * r, ly * r
        let u, v, w = this.U, this.V, this.W
        let origin =
            Point(position.X + lx * v.X + ly * u.X,
                  position.Y + lx * v.Y + ly * u.Y,
                  position.Z + lx * v.Z + ly * u.Z)
        let direction =
            Vector((px - lx) * v.X + (py - ly) * u.X - f * w.X,
                   (px - lx) * v.Y + (py - ly) * u.Y - f * w.Y,
                   (px - lx) * v.Z + (py - ly) * u.Z - f * w.Z).Normalise
        Ray(origin, direction, this.ShutterTimeAt(key, sample, sampleCount))

    override this.CreateRaysAt x y key =
        Array.init sampleCount (fun sample -> this.RayAt(x, y, key, sample))

    interface ISampledCamera with
        member _.SampleCount = sampleCount
        member this.CreateRay(x, y, key, sample) = this.RayAt(x, y, key, sample)
