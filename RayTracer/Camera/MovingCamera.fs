namespace Tracer.Basics

open Tracer.Basics.Sampling

/// A pinhole camera that moves while the shutter is open. `poses` are (time, position, look-at, up) keys in
/// increasing time; each ray is generated from the pose at its shutter time, so a camera tracking a subject
/// keeps it sharp while the background streaks, as a real tracking shot does.
type MovingPinholeCamera
    (poses: (float * Point * Point * Vector)[], zoom: float, width: float, height: float,
     resX: int, resY: int, sampler: Sampler, shutterOpen: float, shutterClose: float) =
    inherit Camera(
        (let _, p, _, _ = poses.[poses.Length / 2] in p),
        (let _, _, l, _ = poses.[poses.Length / 2] in l),
        (let _, _, _, u = poses.[poses.Length / 2] in u),
        zoom, width, height, resX, resY, shutterOpen, shutterClose)
    do
        if poses.Length = 0 then invalidArg (nameof poses) "A moving camera needs at least one pose."
        for i in 1 .. poses.Length - 1 do
            let t0, _, _, _ = poses.[i - 1]
            let t1, _, _, _ = poses.[i]
            if not (t1 > t0) then invalidArg (nameof poses) "Camera poses must have strictly increasing times."

    /// Orthonormal frame for a pose, built exactly as Camera does.
    static let frame (position: Point) (lookat: Point) (up: Vector) =
        let w = (position - lookat).Normalise
        let frameUp =
            let up = up.Normalise
            if abs (up * w) < 0.999999999 then up
            elif abs w.X <= abs w.Y && abs w.X <= abs w.Z then Vector(1.,0.,0.)
            elif abs w.Y <= abs w.Z then Vector(0.,1.,0.)
            else Vector(0.,0.,1.)
        let v = (frameUp % w).Normalise
        let u = (w % v).Normalise
        struct (u, v, w)

    let lerpPoint (a: Point) (b: Point) s = a + s * (b - a)

    member private _.PoseAt(time: float) =
        let last = poses.Length - 1
        let t0, _, _, _ = poses.[0]
        let tn, _, _, _ = poses.[last]
        if last = 0 || time <= t0 then let _, p, l, u = poses.[0] in struct (p, l, u)
        elif time >= tn then let _, p, l, u = poses.[last] in struct (p, l, u)
        else
            let mutable hi = 1
            while (let t, _, _, _ = poses.[hi] in t) < time do hi <- hi + 1
            let ta, pa, la, ua = poses.[hi - 1]
            let tb, pb, lb, ub = poses.[hi]
            let s = (time - ta) / (tb - ta)
            struct (lerpPoint pa pb s, lerpPoint la lb s, ua + s * (ub - ua))

    member private this.RayAt(x, y, key, sample) =
        this.CheckPixel x y
        if sample < 0 || sample >= sampler.SampleCount then
            invalidArg (nameof sample) "Camera sample index is out of range."
        let time = this.ShutterTimeAt(key, sample, sampler.SampleCount)
        let struct (position, lookat, up) = this.PoseAt time
        let struct (u, v, w) = frame position lookat up
        let sx, sy = sampler.SampleAt(key, sample)
        let px = this.Pw * (float x - float this.ResX / 2. + sx)
        let py = this.Ph * (float y - float this.ResY / 2. + sy)
        let direction =
            Vector(px * v.X + py * u.X - zoom * w.X,
                   px * v.Y + py * u.Y - zoom * w.Y,
                   px * v.Z + py * u.Z - zoom * w.Z).Normalise
        Ray(position, direction, time)

    default this.CreateRays x y = this.CreateRaysAt x y (this.PixelKey x y)
    override this.CreateRaysAt x y key = Array.init sampler.SampleCount (fun sample -> this.RayAt(x, y, key, sample))

    interface ISampledCamera with
        member _.SampleCount = sampler.SampleCount
        member this.CreateRay(x, y, key, sample) = this.RayAt(x, y, key, sample)
