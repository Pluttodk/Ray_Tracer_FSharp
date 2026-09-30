namespace Tracer.Basics

open System
open Tracer.Basics.Sampling

type ISampledCamera =
    abstract member SampleCount: int
    abstract member CreateRay: x: int * y: int * key: uint64 * sample: int -> Ray

[<AbstractClass>]
type Camera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    do
        if not position.IsFinite || not lookat.IsFinite || position = lookat then
            invalidArg "lookat" "Camera position and target must be finite and distinct."
        if not up.IsFinite || up = Vector.Zero then
            invalidArg "up" "Camera up must be finite and nonzero."
        for name, value in ["zoom", zoom; "width", width; "height", height] do
            if not (Double.IsFinite value) || value <= 0. then
                invalidArg name "Camera dimensions and view-plane distance must be finite and positive."
        if resX <= 0 || resY <= 0 || int64 resX * int64 resY > int64 Int32.MaxValue then
            invalidArg "resolution" "Camera resolution must contain between one and Int32.MaxValue pixels."
    let viewingDirection = position - lookat
    do
        if not viewingDirection.IsFinite || viewingDirection.IsZero then
            invalidArg "lookat" "Camera viewing direction must be representable as a finite, nonzero vector."
    // Field of view and orthonormal coordinate system.
    let w = viewingDirection.Normalise
    let frameUp =
        let up = up.Normalise
        if abs (up * w) < 0.999999999 then up
        elif abs w.X <= abs w.Y && abs w.X <= abs w.Z then Vector(1.,0.,0.)
        elif abs w.Y <= abs w.Z then Vector(0.,1.,0.)
        else Vector(0.,0.,1.)
    let v = (frameUp % w).Normalise
    let u = (w % v).Normalise
    let pw = width/float resX
    let ph = height/float resY
    do
        if pw = 0. || ph = 0. then invalidArg "resolution" "Camera pixel dimensions underflow at this resolution."
    member this.W = w
    member this.U = u
    member this.V = v
    member this.Pw = pw
    member this.Ph = ph
    member this.Position = position
    member this.Lookat = lookat
    member this.Up = up
    member this.Zoom = zoom
    member this.Width = width
    member this.Height = height
    member this.ResX = resX
    member this.ResY = resY
    member internal _.CheckPixel x y =
        if x < 0 || x >= resX || y < 0 || y >= resY then
            invalidArg "pixel" "Pixel coordinates must lie within the camera resolution."
    member internal _.PixelKey x y = mixKey (uint64 y * uint64 resX + uint64 x)

    // Built-in cameras use the sampled fast path; array-only subclasses retain their existing implementation.
    member this.SampleCount =
        match (this :> obj) with
        | :? ISampledCamera as sampled -> sampled.SampleCount
        | _ -> (this.CreateRays 0 0).Length

    member this.CreateRay(x: int, y: int, key: uint64, sampleIndex: int) =
        this.CheckPixel x y
        match (this :> obj) with
        | :? ISampledCamera as sampled ->
            if sampleIndex < 0 || sampleIndex >= sampled.SampleCount then
                invalidArg (nameof sampleIndex) "Camera sample index is out of range."
            sampled.CreateRay(x, y, key, sampleIndex)
        | _ ->
            let rays = this.CreateRaysAt x y key
            if sampleIndex < 0 || sampleIndex >= rays.Length then
                invalidArg (nameof sampleIndex) "Camera sample index is out of range."
            rays.[sampleIndex]

    member this.CreateRay(x: int, y: int, sampleIndex: int) =
        this.CheckPixel x y
        this.CreateRay(x, y, this.PixelKey x y, sampleIndex)

    abstract member CreateRays : int -> int -> Ray []
    abstract member CreateRaysAt : int -> int -> uint64 -> Ray []
    default this.CreateRaysAt x y _ = this.CreateRays x y
    
