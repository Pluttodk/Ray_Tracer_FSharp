namespace Tracer.Basics.Render

open System
open System.Diagnostics
open System.Threading.Tasks
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.PathTracing
open Tracer.Imaging

type RenderFilm =
    { Width: int
      Height: int
      Pixels: float array
      BuildMilliseconds: float
      TraceMilliseconds: float }
    /// Linear exposure multiplier applied before the display transfer, the way
    /// a camera's exposure works. A path-traced interior can sit two orders of
    /// magnitude below display white - San Miguel's median pixel is 0.0067 -
    /// and without this the only ways to correct it are to distort the lighting
    /// or to post-process outside the renderer.
    member this.ToImage(transfer: string, exposure: float) =
        if not (Double.IsFinite exposure) || exposure <= 0. then
            invalidArg (nameof exposure) "Exposure must be finite and positive."
        let image = new RgbImage(this.Width, this.Height)
        let destination = image.Pixels
        for index = 0 to this.Width * this.Height - 1 do
            let offset = index * 3
            let colour =
                Colour(this.Pixels.[offset] * exposure,
                       this.Pixels.[offset + 1] * exposure,
                       this.Pixels.[offset + 2] * exposure).ToDisplayColor transfer
            destination.[offset] <- colour.R
            destination.[offset + 1] <- colour.G
            destination.[offset + 2] <- colour.B
        image

    /// Post-processes the linear pixels (bloom, grade, vignette); call before ToImage, after denoising.
    member this.Post(settings: Post.PostSettings) =
        if settings.IsIdentity then this
        else { this with Pixels = Post.apply settings this.Width this.Height this.Pixels }

    member this.ToImage(transfer: string) =
        let image = new RgbImage(this.Width, this.Height)
        let destination = image.Pixels
        for index = 0 to this.Width * this.Height - 1 do
            let offset = index * 3
            let colour = Colour(this.Pixels.[offset], this.Pixels.[offset + 1], this.Pixels.[offset + 2]).ToDisplayColor transfer
            destination.[offset] <- colour.R
            destination.[offset + 1] <- colour.G
            destination.[offset + 2] <- colour.B
        image

type Render(scene: Scene, camera: Camera, ?options: RenderOptions) =
    let options = defaultArg options RenderOptions.Default
    do
        if options.Threads <= 0 || options.TileSize <= 0 then
            invalidArg (nameof options) "Thread and tile counts must be positive."
        if not (List.contains options.Transfer ["gamma2"; "srgb"; "linear"; "aces"]) then
            invalidArg (nameof options) "Unknown output transfer function."
    let shapes = List.toArray scene.Shapes
    let kind = defaultArg options.Acceleration scene.Acceleration
    let allOpaque = shapes |> Array.forall (fun shape -> shape.IsOpaque)
    let sampledCamera: ISampledCamera voption =
        match camera :> obj with
        | :? ISampledCamera as sampled when camera.GetType() = typeof<PinholeCamera> || camera.GetType() = typeof<ThinLensCamera> ->
            ValueSome sampled
        | _ -> ValueNone
    let mutable lastFilm: RenderFilm option = None
    /// Set when denoising was requested but could not run, so callers can report
    /// it rather than silently shipping an undenoised image.
    let mutable denoiseNote: string option = None
    let mutable lastMeanSamples = 0.

    let makeIntegrator acceleration =
        let query =
            { new IRayQuery with
                member _.Closest(ray, minimum, maximum) = Acceleration.traverseClosest acceleration ray minimum maximum
                member _.Any(ray, minimum, maximum) = Acceleration.anyHit acceleration ray minimum maximum }
        match options.Integrator with
        | Classic -> ClassicIntegrator(scene, query, allOpaque, options.CancellationToken) :> IIntegrator
        | Path ->
            PathIntegrator(scene, query, allOpaque, options.CancellationToken,
                           scene.MaxBounces, options.RouletteDepth) :> IIntegrator

    member _.Camera = camera
    member _.Scene = scene
    member _.Shapes = Array.copy shapes
    member _.Options = options
    member _.LastFilm = lastFilm
    member _.DenoiseNote = denoiseNote
    /// Mean samples per pixel actually traced. Equals the requested count when
    /// adaptive sampling is off; below it when pixels terminated early.
    member _.LastMeanSamplesPerPixel = lastMeanSamples
    member _.PreProcessing =
        if kind = Acceleration.FlatBVH then Acceleration.buildFlatWithOptions options.BvhOptions shapes
        else Acceleration.buildWith kind shapes
    member _.GetFirstHitPoint acceleration ray = Acceleration.traverseClosest acceleration ray 0. infinity
    member _.GetFirstShadowHitPoint acceleration ray = Acceleration.traverseClosest acceleration ray 0. infinity
    member _.Cast acceleration ray = (makeIntegrator acceleration).Trace(ray, sampleKey options.Seed 0 0)

    member this.RenderLinear =
        options.CancellationToken.ThrowIfCancellationRequested()
        let watch = Stopwatch.StartNew()
        let acceleration = this.PreProcessing
        let buildMilliseconds = watch.Elapsed.TotalMilliseconds
        let integrator = makeIntegrator acceleration
        let pixels = Array.zeroCreate<float> (camera.ResX * camera.ResY * 3)
        // Guide buffers are only accumulated when they will actually be used;
        // capturing them costs an extra write per sample.
        let denoising = options.Denoise && options.Integrator = Path
        // Adaptive sampling only makes sense for a stochastic integrator; the
        // Whitted path is deterministic, so its per-sample variance is aliasing,
        // not noise, and terminating early would just alias more.
        let adaptive = options.AdaptiveThreshold > 0. && options.Integrator = Path
        let adaptiveMinSamples = max 2 options.AdaptiveMinSamples
        let mutable adaptiveSamplesTaken = 0L
        let albedo = if denoising then Array.zeroCreate<float> (camera.ResX * camera.ResY * 3) else Array.empty
        let normals = if denoising then Array.zeroCreate<float> (camera.ResX * camera.ResY * 3) else Array.empty
        let tileColumns = (camera.ResX + options.TileSize - 1) / options.TileSize
        let tileRows = (camera.ResY + options.TileSize - 1) / options.TileSize
        let parallelOptions =
            ParallelOptions(MaxDegreeOfParallelism = options.Threads, CancellationToken = options.CancellationToken)
        watch.Restart()
        Parallel.For(0, tileColumns * tileRows, parallelOptions, fun tile ->
            let startX = (tile % tileColumns) * options.TileSize
            let startY = (tile / tileColumns) * options.TileSize
            let endX = min camera.ResX (startX + options.TileSize)
            let endY = min camera.ResY (startY + options.TileSize)
            for y = startY to endY - 1 do
                for x = startX to endX - 1 do
                    let pixel = y * camera.ResX + x
                    let cameraKey = mixKey (uint64 (uint32 options.Seed) ^^^ uint64 pixel)
                    let struct (rays, count) =
                        match sampledCamera with
                        | ValueSome sampled -> struct (Array.empty, sampled.SampleCount)
                        | ValueNone ->
                            let rays = camera.CreateRaysAt x y cameraKey
                            struct (rays, rays.Length)
                    if count = 0 then invalidOp $"Camera returned no rays for pixel ({x},{y})."
                    let mutable red = 0.
                    let mutable green = 0.
                    let mutable blue = 0.
                    let mutable albedoR = 0.
                    let mutable albedoG = 0.
                    let mutable albedoB = 0.
                    let mutable normalX = 0.
                    let mutable normalY = 0.
                    let mutable normalZ = 0.
                    // Welford accumulators over per-sample luminance, used to
                    // decide when this pixel has converged. Running them
                    // incrementally avoids a second pass over the samples.
                    let mutable taken = 0
                    let mutable luminanceMean = 0.
                    let mutable luminanceM2 = 0.
                    let mutable converged = false
                    while taken < count && not converged do
                        let sample = taken
                        let ray =
                            match sampledCamera with
                            | ValueSome sampled -> sampled.CreateRay(x, y, cameraKey, sample)
                            | ValueNone -> rays.[sample]
                        let key = sampleKey options.Seed pixel sample
                        let colour =
                            if denoising then
                                let struct (colour, guideAlbedo, guideNormal) = integrator.TraceWithGuides(ray, key)
                                albedoR <- albedoR + guideAlbedo.R
                                albedoG <- albedoG + guideAlbedo.G
                                albedoB <- albedoB + guideAlbedo.B
                                normalX <- normalX + guideNormal.X
                                normalY <- normalY + guideNormal.Y
                                normalZ <- normalZ + guideNormal.Z
                                colour
                            else integrator.Trace(ray, key)
                        red <- red + colour.R
                        green <- green + colour.G
                        blue <- blue + colour.B
                        taken <- taken + 1
                        if adaptive then
                            let luminance = 0.2126 * colour.R + 0.7152 * colour.G + 0.0722 * colour.B
                            let delta = luminance - luminanceMean
                            luminanceMean <- luminanceMean + delta / float taken
                            luminanceM2 <- luminanceM2 + delta * (luminance - luminanceMean)
                            // Stop once the standard error of the MEAN is a small
                            // fraction of the mean itself. Checking only every
                            // few samples keeps the test cheap and stops a lucky
                            // run of samples from terminating a pixel early.
                            if taken >= adaptiveMinSamples && taken % 8 = 0 then
                                let variance = luminanceM2 / float (taken - 1)
                                let standardError = sqrt (variance / float taken)
                                if standardError <= options.AdaptiveThreshold * (abs luminanceMean + 1e-4) then
                                    converged <- true
                    let scale = 1. / float (max 1 taken)
                    let target = ((camera.ResY - 1 - y) * camera.ResX + x) * 3
                    pixels.[target] <- red * scale
                    pixels.[target + 1] <- green * scale
                    pixels.[target + 2] <- blue * scale
                    if adaptive then
                        System.Threading.Interlocked.Add(&adaptiveSamplesTaken, int64 taken) |> ignore
                    if denoising then
                        albedo.[target] <- albedoR * scale
                        albedo.[target + 1] <- albedoG * scale
                        albedo.[target + 2] <- albedoB * scale
                        // Averaged normals are no longer unit length; OIDN wants
                        // them normalized, so renormalize per pixel.
                        let length = sqrt (normalX * normalX + normalY * normalY + normalZ * normalZ)
                        if length > 1e-9 then
                            normals.[target] <- normalX / length
                            normals.[target + 1] <- normalY / length
                            normals.[target + 2] <- normalZ / length) |> ignore
        watch.Stop()
        lastMeanSamples <-
            if adaptive then float adaptiveSamplesTaken / float (camera.ResX * camera.ResY)
            else float (if sampledCamera.IsSome then (match sampledCamera with ValueSome c -> c.SampleCount | _ -> 0) else 0)
        if denoising then
            let toFloat32 (source: float array) = Array.init source.Length (fun i -> float32 source.[i])
            match Denoiser.denoiseWithDiagnostics camera.ResX camera.ResY
                      (toFloat32 pixels) (Some(toFloat32 albedo)) (Some(toFloat32 normals)) with
            | Ok denoised ->
                for index = 0 to pixels.Length - 1 do
                    let value = float denoised.[index]
                    // The denoiser can return small negatives; Colour rejects
                    // those, and negative radiance is meaningless anyway.
                    pixels.[index] <- if Double.IsFinite value && value > 0. then value else 0.
            | Error message -> denoiseNote <- Some message
        let film =
            { Width = camera.ResX; Height = camera.ResY; Pixels = pixels
              BuildMilliseconds = buildMilliseconds; TraceMilliseconds = watch.Elapsed.TotalMilliseconds }
        lastFilm <- Some film
        film

    member this.RenderParallel = this.RenderLinear.ToImage(options.Transfer, options.Exposure)
    member _.SaveImage(image: RgbImage, path: string) = image.SavePng path
    member _.Clean(image: RgbImage) = image.Dispose()

    member this.RenderToFile path =
        use image = this.RenderParallel
        this.SaveImage(image, path)

    member _.ShowImageOnScreen(_: RgbImage) : unit =
        raise (PlatformNotSupportedException("Use renderToFile to save a PNG with the headless renderer."))

    member _.RenderToScreen : unit =
        raise (PlatformNotSupportedException("Use renderToFile to save a PNG with the headless renderer."))
