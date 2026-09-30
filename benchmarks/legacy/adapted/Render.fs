namespace Tracer.Basics.Render

open Tracer.Basics
open Tracer.Basics.Acceleration
open System
open System.Threading.Tasks
open Tracer.Basics.Sampling
open System.Threading
open Tracer.Imaging
open System.Diagnostics

type LegacyRenderFilm =
    { Width: int
      Height: int
      LinearRgb: float[]
      Image: RgbImage
      BuildMs: float
      TraceMs: float
      OutputPreparationMs: float
      PhaseNotes: string[] }

type Render(scene : Scene, camera : Camera) =
    // Pre-rendering
    let rec filtershapes (nobb: Shape list) (bb : Shape list) = function
      | []            -> nobb, List.toArray bb
      | (c:Shape)::cr -> 
          try 
            c.getBoundingBox() |> ignore
            filtershapes nobb (c::bb) cr
          with 
            | _ -> filtershapes (c::nobb) bb cr                   
    let (nobbshapes, bbshapes) = filtershapes [] [] scene.Shapes

    let idOfScene = Acceleration.listOfAccel.Length + 1
    member this.Camera = camera
    member this.Scene = scene
    member this.Shapes = List.toArray scene.Shapes

    member this.Cast accel ray =
        // Get the hitpoint
        let hitPoint: HitPoint = this.GetFirstHitPoint accel ray

        // Check if we hit
        if hitPoint.DidHit then
            // Sum the light colors for that hitpoint
            let ambientColour = this.CastAmbientColour accel hitPoint
            let totalLightColour = 
                this.Scene.Lights 
                |> List.fold (fun acc light -> 
                    if light :? EnvironmentLight then
                        lock light (fun() -> 
                            let colour = this.CastRecursively accel ray hitPoint.Shape hitPoint light Colour.Black this.Scene.MaxBounces hitPoint.Material.BounceMethod
                            acc + colour)
                    else
                        let colour = this.CastRecursively accel ray hitPoint.Shape hitPoint light Colour.Black this.Scene.MaxBounces hitPoint.Material.BounceMethod
                        acc + colour
                    ) Colour.Black
            ambientColour + totalLightColour
        else
            // If we did not hit, return the background colour
            this.Scene.BackgroundColour

    member this.CastAmbientColour accel hitPoint = 
        hitPoint.Material.AmbientColour(hitPoint, this.Scene.Ambient) *
            match this.Scene.Ambient with
                | :? AmbientOccluder as occluder -> this.Occlude accel occluder hitPoint
                | _ -> Colour.White

    member this.Occlude accel (occluder: AmbientOccluder) (hitPoint: HitPoint) = 
        let sampler = occluder.Sampler
        let transOrthoCoord (hemPoint:(float*float*float)) = 
            
            // Transform orthonormal frame of sample point
            let sp = new Tracer.Basics.Point(hemPoint)
            let up = new Vector(0., 1., 0.)
            let w = hitPoint.Normal
            let v = (up % w).Normalise
            let u = w % v
            let transformed_sp = sp.OrthonormalTransform(u, v, w)

            if transformed_sp * hitPoint.Normal > 0.0 then
                transformed_sp
            else 
                (-sp.X * v - sp.Y * u + sp.Z * w).Normalise

        let samples = sampler.NextSet()
        let total = [for (x, y) in samples do yield transOrthoCoord (mapToHemisphere (x,y) 1.)]
                    |> List.fold (fun acc ad -> acc + this.CastAmbientOcclusion accel ad occluder hitPoint) Colour.Black 

        total / sampler.SampleCount

    member this.CastAmbientOcclusion accel (sp: Vector) (occluder: AmbientOccluder) (hitPoint: HitPoint) = 
        let ray = Ray(hitPoint.EscapedPoint, sp.Normalise)
        let rayHit = this.GetFirstHitPoint accel ray
        if rayHit.DidHit then
            occluder.MinIntensityColour
        else
            occluder.Colour       

    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint accel (ray:Ray) : HitPoint = 

      let rec findClosestHit (h:HitPoint) t' = function
      | []    -> 
          let hit = traverseIAcceleration accel ray bbshapes
          if hit.DidHit && hit.Time < t' then hit
          else h
      | (s:Shape)::sl -> 
          let hit = s.hitFunction ray
          if hit.DidHit && hit.Time < t' then findClosestHit hit hit.Time sl
          else findClosestHit h t' sl
      findClosestHit (HitPoint(ray)) infinity nobbshapes

    member this.GetFirstShadowHitPoint accel (ray:Ray) : HitPoint = 
        let hit = this.GetFirstHitPoint accel ray
        if hit.Material :? EmissiveMaterial then HitPoint(ray) // no shadow if we have direct rout to emissive material
        else hit

    // Returns the average shadow for a hitpoint and a light source
    member this.CastShadow accel (hitPoint: HitPoint) (light: Light) : Colour = 
        if light :? AmbientLight || hitPoint.Material :? TransparentMaterial
            then Colour.Black
        else
            let shadowRays = light.GetShadowRay hitPoint
            
            let maxTime =
                match light with
                | :? PointLight as p -> p.Position.Distance(hitPoint.Point).Magnitude
                | _ -> 2147483647.

            let isShadow ray = 
                let hp = (this.GetFirstShadowHitPoint accel ray)
                if hp.DidHit && hp.Time < maxTime then
                        Colour.White
                    else 
                        Colour.Black
            
            if shadowRays.Length = 0 then 
                Colour.Black
            else
                let totalShadow = Array.fold (fun acc ray -> acc + isShadow ray) Colour.Black shadowRays
                (totalShadow / float(shadowRays.Length))

    // Will cast a ray recursively
    member this.CastRecursively 
        (accel: IAcceleration) (incomingRay: Ray) (shape: Shape) (hitPoint: HitPoint) (light: Light) (acc: Colour) (bounces: int) 
        (reflectionFunction: HitPoint -> Ray[]) : Colour =
        
        if light :? EnvironmentLight then
            (light :?> EnvironmentLight).FlushDirections(hitPoint)

        let shadowColour = this.CastShadow accel hitPoint light

        if bounces = 0 || not hitPoint.Material.IsRecursive then
            acc + (hitPoint.Material.Bounce(shape, hitPoint, light) - shadowColour)
        else
            let outRay = reflectionFunction hitPoint
            let baseColour = acc + (hitPoint.Material.Bounce(shape, hitPoint, light) - shadowColour)
            let mutable outColour = Colour.Black
            for i = 0 to outRay.Length-1 do
                 
                    let outHitPoint = this.GetFirstHitPoint accel outRay.[i]
                    if outHitPoint.DidHit then
                        
                        let recursiveColour = this.CastRecursively accel outRay.[i] outHitPoint.Shape outHitPoint light baseColour (bounces - 1) reflectionFunction
                        outColour <- outColour + hitPoint.Material.ReflectionFactor(hitPoint, outRay.[i]) * recursiveColour
                    else
                        outColour <- outColour + this.Scene.BackgroundColour
            baseColour + (outColour)

    member this.PreProcessing =
        let accel = Acceleration.createAcceleration (shapeArray (idOfScene, bbshapes, None))

        accel

    member this.ShowImageOnScreen (_:RgbImage) : unit =
        raise (PlatformNotSupportedException("Window preview is unavailable in legacy-port. Use renderToFile to save a PNG."))

    member this.SaveImage (renderedImage:RgbImage, filepath:string) =
        renderedImage.SavePng(filepath)

    member private this.RenderPixels (captureLinear: bool) =
        let watch = Stopwatch.StartNew()
        let accel = this.PreProcessing
        let buildMs = watch.Elapsed.TotalMilliseconds

        watch.Restart()
        let renderedImage = new RgbImage(camera.ResX, camera.ResY)
        let pixel = renderedImage.Pixels
        let linear : Colour[] =
            if captureLinear then Array.zeroCreate (camera.ResX * camera.ResY)
            else Array.empty
        let bufferSetupMs = watch.Elapsed.TotalMilliseconds

        watch.Restart()
        Parallel.For(0, camera.ResY * camera.ResX, fun xy ->
            let y = xy / camera.ResX
            let coordsX = xy % camera.ResX
            let rays = camera.CreateRays coordsX y
            let cols = Array.map (fun ray -> (this.Cast accel ray)) rays
            let colour = (Array.fold (+) Colour.Black cols)/float cols.Length

            let color = colour.ToColor

            pixel.[xy * 3] <- color.R
            pixel.[xy * 3 + 1] <- color.G
            pixel.[xy * 3 + 2] <- color.B
            if captureLinear then
                linear.[(camera.ResY - 1 - y) * camera.ResX + coordsX] <- colour

            ) |> ignore
        let traceMs = watch.Elapsed.TotalMilliseconds

        watch.Restart()
        renderedImage.FlipVertical()
        let outputPreparationMs = bufferSetupMs + watch.Elapsed.TotalMilliseconds

        renderedImage, linear, buildMs, traceMs, outputPreparationMs

    member this.RenderParallel =
        let image, _, _, _, _ = this.RenderPixels false
        image

    member this.RenderWithLinear =
        let image, linear, _, _, _ = this.RenderPixels true
        image, linear

    member this.RenderCaptured : LegacyRenderFilm =
        let image, colours, buildMs, traceMs, outputPreparationMs = this.RenderPixels true
        let watch = Stopwatch.StartNew()
        let linear = Array.zeroCreate<float> (colours.Length * 3)
        for i = 0 to colours.Length - 1 do
            linear.[i * 3] <- colours.[i].R
            linear.[i * 3 + 1] <- colours.[i].G
            linear.[i * 3 + 2] <- colours.[i].B
        { Width = image.Width
          Height = image.Height
          LinearRgb = linear
          Image = image
          BuildMs = buildMs
          TraceMs = traceMs
          OutputPreparationMs = outputPreparationMs + watch.Elapsed.TotalMilliseconds
          PhaseNotes =
              [| "BuildMs measures the original single PreProcessing call. Renderer construction/bound partitioning is outside this capture and must be timed by the caller."
                 "TraceMs includes the original Parallel.For, camera samples, tracing, averaging, original per-pixel Colour.ToColor/RGB8 writes, and optional raw Colour capture; it is not a pure ray-tracing-only measurement."
                 "OutputPreparationMs covers RGB/raw-buffer setup, the original final vertical flip, and packing original Colour values into top-left interleaved RGB float64. PNG/PFM encoding and original cleanup are outside capture." |] }

    member this.Clean (image:RgbImage) =
        image.Dispose()
        Acceleration.listOfAccel <- []
        GC.Collect()

    member this.RenderToFile filename =
        let image = this.RenderParallel
        this.SaveImage(image, filename)
        this.Clean image

    member this.RenderToScreen : unit =
        raise (PlatformNotSupportedException("Window preview is unavailable in legacy-port. Use renderToFile to save a PNG."))
