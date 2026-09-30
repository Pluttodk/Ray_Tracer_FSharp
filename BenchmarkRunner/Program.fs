namespace Tracer.Benchmarks

open System.Diagnostics
open System.Globalization
open Tracer.Basics
open Tracer.Basics.Render
open Tracer.SceneFormat

module Program =
    let private capture (built: BuiltScene) (settings: RenderSettings) acceleration integrator denoise adaptive exposure =
#if LEGACY
        let preparation = Stopwatch.StartNew()
        let renderer = Render(built.Scene, built.Camera)
        let setupMs = preparation.Elapsed.TotalMilliseconds
        let film = renderer.RenderCaptured
        { Width = film.Width; Height = film.Height; LinearRgb = film.LinearRgb; Image = Some film.Image
          BuildMs = setupMs + film.BuildMs; TraceMs = film.TraceMs
          OutputPreparationMs = film.OutputPreparationMs
          Cleanup = fun () -> renderer.Clean film.Image
          PhaseNotes =
            Array.append film.PhaseNotes
                [| "Legacy build also includes shared-adapter preparation and Render construction; PreProcessing is never called separately by the worker."
                   "Linear capture is an output-only hook. It neither repairs RNG/triangle races nor changes the original ray/color computation." |] }
#else
        let strategy =
            match acceleration with
            | "bvh" -> Acceleration.FlatBVH
            | "kdtree" -> Acceleration.KDTree
            | "grid" -> Acceleration.RegularGrid
            | "brute" -> Acceleration.BruteForce
            | _ -> invalidArg (nameof acceleration) "Unknown CPU acceleration."
        let options =
            let kind =
                match integrator with
                | "classic" -> Classic
                | "path" -> Path
                | _ -> invalidArg (nameof integrator) "Unknown integrator."
            { RenderOptions.Default with Threads = settings.Threads; TileSize = settings.TileSize; Seed = settings.Seed
                                         Transfer = settings.Transfer; Acceleration = Some strategy
                                         Integrator = kind; Denoise = (denoise = "on")
                                         AdaptiveThreshold = adaptive; Exposure = exposure }
        let preparation = Stopwatch.StartNew()
        let renderer = Render(built.Scene, built.Camera, options)
        let setupMs = preparation.Elapsed.TotalMilliseconds
        let film = renderer.RenderLinear
        { Width = film.Width; Height = film.Height; LinearRgb = film.Pixels; Image = None
          BuildMs = setupMs + film.BuildMilliseconds; TraceMs = film.TraceMilliseconds; OutputPreparationMs = 0.
          Cleanup = ignore
          PhaseNotes =
            [| "Modern trace is the completed tiled CPU loop, excluding separate scene acceleration and output encoding."
               "bvh selects the flat binned-SAH scene hierarchy. kdtree/grid/brute select scene-level alternatives; mesh BLAS policy remains the corrected mesh renderer's FlatBVH."
               $"Flat scene BVH options: leafSize={options.BvhOptions.LeafSize}, binCount={options.BvhOptions.BinCount}, maxDepth={options.BvhOptions.MaxDepth}; mesh BLAS uses leafSize={FlatBVH.defaultOptions.LeafSize}, binCount={FlatBVH.defaultOptions.BinCount}."
               "Modern cleanup performs no forced GC. Captured managed buffers are released with the isolated worker process." |] }
#endif

    [<EntryPoint>]
    let main args =
        CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
        CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
        Worker.run capture args
