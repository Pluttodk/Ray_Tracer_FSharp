namespace Tracer.Gpu

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open Tracer.Basics
open Tracer.Imaging
open Tracer.SceneFormat

module Worker =
    let usage =
        "RayTracer.Gpu --scene <json> --material <matte|phong|mirror|glossy|glass|authored> --settings <json> --output <png> --linear <pfm> --metrics <json>"

    let private parse (arguments: string array) =
        let supported = Set.ofList [ "--scene"; "--material"; "--settings"; "--output"; "--linear"; "--metrics" ]
        if arguments.Length <> supported.Count * 2 then invalidArg "arguments" usage
        let pairs =
            arguments
            |> Array.chunkBySize 2
            |> Array.map (fun pair ->
                if not (Set.contains pair.[0] supported) || String.IsNullOrWhiteSpace pair.[1] then
                    invalidArg "arguments" usage
                pair.[0], pair.[1])
        if (pairs |> Array.map fst |> Array.distinct).Length <> supported.Count then
            invalidArg "arguments" "Each worker option must occur exactly once."
        Map.ofArray pairs

    let private ensureParent path =
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path)) |> ignore

    let private canonicalPath (path: string) =
        let full = Path.GetFullPath path
        let root = Path.GetPathRoot full
        let parts = full.Substring(root.Length).Split([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |], StringSplitOptions.RemoveEmptyEntries)
        (root, parts) ||> Array.fold (fun parent name ->
            let candidate = Path.Combine(parent, name)
            let info: FileSystemInfo =
                if Directory.Exists candidate then DirectoryInfo(candidate) :> FileSystemInfo
                else FileInfo(candidate) :> FileSystemInfo
            if not info.Exists then candidate
            else
                match info.ResolveLinkTarget true with
                | null -> candidate
                | target -> target.FullName)

    let private writeMetrics path (metrics: RenderMetrics) =
        ensureParent path
        let pending = path + ".gpu-pending"
        try
            File.WriteAllText(pending, JsonSerializer.Serialize(metrics, SceneFiles.jsonOptions) + Environment.NewLine)
            File.Move(pending, path, true)
        finally
            if File.Exists pending then File.Delete pending

    let private encode (outputPath: string) (linearPath: string) (settings: RenderSettings) (pixels: V3 array) =
        if Path.GetExtension(outputPath).ToLowerInvariant() <> ".png" then invalidArg "output" "GPU output must be a PNG file."
        if Path.GetExtension(linearPath).ToLowerInvariant() <> ".pfm" then invalidArg "linear" "GPU linear output must use the documented RGB32 PFM format (.pfm)."
        if pixels.LongLength <> int64 settings.Width * int64 settings.Height then
            invalidOp "GPU output dimensions do not match the rendered pixel count."
        let componentCount = pixels.LongLength * 3L
        if componentCount > int64 Array.MaxLength then
            invalidOp "GPU output exceeds the supported packed RGB array size."
        ensureParent outputPath
        ensureParent linearPath
        let pendingOutput, pendingLinear = outputPath + ".gpu-pending", linearPath + ".gpu-pending"
        try
            use image = new RgbImage(settings.Width, settings.Height)
            let linearRgb = Array.zeroCreate<float> (int componentCount)
            for index = 0 to pixels.Length - 1 do
                let pixel = pixels.[index]
                // The device computes at FP32; the output boundary is FP64
                // Colour and an FP32 PFM. Widening here is exact.
                let display = Colour(double pixel.X, double pixel.Y, double pixel.Z).ToDisplayColor settings.Transfer
                image.Pixels.[index * 3] <- display.R
                image.Pixels.[index * 3 + 1] <- display.G
                image.Pixels.[index * 3 + 2] <- display.B
                linearRgb.[index * 3] <- double pixel.X
                linearRgb.[index * 3 + 1] <- double pixel.Y
                linearRgb.[index * 3 + 2] <- double pixel.Z
            image.SavePng pendingOutput
            Tracer.Benchmarks.LinearOutput.writePfm pendingLinear settings.Width settings.Height linearRgb
            if FileInfo(pendingOutput).Length = 0L || FileInfo(pendingLinear).Length = 0L then
                invalidOp "GPU encoding did not produce valid nonempty output files."
            File.Move(pendingOutput, outputPath, true)
            File.Move(pendingLinear, linearPath, true)
        finally
            if File.Exists pendingOutput then File.Delete pendingOutput
            if File.Exists pendingLinear then File.Delete pendingLinear

    let run (arguments: string array) =
        let total = Stopwatch.StartNew()
        let allocatedStart = GC.GetTotalAllocatedBytes true
        let collections = Array.init 3 GC.CollectionCount
        let mutable metricPath = ""
        let mutable metrics: RenderMetrics =
            { SchemaVersion = 1; Engine = "gpu"; Backend = "cuda-compute"; Device = ""
              Scene = ""; Material = ""; Status = "failure"; Error = ""
              Settings = SceneFiles.preset "quick"
              Timings =
                { LoadMs = 0.; BuildMs = Nullable(); CompileMs = Nullable(); UploadMs = Nullable()
                  TraceMs = 0.; DownloadMs = Nullable(); EncodeMs = 0.; CleanupMs = 0.; TotalMs = 0. }
              AllocatedBytes = Nullable(); PeakWorkingSetBytes = 0L; PeakDeviceBytes = Nullable()
              GcCollections = [| 0; 0; 0 |]; InvalidPixels = 0; OutputPath = ""; LinearPath = "" }
        try
            let options = parse arguments
            let requestedMetrics = Path.GetFullPath options.["--metrics"]
            let outputPath = Path.GetFullPath options.["--output"]
            let linearPath = Path.GetFullPath options.["--linear"]
            let scenePath = Path.GetFullPath options.["--scene"]
            let settingsPath = Path.GetFullPath options.["--settings"]
            let outputs = [| outputPath; linearPath; requestedMetrics |]
            let comparer = if OperatingSystem.IsWindows() then StringComparer.OrdinalIgnoreCase else StringComparer.Ordinal
            let destinations =
                Array.append outputs (outputs |> Array.map (fun path -> path + ".gpu-pending"))
                |> Array.map canonicalPath
            let inputs = HashSet<string>([| canonicalPath scenePath; canonicalPath settingsPath |], comparer)
            if HashSet<string>(destinations, comparer).Count <> destinations.Length ||
               (destinations |> Array.exists inputs.Contains) then
                invalidArg "output" "PNG, PFM, metrics and temporary paths must be distinct and must not replace input files."
            metrics <- { metrics with Scene = scenePath; Material = options.["--material"]; OutputPath = outputPath; LinearPath = linearPath }
            // Failure metrics are unsafe until every authored asset path is known.
            let sceneRead = Stopwatch.StartNew()
            let specification = SceneFiles.load scenePath
            let sceneReadMs = sceneRead.Elapsed.TotalMilliseconds
            for mesh in specification.Meshes do
                inputs.Add(canonicalPath (SceneFiles.resolveAsset scenePath mesh.Path)) |> ignore
            for material in specification.Materials do
                if not (String.IsNullOrWhiteSpace material.Texture) then
                    inputs.Add(canonicalPath (SceneFiles.resolveAsset scenePath material.Texture)) |> ignore
            if destinations |> Array.exists inputs.Contains then
                invalidArg "output" "GPU output paths must not overwrite scene assets."
            metricPath <- requestedMetrics
            if Path.GetExtension(outputPath).ToLowerInvariant() <> ".png" ||
               Path.GetExtension(linearPath).ToLowerInvariant() <> ".pfm" then
                invalidArg "output" "CUDA output paths require .png and .pfm extensions."
            use settingsStream = File.OpenRead settingsPath
            let settings = JsonSerializer.Deserialize<RenderSettings>(settingsStream, SceneFiles.jsonOptions)
            if obj.ReferenceEquals(settings, null) then invalidArg "settings" "Render settings must be a JSON object."
            metrics <- { metrics with Settings = settings }
            SceneFiles.validateSettings settings |> ignore
            let loaded = HostScene.loadWith scenePath options.["--material"] settings (fun () -> specification)
            metrics <-
                { metrics with Scene = loaded.Spec.Id
                               Timings = { metrics.Timings with LoadMs = sceneReadMs + loaded.LoadMs; BuildMs = Nullable loaded.BuildMs } }
            let rendered = CudaRenderer.render loaded.Prepared settings
            metrics <-
                { metrics with
                    Device = rendered.Device; PeakDeviceBytes = Nullable rendered.PeakDeviceBytes
                    Timings =
                        { metrics.Timings with
                            CompileMs = Nullable rendered.CompileMs; UploadMs = Nullable rendered.UploadMs
                            TraceMs = rendered.TraceMs; DownloadMs = Nullable rendered.DownloadMs
                            CleanupMs = rendered.CleanupMs } }
            let encoding = Stopwatch.StartNew()
            let invalidPixels =
                rendered.Pixels
                |> Array.sumBy (fun pixel ->
                    if Single.IsFinite(single pixel.X) && Single.IsFinite(single pixel.Y) && Single.IsFinite(single pixel.Z) then 0 else 1)
            if invalidPixels > 0 then
                metrics <- { metrics with InvalidPixels = invalidPixels }
                invalidOp $"{invalidPixels} CUDA pixels exceed finite RGB32 PFM storage."
            encode outputPath linearPath settings rendered.Pixels
            metrics <-
                { metrics with
                    Status = "success"; Error = ""
                    Timings = { metrics.Timings with EncodeMs = encoding.Elapsed.TotalMilliseconds; TotalMs = total.Elapsed.TotalMilliseconds }
                    AllocatedBytes = Nullable(GC.GetTotalAllocatedBytes(true) - allocatedStart)
                    PeakWorkingSetBytes = Process.GetCurrentProcess().PeakWorkingSet64
                    GcCollections = Array.init 3 (fun generation -> GC.CollectionCount generation - collections.[generation]) }
            writeMetrics metricPath metrics
            printfn "%s" (JsonSerializer.Serialize(
                {| status = "success"; backend = metrics.Backend; device = metrics.Device; metrics = metricPath
                   batchPixels = rendered.BatchPixels; batches = rendered.Batches
                   skippedDegenerateTriangles = loaded.SkippedDegenerateTriangles
                   rayStackCapacity = rendered.RayStackCapacity; mediumStackCapacity = rendered.MediumStackCapacity
                   traversalStackCapacity = rendered.TraversalStackCapacity |}))
            0
        with error ->
            metrics <-
                { metrics with Status = if error :? NotSupportedException then "unavailable" else "failure"
                               Error = error.Message
                               InvalidPixels = match error with :? CudaExecutionException as cuda -> cuda.InvalidPixels | _ -> metrics.InvalidPixels
                               Timings = { metrics.Timings with TotalMs = total.Elapsed.TotalMilliseconds }
                               AllocatedBytes = Nullable(GC.GetTotalAllocatedBytes(true) - allocatedStart)
                               PeakWorkingSetBytes = Process.GetCurrentProcess().PeakWorkingSet64
                               GcCollections = Array.init 3 (fun generation -> GC.CollectionCount generation - collections.[generation]) }
            if not (String.IsNullOrWhiteSpace metricPath) then
                try writeMetrics metricPath metrics
                with metricsError ->
                    eprintfn "%s" (JsonSerializer.Serialize({| status = "error"; backend = "cuda-compute"; error = metricsError.Message |}))
            eprintfn "%s" (JsonSerializer.Serialize(metrics, SceneFiles.jsonOptions))
            1
