namespace Tracer.Benchmarks

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Text.Json
open Tracer.Imaging
open Tracer.SceneFormat

type RenderCapture =
    { Width: int
      Height: int
      LinearRgb: float array
      Image: RgbImage option
      BuildMs: float
      TraceMs: float
      OutputPreparationMs: float
      Cleanup: unit -> unit
      PhaseNotes: string array }

[<CLIMutable>]
type WorkerMetadata =
    { SchemaVersion: int
      Engine: string
      Runtime: string
      OperatingSystem: string
      ProcessorCount: int
      RequestedThreads: int
      Acceleration: string
      SampleSetCount: int
      RawFormat: string
      CompletedPhases: string array
      PhaseNotes: string array
      Limitations: string array }

module Worker =
#if LEGACY
    let engine = "legacy-port"
    let limitations =
        [| "Frozen legacy math, light transport, mesh hit state, RNG, acceleration and cleanup are intentionally unchanged."
           "Legacy setRandomSeed does not seed every RNG; sample identity depends on scheduling."
           "Triangle barycentrics are mutable shared per-hit state. Parallel legacy images may vary, including with regular sampling."
           "DOTNET_PROCESSOR_COUNT controls the legacy default Parallel.For scheduler, not an exact operating-system thread cap."
           "Legacy retains forced cleanup GC; output-only linear capture increases retained memory compared with the historical display-only path."
           "Only float64 rendering and legacy gamma2 output are supported." |]
#else
    let engine = "cpu"
    let limitations =
        [| "Classic/Whitted transport only; this is not path tracing or indirect global illumination."
           "This CPU worker currently supports float64 arithmetic only. float32 requests fail rather than silently change precision." |]
#endif

    let private ensureParent (path: string) =
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
    let private writeJson path value =
        ensureParent path
        File.WriteAllText(path, JsonSerializer.Serialize(value, SceneFiles.jsonOptions), UTF8Encoding(false))
    let private cpuName () =
        if File.Exists "/proc/cpuinfo" then
            File.ReadLines "/proc/cpuinfo"
            |> Seq.tryFind (fun line -> line.StartsWith "model name")
            |> Option.map (fun line -> line.Substring(line.IndexOf(':') + 1).Trim())
            |> Option.defaultValue (RuntimeInformation.ProcessArchitecture.ToString())
        else RuntimeInformation.ProcessArchitecture.ToString()

    let private options (args: string array) =
        let accepted = Set.ofList [ "--scene"; "--material"; "--settings"; "--output"; "--linear"; "--metrics"; "--acceleration"; "--integrator"; "--denoise"; "--adaptive"; "--exposure" ]
        let result = Dictionary<string, string>(StringComparer.Ordinal)
        if args.Length % 2 <> 0 then invalidArg (nameof args) "Every worker option requires a value."
        for index in 0 .. 2 .. args.Length - 1 do
            let key, value = args.[index], args.[index + 1]
            if not (accepted.Contains key) then invalidArg (nameof args) $"Unknown worker option {key}."
            if result.ContainsKey key then invalidArg (nameof args) $"Duplicate worker option {key}."
            if String.IsNullOrWhiteSpace value || value.StartsWith "--" then invalidArg (nameof args) $"Missing value for {key}."
            result.Add(key, value)
        for key in ((((accepted.Remove "--acceleration").Remove "--integrator").Remove "--denoise").Remove "--adaptive").Remove "--exposure" do
            if not (result.ContainsKey key) then invalidArg (nameof args) $"Required worker option {key} is missing."
#if LEGACY
        if result.ContainsKey "--acceleration" then
            raise (NotSupportedException "The frozen legacy-port worker uses its original kdtree default; --acceleration belongs to modern CPU only.")
        if result.ContainsKey "--integrator" then
            raise (NotSupportedException "The frozen legacy-port worker only implements the original Whitted transport; --integrator belongs to modern CPU only.")
        if result.ContainsKey "--denoise" then
            raise (NotSupportedException "The frozen legacy-port worker has no denoiser; --denoise belongs to modern CPU only.")
        if result.ContainsKey "--adaptive" then
            raise (NotSupportedException "The frozen legacy-port worker samples every pixel uniformly; --adaptive belongs to modern CPU only.")
        if result.ContainsKey "--exposure" then
            raise (NotSupportedException "The frozen legacy-port worker has no exposure control; --exposure belongs to modern CPU only.")
#endif
        result

    let private displayImage (transfer: string) (capture: RenderCapture) : RgbImage =
#if LEGACY
        invalidOp "Legacy capture must supply its original gamma2 image."
#else
        let image = new RgbImage(capture.Width, capture.Height)
        let pixels = image.Pixels
        for index = 0 to capture.Width * capture.Height - 1 do
            let offset = index * 3
            let colour = Tracer.Basics.Colour(capture.LinearRgb.[offset], capture.LinearRgb.[offset + 1], capture.LinearRgb.[offset + 2])
            let display = colour.ToDisplayColor transfer
            pixels.[offset] <- display.R
            pixels.[offset + 1] <- display.G
            pixels.[offset + 2] <- display.B
        image
#endif

    let run (captureRender: BuiltScene -> RenderSettings -> string -> string -> string -> float -> float -> RenderCapture) (args: string array) =
        if args = [| "--help" |] then
            printfn "Usage: --scene <json> --material <matte|phong|mirror|glossy|glass|authored> --settings <json> --output <png> --linear <pfm> --metrics <json> [--acceleration <kdtree|bvh|grid|brute> (modern CPU only)] [--integrator <classic|path> (modern CPU only)] [--denoise <on|off>] [--adaptive <relative error, e.g. 0.01>] [--exposure <multiplier>] (modern CPU path tracer only)"
            0
        else
            let total = Stopwatch.StartNew()
            let allocatedBefore = GC.GetTotalAllocatedBytes true
            let gcBefore = Array.init 3 GC.CollectionCount
            let completed = ResizeArray<string>()
            let mutable settings = Unchecked.defaultof<RenderSettings>
            let mutable sceneId, variant, acceleration, outputPath, linearPath, metricsPath = "", "", "kdtree", "", "", ""
            let mutable integrator = "classic"
            let mutable denoise = "off"
            let mutable adaptive = 0.0
            let mutable exposure = 1.0
            let mutable metricsDestinationValidated = false
            let mutable loadMs, buildMs, traceMs, encodeMs, cleanupMs, invalidPixels = 0., 0., 0., 0., 0., 0
            let mutable capture: RenderCapture option = None
            let mutable phaseNotes = [||]
            let mutable cleaned = false
            let cleanup () =
                if not cleaned then
                    cleaned <- true
                    match capture with
                    | None -> ()
                    | Some value ->
                        let watch = Stopwatch.StartNew()
                        try value.Cleanup()
                        finally
                            value.Image |> Option.iter (fun image -> image.Dispose())
                            cleanupMs <- watch.Elapsed.TotalMilliseconds
                            completed.Add "cleanup"
            let emit status error =
                total.Stop()
                use current = Process.GetCurrentProcess()
                current.Refresh()
                let timings =
                    { LoadMs = loadMs; BuildMs = (if completed.Contains "build" then Nullable buildMs else Nullable())
                      CompileMs = Nullable(); UploadMs = Nullable(); TraceMs = traceMs; DownloadMs = Nullable()
                      EncodeMs = encodeMs; CleanupMs = cleanupMs; TotalMs = total.Elapsed.TotalMilliseconds }
                let metrics =
                    { SchemaVersion = 1; Engine = engine; Backend = "cpu/" + acceleration + "/" + integrator + (if denoise = "on" then "/denoised" else ""); Device = cpuName()
                      Scene = sceneId; Material = variant; Status = status; Error = error; Settings = settings; Timings = timings
                      AllocatedBytes = Nullable(GC.GetTotalAllocatedBytes(true) - allocatedBefore)
                      PeakWorkingSetBytes = current.PeakWorkingSet64; PeakDeviceBytes = Nullable()
                      GcCollections = Array.init 3 (fun generation -> GC.CollectionCount generation - gcBefore.[generation])
                      InvalidPixels = invalidPixels; OutputPath = outputPath; LinearPath = linearPath }
                if metricsDestinationValidated then
                    writeJson metricsPath metrics
                    let metadata =
                        { SchemaVersion = 1; Engine = engine; Runtime = RuntimeInformation.FrameworkDescription
                          OperatingSystem = RuntimeInformation.OSDescription; ProcessorCount = Environment.ProcessorCount
                          RequestedThreads = if obj.ReferenceEquals(settings, null) then 0 else settings.Threads
                          Acceleration = acceleration; SampleSetCount = if obj.ReferenceEquals(settings, null) || settings.Sampler = "regular" then 1 else 83
                          RawFormat = "PFM RGB float32 little-endian (-1.0), bottom-up rows on disk; unclamped linear radiance, float64 renderer arithmetic."
                          CompletedPhases = completed.ToArray()
                          PhaseNotes = Array.append phaseNotes
                              [| "Load includes JSON, texture decode and mkPLY, including any mesh acceleration constructed inside that public API. Build includes remaining adapter/mesh preparation and separately exposed renderer acceleration."
                                 "Encode includes output conversion, PNG encoding and retained linear PFM serialization."
                                 "Total excludes final metrics/metadata serialization; subprocess cold time includes those and process exit."
                                 "For failed runs, numeric durations describe work observed before failure, not completed phases. Use completedPhases/status; do not aggregate them." |]
                          Limitations = limitations }
                    writeJson (metricsPath + ".worker.json") metadata
            try
                let values = options args
                let scenePath, settingsPath = Path.GetFullPath values.["--scene"], Path.GetFullPath values.["--settings"]
                sceneId <- Path.GetFileNameWithoutExtension scenePath
                outputPath <- Path.GetFullPath values.["--output"]
                linearPath <- Path.GetFullPath values.["--linear"]
                metricsPath <- Path.GetFullPath values.["--metrics"]
                variant <- values.["--material"]
                let load = Stopwatch.StartNew()
                let specification = SceneFiles.load scenePath
                let destinations = [| outputPath; linearPath; metricsPath; metricsPath + ".worker.json" |] |> Array.map canonicalPath
                let comparer = if OperatingSystem.IsWindows() then StringComparer.OrdinalIgnoreCase else StringComparer.Ordinal
                let paths = HashSet<string>(comparer)
                if Array.append [| canonicalPath scenePath; canonicalPath settingsPath |] destinations
                   |> Array.exists (fun path -> not (paths.Add path)) then
                    invalidArg "paths" "Inputs, PNG, linear output, metrics and worker metadata must have distinct paths."
                let assets =
                    Array.append
                        (specification.Meshes |> Array.map (fun mesh -> SceneFiles.resolveAsset scenePath mesh.Path))
                        (specification.Materials |> Array.filter (fun material -> not (String.IsNullOrWhiteSpace material.Texture))
                                                 |> Array.map (fun material -> SceneFiles.resolveAsset scenePath material.Texture))
                    |> Array.map canonicalPath
                let inputs = HashSet<string>(assets, comparer)
                if destinations |> Array.exists inputs.Contains then
                    invalidArg "paths" "Output paths must not overwrite scene assets."
                metricsDestinationValidated <- true
                if not (Array.contains variant SceneFiles.materialVariants) then invalidArg "--material" $"Unsupported variant {variant}."
                use stream = File.OpenRead settingsPath
                settings <- JsonSerializer.Deserialize<RenderSettings>(stream, SceneFiles.jsonOptions) |> SceneFiles.validateSettings
                if settings.Precision <> "float64" then raise (NotSupportedException $"{engine} does not implement {settings.Precision} arithmetic.")
#if LEGACY
                if settings.Transfer <> "gamma2" then raise (NotSupportedException "legacy-port preserves gamma2; other transfer settings are unsupported.")
                if settings.Threads <> Environment.ProcessorCount then
                    raise (NotSupportedException $"Legacy thread settings require launching with DOTNET_PROCESSOR_COUNT={settings.Threads}; this process sees {Environment.ProcessorCount}.")
#else
                acceleration <- match values.TryGetValue "--acceleration" with true, value -> value | _ -> "bvh"
                integrator <- match values.TryGetValue "--integrator" with true, value -> value | _ -> "classic"
                if not (Array.contains integrator [| "classic"; "path" |]) then invalidArg "--integrator" "Unknown integrator."
                denoise <- match values.TryGetValue "--denoise" with true, value -> value | _ -> "off"
                if not (Array.contains denoise [| "on"; "off" |]) then invalidArg "--denoise" "Use --denoise on|off."
                adaptive <-
                    match values.TryGetValue "--adaptive" with
                    | true, value ->
                        match Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                        | true, parsed when Double.IsFinite parsed && parsed >= 0. && parsed < 1. -> parsed
                        | _ -> invalidArg "--adaptive" "Use --adaptive <relative error in [0,1)>, e.g. 0.01."
                    | _ -> 0.0
                exposure <-
                    match values.TryGetValue "--exposure" with
                    | true, value ->
                        match Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                        | true, parsed when Double.IsFinite parsed && parsed > 0. -> parsed
                        | _ -> invalidArg "--exposure" "Use --exposure <positive multiplier>, e.g. 8."
                    | _ -> 1.0 
                if not (Array.contains acceleration [| "bvh"; "kdtree"; "grid"; "brute" |]) then invalidArg "--acceleration" "Unknown CPU acceleration."
#endif
                let decode (path: string) =
                    use image = RgbImage.Load path
                    { Width = image.Width; Height = image.Height; Pixels = Array.copy image.Pixels }
                let settingsLoadMs = load.Elapsed.TotalMilliseconds
                let built = SceneBuilder.buildLoaded scenePath variant settings decode specification
                sceneId <- built.Specification.Id
                loadMs <- settingsLoadMs + built.LoadMs
                buildMs <- built.BuildMs
                completed.Add "load"
                let rendered = captureRender built settings acceleration integrator denoise adaptive exposure
                capture <- Some rendered
                phaseNotes <- rendered.PhaseNotes
                buildMs <- buildMs + rendered.BuildMs
                traceMs <- rendered.TraceMs
                completed.Add "build"
                completed.Add "trace"
                let encode = Stopwatch.StartNew()
                if rendered.Width <> settings.Width || rendered.Height <> settings.Height
                   || int64 rendered.LinearRgb.Length <> int64 settings.Width * int64 settings.Height * 3L then
                    raise (InvalidDataException "Renderer returned an incorrectly sized linear film.")
                let invalid index =
                    let value = rendered.LinearRgb.[index]
                    not (Double.IsFinite value) || value < 0. || not (Single.IsFinite(float32 value))
                for pixel = 0 to rendered.Width * rendered.Height - 1 do
                    let index = pixel * 3
                    if invalid index || invalid (index + 1) || invalid (index + 2) then
                        invalidPixels <- invalidPixels + 1
                if invalidPixels > 0 then raise (InvalidDataException $"Renderer produced {invalidPixels} invalid or PFM-unrepresentable pixels.")
                ensureParent outputPath
                match rendered.Image with
                | Some image -> image.SavePng outputPath
                | None ->
                    use image = displayImage settings.Transfer rendered
                    image.SavePng outputPath
                LinearOutput.writePfm linearPath rendered.Width rendered.Height rendered.LinearRgb
                encodeMs <- rendered.OutputPreparationMs + encode.Elapsed.TotalMilliseconds
                completed.Add "encode"
                cleanup ()
                emit "success" ""
                if not (File.Exists outputPath && File.Exists linearPath && File.Exists metricsPath) then
                    raise (IOException "A required output disappeared before worker completion.")
                0
            with ex ->
                try cleanup () with cleanupError -> eprintfn "worker cleanup: %s" cleanupError.Message
                eprintfn "%s worker: %s" engine (ex.ToString())
                try emit (if ex :? NotSupportedException then "unavailable" else "failure") ex.Message
                with metricsError -> eprintfn "Unable to write failure metrics: %s" metricsError.Message
                1
