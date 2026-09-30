#load "../SceneFormat/SceneFormat.fs"
#load "../BenchmarkRunner/ScenePolicy.fs"
#load "BenchmarkData.fs"
#load "BenchmarkReport.fs"

open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Text.Json
open System.Threading
open Tracer.SceneFormat
open Tracer.Benchmarks
open Tracer.Benchmarks.Reporting

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let args = fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--")
let help = """Classic F# renderer process-isolated benchmark

dotnet fsi scripts/benchmark.fsx -- --presets standard,high --scenes all --materials all --engines legacy,cpu,gpu --output artifacts/benchmarks

Matrix:
  --presets quick|standard|high[,..]  Default standard,high
  --scenes all|chair,roman-bust,space-sentinel,sky-arena
                                    all remains exactly the four-scene matrix
  --scenes gold-dragon --materials authored
                                    Separate bonus; requires prepared scene/assets
  --scene FILE                      One custom neutral scene instead of --scenes
  --scene-dir DIR                    Default benchmarks/scenes
  --materials all|matte,phong,mirror,glossy,glass,authored
  --engines legacy,cpu,gpu           No implicit GPU fallback
  --output DIR                      Default artifacts/benchmarks

Execution:
  --resume                          Reuse only complete, hash-verified identical fingerprints
  --repeats N                       Override measured repeats (quick=1; standard/high=5)
  --warmups N                       Separate isolated warmups per case/engine (default 1)
  --timeout-seconds N                Exact worker process deadline (default 14400)
  --build-timeout-seconds N          Preparation/build deadline (default 1200)
  --skip-build                      Use existing assemblies; hashes retained, clearly labeled
  --skip-prepare                    Do not run offline procedural asset preparation
  --prepare-assets                  Regenerate assets even for --scene (outside measurements)
  --report-only                     Rebuild HTML/summary from retained trials, no workers
  --dotnet FILE                     SDK host (default DOTNET_HOST_PATH/current process host)
  --legacy-worker DLL --cpu-worker DLL --gpu-worker DLL
                                    Explicit worker assemblies, never shell commands
  --acceleration bvh|kdtree|grid|brute  Modern CPU only (default bvh)

Targeted settings (every override is fingerprinted):
  --settings FILE                    Base RenderSettings JSON
  --width N --height N --camera-samples N --light-samples N --glossy-samples N
  --max-bounces N --threads N --tile-size N --seed N
  --precision float64|float32 --sampler regular|multi-jittered
  --transfer gamma2|srgb|linear

Each repeat retains PNG, unclamped RGB PFM, metrics, stdout, stderr and trial JSON.
Failures/timeouts/incomplete outputs remain in reports and make the exit nonzero.
Ctrl+C/SIGTERM kills only the current owned worker process tree and saves results.
No downloads occur inside worker measurements. No images or reports are uploaded.
"""

let flags = Set.ofList [ "--resume"; "--skip-build"; "--skip-prepare"; "--prepare-assets"; "--report-only"; "--help" ]
let valueOptions =
    Set.ofList
        [ "--presets"; "--scenes"; "--scene"; "--scene-dir"; "--materials"; "--engines"; "--output"; "--repeats"; "--warmups"
          "--timeout-seconds"; "--build-timeout-seconds"; "--dotnet"; "--legacy-worker"; "--cpu-worker"; "--gpu-worker"
          "--acceleration"; "--settings"; "--width"; "--height"; "--camera-samples"; "--light-samples"; "--glossy-samples"
          "--max-bounces"; "--threads"; "--tile-size"; "--seed"; "--precision"; "--sampler"; "--transfer" ]

let parse () =
    let values = Dictionary<string, string>(StringComparer.Ordinal)
    let mutable index = 0
    while index < args.Length do
        let name = args.[index]
        if values.ContainsKey name then invalidArg "arguments" $"Duplicate option {name}."
        if flags.Contains name then
            values.Add(name, "true")
            index <- index + 1
        elif valueOptions.Contains name then
            if index + 1 >= args.Length then invalidArg "arguments" $"Missing value for {name}."
            values.Add(name, args.[index + 1])
            index <- index + 2
        else invalidArg "arguments" $"Unknown option {name}. Use --help."
    values

let execute (options: Dictionary<string, string>) =
    let get (name: string) (fallback: string) = match options.TryGetValue name with true, value -> value | _ -> fallback
    let has name = options.ContainsKey name
    let path (value: string) = if Path.IsPathRooted value then Path.GetFullPath value else Path.GetFullPath(Path.Combine(root, value))
    let integer name fallback = if has name then Int32.Parse(get name "", Data.invariant) else fallback
    let positiveSeconds name fallback =
        let value = if has name then Double.Parse(get name "", Data.invariant) else fallback
        if not (Double.IsFinite value) || value <= 0. || value > float Int32.MaxValue / 1000. then
            invalidArg name "Timeout must be finite, positive, and at most 2147483 seconds."
        value
    let choose name fallback (allowed: string array) =
        let text = get name fallback
        let values = if text = "all" then allowed else text.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        if values.Length = 0 || (Array.distinct values).Length <> values.Length
           || values |> Array.exists (fun value -> not (Array.contains value allowed)) then
            invalidArg name $"""Expected a nonempty unique selection from {String.concat "," allowed}."""
        values
    let output = path (get "--output" "artifacts/benchmarks")
    let gitDirectory = Path.Combine(root, ".git")
    if output = root || output = gitDirectory
       || output.StartsWith(gitDirectory + string Path.DirectorySeparatorChar, StringComparison.Ordinal) then
        invalidArg "--output" "Choose a dedicated artifacts directory, not the repository root or .git."
    Directory.CreateDirectory output |> ignore
    if has "--report-only" then
        let manifest = Data.deserialize<RunManifest> (Path.Combine(output, "run-manifest.json"))
        let trials =
            Data.deserialize<Trial array> (Path.Combine(output, "trials.json"))
            |> Array.map (fun trial ->
                if trial.Status = "success" && not (Data.filesIntact trial.Files) then
                    { trial with Status = "corrupt"; Error = "A retained artifact is missing, empty, or differs from its recorded hash." }
                else trial)
        let damaged = trials |> Array.exists (fun trial -> trial.Status = "corrupt")
        let manifest = if damaged then { manifest with Status = "incomplete" } else manifest
        Data.writeJson (Path.Combine(output, "run-manifest.json")) manifest
        let reportErrors = Report.write output manifest trials
        printfn "Report: %s" (Path.Combine(output, "index.html"))
        if damaged || reportErrors || manifest.Status <> "complete" || trials |> Array.exists (fun trial -> trial.Status <> "success") then 1 else 0
    else
        use cancellation = new CancellationTokenSource()
        let cancelHandler = ConsoleCancelEventHandler(fun _ event -> event.Cancel <- true; cancellation.Cancel())
        Console.CancelKeyPress.AddHandler cancelHandler
        use signal =
            if OperatingSystem.IsWindows() then Unchecked.defaultof<PosixSignalRegistration>
            else PosixSignalRegistration.Create(PosixSignal.SIGTERM, fun context -> context.Cancel <- true; cancellation.Cancel())
        try
            let presets = choose "--presets" "standard,high" [| "quick"; "standard"; "high" |]
            let engines = choose "--engines" "legacy,cpu,gpu" [| "legacy"; "cpu"; "gpu" |]
            let materials = choose "--materials" "all" SceneFiles.materialVariants
            let sceneIds = ScenePolicy.selectScenes (get "--scenes" "all")
            if has "--scene" && has "--scenes" then invalidArg "--scene" "Use --scene or --scenes, not both."
            if not (has "--scene") then
                for sceneId in sceneIds do ScenePolicy.validateVariants sceneId materials
            let warmups = integer "--warmups" 1
            if warmups < 0 then invalidArg "--warmups" "Warmups must be nonnegative."
            let repeats preset =
                let value = integer "--repeats" (if preset = "quick" then 1 else 5)
                if value <= 0 then invalidArg "--repeats" "Measured repeats must be positive."
                value
            let timeout = positiveSeconds "--timeout-seconds" 14400.
            let buildTimeout = positiveSeconds "--build-timeout-seconds" 1200.
            let acceleration = get "--acceleration" "bvh"
            if not (Array.contains acceleration [| "bvh"; "kdtree"; "grid"; "brute" |]) then invalidArg "--acceleration" "Unknown CPU acceleration."
            let host =
                let env = Environment.GetEnvironmentVariable "DOTNET_HOST_PATH"
                if not (String.IsNullOrWhiteSpace env) then env
                elif not (String.IsNullOrWhiteSpace Environment.ProcessPath) && Path.GetFileNameWithoutExtension(Environment.ProcessPath) = "dotnet" then Environment.ProcessPath
                elif File.Exists(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".dotnet", "dotnet")) then
                    Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".dotnet", "dotnet")
                else "dotnet"
            let dotnet = get "--dotnet" host
            let preparation = Path.Combine(output, "preparation", DateTime.UtcNow.ToString("yyyyMMddTHHmmssfffffff", Data.invariant))
            Directory.CreateDirectory preparation |> ignore
            let probe executable arguments =
                let result = Data.runProcess root executable arguments [] buildTimeout cancellation.Token
                if result.Status = "exited" && result.ExitCode = Nullable 0 then result.Stdout.Trim()
                else $"unavailable ({result.Status}): {result.Error} {result.Stderr}".Trim()
            let prepare name arguments =
                printfn "Preparing %s (outside measured work)..." name
                let result = Data.runProcess root dotnet arguments [] buildTimeout cancellation.Token
                Data.writeText (Path.Combine(preparation, name + ".stdout.log")) result.Stdout
                Data.writeText (Path.Combine(preparation, name + ".stderr.log")) result.Stderr
                Data.writeJson (Path.Combine(preparation, name + ".json")) result
                if result.Status <> "exited" || result.ExitCode <> Nullable 0 then
                    Error $"{name}: {result.Status}, exit {result.ExitCode}: {result.Error} {result.Stderr} {result.Stdout}"
                else Ok ()
            let build name project =
                if not (File.Exists project) then Error $"Worker project unavailable: {project}"
                else prepare name [ "build"; project; "-c"; "Release"; "--nologo"; "-v"; "quiet" ]
            let sceneDirectory = path (get "--scene-dir" "benchmarks/scenes")
            let scenePaths =
                if has "--scene" then [| path (get "--scene" "") |]
                else sceneIds |> Array.map (fun id -> Path.Combine(sceneDirectory, id + ".json"))
            let prepareAssets =
                has "--prepare-assets" || (not (has "--scene") && ScenePolicy.usesProceduralAssets sceneIds)
            if not (has "--skip-prepare") && prepareAssets then
                let project = path "SceneAssets/SceneAssets.fsproj"
                if File.Exists project then
                    match build "scene-assets-build" project with
                    | Error error -> failwith error
                    | Ok () ->
                        match prepare "scene-assets-generate"
                            [ path "SceneAssets/bin/Release/net10.0/SceneAssets.dll"; "generate" ] with
                        | Error error -> failwith error
                        | Ok () -> ()
                elif scenePaths |> Array.exists (File.Exists >> not) then
                    failwith "Offline SceneAssets project and at least one scene are missing; prepare assets before rendering."
            if not (has "--scene") && Array.contains "gold-dragon" sceneIds
               && not (File.Exists(Path.Combine(sceneDirectory, "gold-dragon.json"))) then
                failwith "Gold Dragon requires a prepared neutral scene at <scene-dir>/gold-dragon.json and its verified Stanford asset. benchmarks/gold-dragon.json is provenance, not a SceneSpec. Acquisition is not performed by the benchmark."
            if Array.contains "legacy" engines then
                let checksums = path "benchmarks/legacy/source.sha256"
                if not (File.Exists checksums) then failwith "Frozen legacy source checksum manifest is missing."
                for line in File.ReadLines checksums do
                    if not (String.IsNullOrWhiteSpace line) then
                        let fields = line.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)
                        if fields.Length <> 2 || SceneFiles.hashFile (path (fields.[1].TrimStart('*', ' '))) <> fields.[0] then
                            failwith $"Frozen legacy source integrity check failed: {line}"
            let defaultWorkers =
                Map.ofList
                    [ "legacy", ("benchmarks/legacy/Runner/LegacyRunner.fsproj", "benchmarks/legacy/Runner/bin/Release/net10.0/LegacyRunner.dll")
                      "cpu", ("BenchmarkRunner/BenchmarkRunner.fsproj", "BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll")
                      "gpu", ("RayTracer.Gpu/RayTracer.Gpu.fsproj", "RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll") ]
            let workers, buildErrors =
                engines
                |> Array.map (fun engine ->
                    let project, binary = defaultWorkers.[engine]
                    let worker = path (get ("--" + engine + "-worker") binary)
                    let built =
                        if has ("--" + engine + "-worker") || has "--skip-build" then Ok ()
                        else build (engine + "-build") (path project)
                    let error =
                        match built with
                        | Error error -> error
                        | Ok () when not (File.Exists worker) -> $"Worker assembly is unavailable: {worker}"
                        | Ok () -> ""
                    (engine, worker), (engine, error))
                |> Array.unzip
            let workers, buildErrors = Map.ofArray workers, Map.ofArray buildErrors
            let sdk = probe dotnet [ "--info" ]
            if sdk.StartsWith "unavailable" then failwith sdk
            let gitCommit = probe "git" [ "--no-pager"; "rev-parse"; "HEAD" ]
            let gitStatus = probe "git" [ "--no-pager"; "status"; "--porcelain=v1"; "--untracked-files=all" ]
            let sourceNames =
                let result = Data.runProcess root "git" [ "ls-files"; "-c"; "-o"; "--exclude-standard"; "-z" ] [] buildTimeout cancellation.Token
                if result.Status <> "exited" || result.ExitCode <> Nullable 0 then failwith $"Cannot fingerprint source: {result.Error} {result.Stderr}"
                result.Stdout.Split('\000', StringSplitOptions.RemoveEmptyEntries) |> Array.distinct
            let excludedPrefix = output.TrimEnd(Path.DirectorySeparatorChar) + string Path.DirectorySeparatorChar
            let sources =
                sourceNames
                |> Array.filter (fun name ->
                    let full = path name
                    let segments = name.Replace('\\', '/').Split('/')
                    not (full.StartsWith(excludedPrefix, StringComparison.Ordinal))
                    && not (segments |> Array.exists (fun part -> part = "obj" || part = "bin" || part = "artifacts" || part = ".git"))
                    && (Set.ofList [ ".fs"; ".fsi"; ".fsx"; ".fsproj"; ".props"; ".targets"; ".json"; ".config"; ".yaml"; ".yml"; ".sha256"; ".sh"; ".md" ]).Contains(Path.GetExtension name))
                |> Array.sort
                |> Array.map (fun name -> let file = Data.hashFile (path name) in { file with Path = name.Replace('\\', '/') })
            let dependencies =
                workers
                |> Map.toArray
                |> Array.collect (fun (_, worker) ->
                    let directory = Path.GetDirectoryName worker
                    if Directory.Exists directory then
                        Directory.EnumerateFiles directory
                        |> Seq.filter (fun file -> file.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                                                   || file.EndsWith(".deps.json", StringComparison.Ordinal)
                                                   || file.EndsWith(".runtimeconfig.json", StringComparison.Ordinal))
                        |> Seq.map Data.hashFile |> Seq.toArray
                    else [||])
                |> Array.distinctBy (fun file -> file.Path)
                |> Array.sortBy (fun file -> file.Path)
            let cpu =
                if File.Exists "/proc/cpuinfo" then
                    File.ReadLines "/proc/cpuinfo" |> Seq.tryFind (fun line -> line.StartsWith "model name")
                    |> Option.defaultValue (RuntimeInformation.ProcessArchitecture.ToString())
                else Environment.GetEnvironmentVariable "PROCESSOR_IDENTIFIER"
            let gpuExecutable = if File.Exists "/usr/lib/wsl/lib/nvidia-smi" then "/usr/lib/wsl/lib/nvidia-smi" else "nvidia-smi"
            let gpu = probe gpuExecutable [ "--query-gpu=name,uuid,driver_version,memory.total"; "--format=csv,noheader" ]
            let runtimeEnvironment =
                [| "DOTNET_TieredCompilation"; "DOTNET_TC_QuickJit"; "DOTNET_TC_QuickJitForLoops"; "DOTNET_ReadyToRun"
                   "DOTNET_gcServer"; "DOTNET_GCHeapHardLimit"; "DOTNET_PROCESSOR_COUNT"; "COMPlus_TieredCompilation"
                   "COMPlus_ReadyToRun"; "COMPlus_gcServer"; "CUDA_VISIBLE_DEVICES" |]
                |> Array.map (fun key -> key + "=" + (Environment.GetEnvironmentVariable key |> Option.ofObj |> Option.defaultValue "<unset>"))
            let notes =
                [| "All worker processes execute serially. Each measured repeat is a fresh process; separate warmups do not remove measured JIT/startup cost."
                   "legacy-port retains the ineffective legacy random seeding, schedule-dependent samples, shared triangle barycentric races, original transport and forced cleanup GC."
                   "DOTNET_PROCESSOR_COUNT is set to requested threads for every worker. Legacy keeps its original Parallel.For scheduling; this is not a per-OS-thread hard cap."
                   "Renderer precision and PFM float32 storage precision differ. PFM retains unclamped linear HDR values, not gamma-encoded PNG values."
                   "Source digest hashes tracked and nonignored untracked working-tree source, including dirty file contents and missing tracked files; not merely the git SHA."
                   "Worker phases marked unavailable are not observations. Failure metrics are retained verbatim but never included as zero-valued timings."
                   if has "--skip-build" || engines |> Array.exists (fun engine -> has ("--" + engine + "-worker")) then
                       "Existing/override assemblies were explicitly requested: binary hashes are retained, but this invocation did not establish source-to-binary build correspondence." |]
            let metadata =
                { SchemaVersion = 1; CreatedUtc = Data.timestamp (); Repository = root; GitCommit = gitCommit; GitStatus = gitStatus
                  SourceDigest = Data.digestFiles sources; Sources = sources; Dotnet = dotnet; Sdk = sdk
                  OperatingSystem = RuntimeInformation.OSDescription; Architecture = RuntimeInformation.ProcessArchitecture.ToString()
                  Cpu = cpu; GpuProbe = gpu; RuntimeEnvironment = runtimeEnvironment; Dependencies = dependencies
                  DependencyDescriptions = dependencies |> Array.filter (fun file -> file.Path.EndsWith ".deps.json") |> Array.map (fun file -> File.ReadAllText file.Path)
                  Notes = notes }
            let settings preset =
                let initial =
                    if has "--settings" then Data.deserialize<RenderSettings> (path (get "--settings" ""))
                    else SceneFiles.preset preset
                { initial with Width = integer "--width" initial.Width; Height = integer "--height" initial.Height
                               CameraSamples = integer "--camera-samples" initial.CameraSamples
                               LightSamples = integer "--light-samples" initial.LightSamples
                               GlossySamples = integer "--glossy-samples" initial.GlossySamples
                               MaxBounces = integer "--max-bounces" initial.MaxBounces; Threads = integer "--threads" initial.Threads
                               TileSize = integer "--tile-size" initial.TileSize; Seed = integer "--seed" initial.Seed
                               Precision = get "--precision" initial.Precision; Sampler = get "--sampler" initial.Sampler
                               Transfer = get "--transfer" initial.Transfer } |> SceneFiles.validateSettings
            let scenes =
                scenePaths
                |> Array.map (fun scenePath ->
                    let scene = SceneFiles.load scenePath
                    ScenePolicy.validateVariants scene.Id materials
                    let assets =
                        Array.concat
                            [| [| scenePath |]
                               scene.Meshes |> Array.map (fun mesh -> SceneFiles.resolveAsset scenePath mesh.Path)
                               scene.Materials |> Array.filter (fun material -> not (String.IsNullOrWhiteSpace material.Texture))
                                               |> Array.map (fun material -> SceneFiles.resolveAsset scenePath material.Texture)
                               [| path "benchmarks/scenes/provenance.json"; path "benchmarks/legacy/source.sha256"
                                  path "benchmarks/legacy/baseline.json" |] |> Array.filter File.Exists
                               if scene.Id = "gold-dragon" then
                                   [| path "benchmarks/gold-dragon.json"
                                      path "artifacts/scene-assets/gold-dragon/staging-plane.provenance.json" |] |> Array.filter File.Exists |]
                        |> Array.distinct |> Array.sort |> Array.map Data.hashFile
                    if assets |> Array.exists (fun file -> file.Bytes <= 0L) then failwith $"Scene {scene.Id} has missing or empty input assets."
                    scenePath, scene, assets)
            if (scenes |> Array.map (fun (_, scene, _) -> scene.Id) |> Array.distinct).Length <> scenes.Length then
                failwith "Selected scene IDs must be unique."
            let contextDigest =
                Data.serialize
                    {| schemaVersion = 1; gitCommit = metadata.GitCommit; sourceDigest = metadata.SourceDigest; sdk = metadata.Sdk
                       os = metadata.OperatingSystem; architecture = metadata.Architecture; cpu = metadata.Cpu; gpu = metadata.GpuProbe
                       environment = metadata.RuntimeEnvironment; dependencies = metadata.Dependencies; notes = metadata.Notes
                       timeoutSeconds = timeout |}
                |> Data.hashText
            let cases =
                [| for preset in presets do
                       for scenePath, scene, inputs in scenes do
                           for material in materials do
                               for engine in engines do
                                   let initial =
                                       { Preset = preset; Scene = scene.Id; ScenePath = scenePath; Material = material; Engine = engine
                                         Worker = workers.[engine]; Acceleration = (if engine = "cpu" then acceleration elif engine = "legacy" then "kdtree" else "gpu")
                                         Settings = settings preset; InputFiles = inputs; ContextFingerprint = contextDigest
                                         Fingerprint = ""; Repeats = repeats preset; Warmups = warmups }
                                   let fingerprint = Data.hashText (contextDigest + Data.serialize initial)
                                   yield { initial with Fingerprint = fingerprint } |]
            let mutable manifest =
                { SchemaVersion = 1; ContextFingerprint = contextDigest; Metadata = metadata; Cases = cases; TimeoutSeconds = timeout
                  RequestedMeasuredTrials = cases |> Array.sumBy (fun c -> c.Repeats); Status = "running" }
            let archivedMetadata = Path.Combine(output, "contexts", contextDigest, "metadata.json")
            if not (File.Exists archivedMetadata) then Data.writeJson archivedMetadata metadata
            let archivedManifest = Path.Combine(output, "runs", Path.GetFileName preparation + ".json")
            Data.writeJson (Path.Combine(output, "run-manifest.json")) manifest
            Data.writeJson archivedManifest manifest
            let safeName (value: string) =
                value |> Seq.map (fun c -> if Char.IsAsciiLetterOrDigit c || c = '-' || c = '_' then c else '_') |> Seq.toArray |> String
            let trialDirectory (c: CaseSpec) (repeat: int) warmup =
                Path.Combine(output, safeName c.Preset, safeName c.Scene, safeName c.Material, c.Engine, c.Fingerprint,
                             (if warmup then "warmup-" else "repeat-") + repeat.ToString("D3", Data.invariant))
            let initialTrial (c: CaseSpec) repeat warmup =
                { SchemaVersion = 1; Case = c; Repeat = repeat; Warmup = warmup; Attempt = 0; StartedUtc = ""
                  Status = "not-run"; Error = "Requested trial has not run."; ExitCode = Nullable(); ColdMs = Nullable()
                  Metrics = Data.nullRecord<RenderMetrics>; OutputPath = ""; LinearPath = ""; MetricsPath = ""; WorkerMetadataPath = ""
                  StdoutPath = ""; StderrPath = ""; TrialPath = Path.Combine(trialDirectory c repeat warmup, "trial.json"); Files = [||] }
            let trials =
                cases |> Array.collect (fun c ->
                    Array.append (Array.init c.Warmups (fun i -> initialTrial c (i + 1) true))
                                 (Array.init c.Repeats (fun i -> initialTrial c (i + 1) false)))
            Report.writeTrials output trials
            let loadMetrics metricsPath =
                if File.Exists metricsPath then
                    try Data.deserialize<RenderMetrics> metricsPath with _ -> Data.nullRecord<RenderMetrics>
                else Data.nullRecord<RenderMetrics>
            let validSuccess (trial: Trial) =
                if obj.ReferenceEquals(trial.Metrics, null) then failwith "Missing or invalid structured worker metrics."
                let metrics = trial.Metrics
                if metrics.SchemaVersion <> 1 || not (Array.contains metrics.Status [| "success"; "ok" |]) then
                    failwith $"Worker metrics did not report success: {metrics.Status}: {metrics.Error}"
                if metrics.Scene <> trial.Case.Scene || metrics.Material <> trial.Case.Material
                   || Data.serialize metrics.Settings <> Data.serialize trial.Case.Settings then
                    failwith "Worker metrics do not match the requested scene/material/settings."
                let expectedEngine = if trial.Case.Engine = "legacy" then "legacy-port" else trial.Case.Engine
                if Data.engineFamily metrics.Engine <> expectedEngine then
                    failwith $"Requested {expectedEngine}, but worker reports {metrics.Engine}; fallback is forbidden."
                if metrics.InvalidPixels <> 0 then failwith $"Worker reported {metrics.InvalidPixels} invalid pixels."
                if Path.GetFullPath metrics.OutputPath <> trial.OutputPath || Path.GetFullPath metrics.LinearPath <> trial.LinearPath then
                    failwith "Worker output paths do not match the requested artifacts."
                let timings = metrics.Timings
                if obj.ReferenceEquals(timings, null) then failwith "Missing worker timings."
                for value in [ timings.LoadMs; timings.TraceMs; timings.EncodeMs; timings.CleanupMs; timings.TotalMs ] do
                    if not (Data.finite value) then failwith "Worker timings must be finite and nonnegative."
                for value in [ timings.BuildMs; timings.CompileMs; timings.UploadMs; timings.DownloadMs ] do
                    if value.HasValue && not (Data.finite value.Value) then failwith "Optional worker timing is invalid."
                if trial.Case.Engine = "gpu" then
                    if not (timings.CompileMs.HasValue && timings.UploadMs.HasValue && timings.DownloadMs.HasValue) then
                        failwith "GPU success must report compilation, upload and download costs, not kernel-only timing."
                    if String.IsNullOrWhiteSpace metrics.Device || metrics.Backend.Contains("cpu", StringComparison.OrdinalIgnoreCase) then
                        failwith "An explicit GPU request did not identify a real GPU backend/device."
                if LinearImage.pngDimensions trial.OutputPath <> (trial.Case.Settings.Width, trial.Case.Settings.Height) then
                    failwith "PNG dimensions do not match settings."
                let film = LinearImage.read trial.LinearPath
                if film.Width <> trial.Case.Settings.Width || film.Height <> trial.Case.Settings.Height
                   || film.Pixels |> Array.exists (fun value -> not (Single.IsFinite value) || value < 0.0f) then
                    failwith "Linear film has incorrect dimensions or invalid radiance values."
            let canResume (trial: Trial) =
                if not (has "--resume") || not (File.Exists trial.TrialPath) then None
                else
                    try
                        let previous = Data.deserialize<Trial> trial.TrialPath
                        if previous.SchemaVersion = 1 && previous.Case.Fingerprint = trial.Case.Fingerprint
                           && previous.Repeat = trial.Repeat && previous.Warmup = trial.Warmup
                           && previous.Status = "success" && Data.filesIntact previous.Files then
                            validSuccess previous
                            Some previous
                        else None
                    with _ -> None
            let resumable = trials |> Array.map canResume
            let needsWarmup =
                Array.zip trials resumable
                |> Array.choose (fun (trial, previous) -> if not trial.Warmup && Option.isNone previous then Some trial.Case.Fingerprint else None)
                |> Set.ofArray
            let runTrial (trial: Trial) previous =
                match previous with
                | Some previous ->
                    printfn "Resume %s/%s/%s/%s %s %d" trial.Case.Preset trial.Case.Scene trial.Case.Material trial.Case.Engine
                        (if trial.Warmup then "warmup" else "repeat") trial.Repeat
                    previous
                | None ->
                    let c = trial.Case
                    let directory = trialDirectory c trial.Repeat trial.Warmup
                    Directory.CreateDirectory directory |> ignore
                    let mutable attempt = 1
                    while Directory.Exists(Path.Combine(directory, "attempt-" + attempt.ToString("D3", Data.invariant))) do attempt <- attempt + 1
                    let attemptDirectory = Path.Combine(directory, "attempt-" + attempt.ToString("D3", Data.invariant))
                    Directory.CreateDirectory attemptDirectory |> ignore
                    let outputPath, linearPath, metricsPath =
                        Path.Combine(attemptDirectory, "render.png"), Path.Combine(attemptDirectory, "linear.pfm"), Path.Combine(attemptDirectory, "metrics.json")
                    let settingsPath = Path.Combine(attemptDirectory, "settings.json")
                    Data.writeJson settingsPath c.Settings
                    let pending =
                        { trial with Attempt = attempt; StartedUtc = Data.timestamp (); OutputPath = outputPath; LinearPath = linearPath
                                     MetricsPath = metricsPath; WorkerMetadataPath = metricsPath + ".worker.json"
                                     StdoutPath = Path.Combine(attemptDirectory, "stdout.log"); StderrPath = Path.Combine(attemptDirectory, "stderr.log") }
                    printfn "Run %s/%s/%s/%s %s %d/%d" c.Preset c.Scene c.Material c.Engine
                        (if trial.Warmup then "warmup" else "repeat") trial.Repeat (if trial.Warmup then c.Warmups else c.Repeats)
                    let result =
                        if buildErrors.[c.Engine] <> "" then
                            { pending with Status = "unavailable"; Error = buildErrors.[c.Engine] }
                        else
                            let arguments =
                                [ yield c.Worker
                                  yield! [ "--scene"; c.ScenePath; "--material"; c.Material; "--settings"; settingsPath
                                           "--output"; outputPath; "--linear"; linearPath; "--metrics"; metricsPath ]
                                  if c.Engine = "cpu" then yield! [ "--acceleration"; c.Acceleration ] ]
                            let outcome = Data.runProcess root dotnet arguments [ ("DOTNET_PROCESSOR_COUNT", string c.Settings.Threads) ] timeout cancellation.Token
                            Data.writeText pending.StdoutPath outcome.Stdout
                            Data.writeText pending.StderrPath outcome.Stderr
                            let metrics = loadMetrics metricsPath
                            let error =
                                if not (obj.ReferenceEquals(metrics, null)) && not (String.IsNullOrWhiteSpace metrics.Error) then metrics.Error
                                elif not (String.IsNullOrWhiteSpace outcome.Error) then outcome.Error
                                else outcome.Stderr
                            let result = { pending with ColdMs = Nullable outcome.ElapsedMs; ExitCode = outcome.ExitCode; Metrics = metrics }
                            if outcome.Status <> "exited" then { result with Status = outcome.Status; Error = error }
                            elif outcome.ExitCode <> Nullable 0 then
                                { result with Status = if not (obj.ReferenceEquals(metrics, null)) && metrics.Status = "unavailable" then "unavailable" else "failure"
                                              Error = if String.IsNullOrWhiteSpace error then $"Worker exited with code {outcome.ExitCode}." else error }
                            else
                                try
                                    validSuccess result
                                    { result with Status = "success"; Error = "" }
                                with ex -> { result with Status = "incomplete"; Error = ex.Message }
                    let files =
                        [| result.OutputPath; result.LinearPath; result.MetricsPath; result.WorkerMetadataPath; settingsPath |]
                        |> Array.filter File.Exists |> Array.map Data.hashFile
                    let result = { result with Files = files }
                    Data.persistTrial result
                    result
            let mutable executionError = ""
            try
                for index = 0 to trials.Length - 1 do
                    if not cancellation.IsCancellationRequested then
                        let previous =
                            if trials.[index].Warmup && needsWarmup.Contains trials.[index].Case.Fingerprint then None
                            else resumable.[index]
                        trials.[index] <- runTrial trials.[index] previous
                        Report.writeTrials output trials
                    else trials.[index] <- { trials.[index] with Status = "cancelled"; Error = "Not started because the benchmark was cancelled." }
            with ex ->
                executionError <- ex.ToString()
                eprintfn "%s" executionError
            let changed =
                Array.concat
                    [| sources |> Array.map (fun source -> { source with Path = path source.Path })
                       dependencies
                       cases |> Array.collect (fun c -> c.InputFiles) |]
                |> Array.distinctBy (fun file -> file.Path)
                |> Array.filter (fun original ->
                    let current = Data.hashFile original.Path
                    current.Sha256 <> original.Sha256 || current.Bytes <> original.Bytes)
            if changed.Length > 0 then
                executionError <- $"Inputs, dependencies or working-tree source changed during this invocation ({changed.Length} files). Results are retained but not a frozen-input measurement."
                Data.writeJson (Path.Combine(output, "changed-inputs.json")) changed
                for index = 0 to trials.Length - 1 do
                    if trials.[index].Status = "success" then
                        trials.[index] <- Data.invalidateTrial executionError trials.[index]
                eprintfn "%s" executionError
            let success = executionError = "" && not cancellation.IsCancellationRequested && trials |> Array.forall (fun trial -> trial.Status = "success")
            manifest <- { manifest with Status = if success then "complete" elif cancellation.IsCancellationRequested then "cancelled" else "incomplete" }
            Data.writeJson (Path.Combine(output, "run-manifest.json")) manifest
            Data.writeJson archivedManifest manifest
            if executionError <> "" then Data.writeText (Path.Combine(output, "orchestrator-error.log")) executionError
            let reportErrors = Report.write output manifest trials
            if reportErrors then
                manifest <- { manifest with Status = "incomplete-report" }
                Data.writeJson (Path.Combine(output, "run-manifest.json")) manifest
                Data.writeJson archivedManifest manifest
            let completed = trials |> Array.filter (fun trial -> not trial.Warmup && trial.Status = "success") |> Array.length
            printfn "%s: %d/%d measured trials; report %s" manifest.Status completed manifest.RequestedMeasuredTrials (Path.Combine(output, "index.html"))
            if cancellation.IsCancellationRequested then 130 elif success && not reportErrors then 0 else 1
        finally Console.CancelKeyPress.RemoveHandler cancelHandler

let exitCode =
    try
        let options = parse ()
        if options.ContainsKey "--help" then printfn "%s" help; 0
        else
            try
                let code = execute options
                code
            with ex ->
                let target = match options.TryGetValue "--output" with true, value -> value | _ -> "artifacts/benchmarks"
                let output = Path.GetFullPath(Path.Combine(root, target))
                let gitDirectory = Path.Combine(root, ".git")
                if output <> root && output <> gitDirectory
                   && not (output.StartsWith(gitDirectory + string Path.DirectorySeparatorChar, StringComparison.Ordinal)) then
                    Data.writeJson (Path.Combine(output, "last-invocation-error.json"))
                        {| schemaVersion = 1; status = "failure"; createdUtc = Data.timestamp (); error = ex.ToString(); arguments = args |}
                eprintfn "benchmark: %s" ex.Message
                2
    with ex -> eprintfn "benchmark: %s" ex.Message; 2
exit exitCode
