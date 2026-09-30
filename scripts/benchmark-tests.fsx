#load "../SceneFormat/SceneFormat.fs"
#load "../BenchmarkRunner/ScenePolicy.fs"
#load "../BenchmarkRunner/LinearOutput.fs"
#load "BenchmarkData.fs"
#load "BenchmarkReport.fs"

open System
open System.IO
open System.Threading
open Tracer.SceneFormat
open Tracer.Benchmarks
open Tracer.Benchmarks.Reporting

let arguments = fsi.CommandLineArgs |> Array.skip 1
if Array.contains "--child-sleep" arguments then Thread.Sleep 60000
elif Array.contains "--child-echo" arguments then
    printfn "contract-stdout"
    eprintfn "contract-stderr"
    exit 7
else
    let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
    let directory = Path.Combine(root, "artifacts", "benchmark-tests", Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory directory |> ignore
    let mutable assertions = 0
    let check name condition =
        if not condition then failwith $"FAIL: {name}"
        assertions <- assertions + 1
    let throws name action =
        let failed = try action (); false with _ -> true
        check name failed
    let near a b = abs (a - b) < 1e-10
    try
        let primary = ScenePolicy.selectScenes "all"
        check "all remains exactly the four primary scene IDs" (primary = [| "chair"; "roman-bust"; "space-sentinel"; "sky-arena" |])
        check "standard/high primary matrix remains 48 cases per engine" (primary.Length * SceneFiles.materialVariants.Length * 2 = 48)
        check "gold dragon is explicit only" (ScenePolicy.selectScenes "gold-dragon" = [| "gold-dragon" |] && not (Array.contains "gold-dragon" primary))
        check "bonus alone does not trigger procedural asset generation" (not (ScenePolicy.usesProceduralAssets [| "gold-dragon" |]))
        check "primary selection still prepares offline assets" (ScenePolicy.usesProceduralAssets primary)
        ScenePolicy.validateVariants "gold-dragon" [| "authored" |]
        for sceneId in primary do ScenePolicy.validateVariants sceneId SceneFiles.materialVariants
        for variant in SceneFiles.materialVariants |> Array.filter ((<>) "authored") do
            throws ("gold dragon rejects nonauthored override " + variant) (fun () -> ScenePolicy.validateVariants "gold-dragon" [| variant |])
        throws "gold dragon rejects whole material sweep" (fun () -> ScenePolicy.validateVariants "gold-dragon" SceneFiles.materialVariants)
        let material: MaterialSpec =
            { Id = "colour-contract"; Kind = MaterialKind.Glossy; Colour = [| 1.; 1.; 0.3 |]
              AmbientColour = null; SpecularColour = null; ReflectionColour = null
              Ambient = 0.1; Diffuse = 0.9; Specular = 0.4; Exponent = 10; Reflectivity = 0.3; GlossExponent = 10
              Ior = 1.5; Filter = [| 1.; 1.; 1. |]; Emission = 0.; Texture = "" }
        check "null ambient color uses diffuse color" (SceneFiles.ambientColour material = material.Colour)
        check "null specular color defaults to white, not diffuse" (SceneFiles.specularColour material = [| 1.; 1.; 1. |])
        check "null reflection color defaults to white, not diffuse" (SceneFiles.reflectionColour material = [| 1.; 1.; 1. |])
        let gold =
            { material with AmbientColour = [| 1.; 0.75; 0.5 |]
                            SpecularColour = [| 1.; 1.; 1. |]; ReflectionColour = [| 1.; 1.; 1. |] }
        check "gold keeps orange ambient distinct from lemon diffuse"
            (SceneFiles.ambientColour gold = [| 1.; 0.75; 0.5 |] && gold.Colour = [| 1.; 1.; 0.3 |])
        check "gold highlights and reflections remain white"
            (SceneFiles.specularColour gold = [| 1.; 1.; 1. |] && SceneFiles.reflectionColour gold = [| 1.; 1.; 1. |])
        let distribution = Data.distribution [| 1.; 2.; 3.; 4.; 100. |]
        check "median keeps slow outlier" (near distribution.Median 3. && distribution.Max = 100.)
        check "IQR and MAD" (near distribution.P25 2. && near distribution.P75 4. && near distribution.Mad 1.)
        check "empty observations are null, not zero" (obj.ReferenceEquals(Data.distribution [||], null))
        check "modern GPU metadata alias stays GPU" (Data.engineFamily "modern-gpu" = "gpu")
        check "CPU metadata cannot satisfy explicit GPU request" (Data.engineFamily "modern-cpu" <> "gpu" && Data.engineFamily "legacy-port" <> "gpu")
        check "CSV preserves commas/newlines/quotes" (Data.csv [ [| "a,b"; "line\nnext"; "\"quoted\"" |] ] = "\"a,b\",\"line\nnext\",\"\"\"quoted\"\"\"")
        let film: LinearImage.Film =
            { Width = 2; Height = 2
              Pixels = [| 0.f; 0.25f; 1.f; 2.f; 3.f; 4.f; 5.f; 6.f; 7.f; 8.f; 9.f; 10.f |] }
        let pfm = Path.Combine(directory, "roundtrip.pfm")
        LinearImage.write pfm film
        let decoded = LinearImage.read pfm
        check "PFM roundtrip retains top/down orientation and HDR radiance" (decoded = film)
        let sharedPfm = Path.Combine(directory, "shared.pfm")
        LinearOutput.writePfm sharedPfm film.Width film.Height (Array.map float film.Pixels)
        let sharedBytes = File.ReadAllBytes sharedPfm
        check "shared worker PFM writer preserves the established bytes and orientation"
            (sharedBytes = File.ReadAllBytes pfm && LinearImage.read sharedPfm = film)
        let signedHdr = [| -0.25; 0.; 4.; float Single.MaxValue; 1.; 0.5 |]
        let signedPfm = Path.Combine(directory, "signed-hdr.pfm")
        LinearOutput.writePfm signedPfm 2 1 signedHdr
        check "shared PFM writer applies no clamp or display transfer"
            ((LinearImage.read signedPfm).Pixels = Array.map float32 signedHdr)
        for name, width, height, values in
            [ "zero width", 0, 1, [||]
              "negative height", 1, -1, [||]
              "oversized dimensions", Int32.MaxValue, Int32.MaxValue, [||]
              "null data", 1, 1, null
              "wrong component count", 2, 1, [| 0.; 0.; 0. |]
              "NaN", 1, 1, [| Double.NaN; 0.; 0. |]
              "positive infinity", 1, 1, [| Double.PositiveInfinity; 0.; 0. |]
              "negative infinity", 1, 1, [| Double.NegativeInfinity; 0.; 0. |]
              "positive FP32 overflow", 1, 1, [| Double.MaxValue; 0.; 0. |]
              "negative FP32 overflow", 1, 1, [| -Double.MaxValue; 0.; 0. |] ] do
            throws ("shared PFM rejects " + name) (fun () -> LinearOutput.writePfm sharedPfm width height values)
            check ("invalid PFM " + name + " preserves existing output") (File.ReadAllBytes sharedPfm = sharedBytes)
        let missingOutput = Path.Combine(directory, "uncreated", "invalid.pfm")
        throws "shared PFM validates before creating a destination"
            (fun () -> LinearOutput.writePfm missingOutput 1 1 [| Double.MaxValue; 0.; 0. |])
        check "invalid shared PFM creates no partial output directory"
            (not (Directory.Exists(Path.GetDirectoryName missingOutput)))
        let mae, rmse, maximum, relative, preview = LinearImage.compare film decoded
        check "identical image errors are zero" (mae = 0. && rmse = 0. && maximum = 0. && relative.Value = 0. && Array.forall ((=) 0uy) preview)
        let black = { film with Pixels = Array.zeroCreate film.Pixels.Length }
        let _, _, _, relativeBlack, _ = LinearImage.compare black film
        check "nonzero error over black reference has unavailable relative RMSE" (not relativeBlack.HasValue)
        let nonfinite = { film with Pixels = Array.create film.Pixels.Length Single.NaN }
        throws "nonfinite film comparison fails" (fun () -> LinearImage.compare film nonfinite |> ignore)
        let truncated = Path.Combine(directory, "truncated.pfm")
        let bytes = File.ReadAllBytes pfm
        File.WriteAllBytes(truncated, bytes.[0 .. bytes.Length - 2])
        throws "truncated linear buffer fails" (fun () -> LinearImage.read truncated |> ignore)
        let png = Path.Combine(directory, "preview.png")
        LinearImage.writePng png film.Width film.Height (Array.init 12 (fun i -> byte (i * 20)))
        check "PNG encoder writes expected dimensions" (LinearImage.pngDimensions png = (2, 2))
        let damagedPng = Path.Combine(directory, "damaged.png")
        let encoded = File.ReadAllBytes png
        encoded.[encoded.Length - 1] <- encoded.[encoded.Length - 1] ^^^ 1uy
        File.WriteAllBytes(damagedPng, encoded)
        throws "PNG payload checksum corruption fails" (fun () -> LinearImage.pngDimensions damagedPng |> ignore)
        let files = [| png; pfm; truncated |] |> Array.map Data.hashFile
        check "complete artifact hashes verify" (Data.filesIntact files)
        File.AppendAllText(truncated, "changed")
        check "resume rejects changed artifact content" (not (Data.filesIntact files))
        let firstDigest = Data.digestFiles files
        let reordered = Data.digestFiles (Array.rev files)
        check "fingerprints are independent of enumeration order" (firstDigest = reordered)
        check "dirty file contents change source fingerprint" (firstDigest <> Data.digestFiles (files |> Array.map (fun file -> Data.hashFile file.Path)))
        let settings = { SceneFiles.preset "quick" with Width = 2; Height = 2; CameraSamples = 1; Threads = 1 }
        let case =
            { Preset = "quick"; Scene = "<fixture>"; ScenePath = ""; Material = "matte"; Engine = "cpu"; Worker = ""
              Acceleration = "bvh"; Settings = settings; InputFiles = [||]; ContextFingerprint = "test"; Fingerprint = "test"; Repeats = 2; Warmups = 1 }
        let timing =
            { LoadMs = 1.; BuildMs = Nullable 2.; CompileMs = Nullable(); UploadMs = Nullable(); TraceMs = 4.
              DownloadMs = Nullable(); EncodeMs = 1.; CleanupMs = 0.; TotalMs = 8. }
        let metrics =
            { SchemaVersion = 1; Engine = "cpu"; Backend = "cpu/bvh"; Device = "test fixture"; Scene = case.Scene; Material = "matte"
              Status = "success"; Error = ""; Settings = settings; Timings = timing; AllocatedBytes = Nullable 10L
              PeakWorkingSetBytes = 20L; PeakDeviceBytes = Nullable(); GcCollections = [| 0; 0; 0 |]; InvalidPixels = 0
              OutputPath = png; LinearPath = pfm }
        let trial =
            { SchemaVersion = 1; Case = case; Repeat = 1; Warmup = false; Attempt = 1; StartedUtc = Data.timestamp (); Status = "success"
              Error = ""; ExitCode = Nullable 0; ColdMs = Nullable 10.; Metrics = metrics; OutputPath = png; LinearPath = pfm
              MetricsPath = ""; WorkerMetadataPath = ""; StdoutPath = ""; StderrPath = ""; TrialPath = ""; Files = [||] }
        let trials =
            [| { trial with Warmup = true; ColdMs = Nullable 1000. }
               trial
               { trial with Repeat = 2; Status = "timeout"; ColdMs = Nullable 50.; Metrics = Data.nullRecord<RenderMetrics>; Error = "controlled timeout" } |]
        let summaries = Data.summarize trials
        check "failed trials retained and incomplete" (summaries.Length = 1 && summaries.[0].Required = 2 && summaries.[0].Completed = 1 && summaries.[0].Failed = 1)
        let cold = summaries.[0].Measurements |> Array.find (fun measurement -> measurement.Name = "cold")
        check "warmup and failure durations excluded, not converted to zeros" (cold.Distribution.Count = 1 && cold.Distribution.Median = 10.)
        let compile = summaries.[0].Measurements |> Array.find (fun measurement -> measurement.Name = "compile")
        check "unavailable GPU phase remains null" (obj.ReferenceEquals(compile.Distribution, null))
        let metadata =
            { SchemaVersion = 1; CreatedUtc = Data.timestamp (); Repository = root; GitCommit = "test"; GitStatus = ""; SourceDigest = "test"; Sources = [||]
              Dotnet = Environment.ProcessPath; Sdk = ""; OperatingSystem = ""; Architecture = ""; Cpu = ""; GpuProbe = ""
              RuntimeEnvironment = [||]; Dependencies = [||]; DependencyDescriptions = [||]; Notes = [| "Contract test, not a benchmark render." |] }
        let manifest =
            { SchemaVersion = 1; ContextFingerprint = "test"; Metadata = metadata; Cases = [| case |]; TimeoutSeconds = 1.; RequestedMeasuredTrials = 2; Status = "incomplete" }
        Data.writeJson (Path.Combine(directory, "run-manifest.json")) manifest
        Report.write directory manifest trials |> ignore
        let report = File.ReadAllText(Path.Combine(directory, "index.html"))
        check "report explicitly marks incomplete" (report.Contains "INCOMPLETE requested run")
        check "HTML escapes scene IDs" (report.Contains "&lt;fixture&gt;" && not (report.Contains "<fixture>"))
        check "HTML does not use absolute image paths" (not (report.Contains("src=\"" + root)))
        check "report distinguishes current and earlier legacy timing boundaries"
            (report.Contains "single PreProcessing call in build"
             && report.Contains "Earlier legacy captures also included scene-tree construction in trace")
        check "failure CSV contains diagnostic" ((File.ReadAllText(Path.Combine(directory, "trials.csv"))).Contains "controlled timeout")
        let restored = Data.deserialize<Trial array> (Path.Combine(directory, "trials.json"))
        check "trial JSON roundtrip preserves nullable metrics" (restored.Length = 3 && obj.ReferenceEquals(restored.[2].Metrics, null))
        let savedTrial =
            { trial with OutputPath = Path.Combine(directory, "attempt-001", "render.png")
                         TrialPath = Path.Combine(directory, "retained", "trial.json") }
        Data.persistTrial savedTrial
        let contaminated = Data.invalidateTrial "Input fingerprint changed during the invocation." savedTrial
        let pointer = Data.deserialize<Trial> savedTrial.TrialPath
        let attempt = Data.deserialize<Trial> (Path.Combine(directory, "attempt-001", "trial.json"))
        check "input contamination invalidates both persisted trial records"
            (contaminated.Status = "incomplete" && pointer = contaminated && attempt = contaminated)
        check "contaminated timings cannot become successful observations"
            ((Data.summarize [| contaminated |]).[0].Completed = 0)
        let host = Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" |> Option.ofObj |> Option.defaultValue Environment.ProcessPath
        let self = Path.Combine(__SOURCE_DIRECTORY__, "benchmark-tests.fsx")
        let command mode = [ "fsi"; "--exec"; self; "--"; mode ]
        let exited = Data.runProcess root host (command "--child-echo") [] 30. CancellationToken.None
        check "nonzero subprocess exit and streams retained"
            (exited.Status = "exited" && exited.ExitCode = Nullable 7 && exited.Stdout.Contains "contract-stdout" && exited.Stderr.Contains "contract-stderr")
        let timeout = Data.runProcess root host (command "--child-sleep") [] 0.2 CancellationToken.None
        check "exact process deadline kills worker" (timeout.Status = "timeout" && timeout.ElapsedMs >= 150. && timeout.ElapsedMs < 10000.)
        use cancelled = new CancellationTokenSource()
        cancelled.CancelAfter 200
        let stopped = Data.runProcess root host (command "--child-sleep") [] 30. cancelled.Token
        check "cancellation terminates owned worker" (stopped.Status = "cancelled" && stopped.ElapsedMs < 10000.)
        printfn "All %d benchmark contract assertions passed." assertions
    finally
        Directory.Delete(directory, true)
