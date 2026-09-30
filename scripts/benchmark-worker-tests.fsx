#load "../SceneFormat/SceneFormat.fs"
#load "BenchmarkData.fs"
#r "../BenchmarkRunner/bin/Release/net10.0/RayTracer.ImageIO.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/StbImageSharp.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/StbImageWriteSharp.dll"

open System
open System.IO
open System.Text.Json
open System.Threading
open Tracer.SceneFormat
open Tracer.Imaging
open Tracer.Benchmarks.Reporting

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let output = Path.Combine(root, "artifacts", "benchmark-worker-checks", DateTime.UtcNow.ToString("yyyyMMddTHHmmssfffffff", Data.invariant))
Directory.CreateDirectory output |> ignore
let host = Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" |> Option.ofObj |> Option.defaultValue Environment.ProcessPath
let mutable assertions = 0
let check name condition =
    if not condition then failwith $"FAIL: {name}"
    assertions <- assertions + 1
let workers =
    [| "legacy", Path.Combine(root, "benchmarks/legacy/Runner/bin/Release/net10.0/LegacyRunner.dll")
       "cpu", Path.Combine(root, "BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll") |]
for _, worker in workers do
    if not (File.Exists worker) then failwith $"Build both Release workers first. Missing {worker}"

let tetra = """ply
format ascii 1.0
element vertex 4
property float x
property float y
property float z
property float u
property float v
element face 4
property list uchar int vertex_indices
end_header
-1 -1 0 0 0
1 -1 0 1 0
0 1 0 0.5 1
0 0 1.6 0.5 0.5
3 0 2 1
3 0 1 3
3 1 2 3
3 2 0 3
"""
let floor = """ply
format ascii 1.0
element vertex 4
property float x
property float y
property float z
element face 2
property list uchar int vertex_indices
end_header
-3 -1.5 -3
3 -1.5 -3
3 -1.5 3
-3 -1.5 3
3 0 2 1
3 0 3 2
"""
File.WriteAllText(Path.Combine(output, "tetra.ply"), tetra)
File.WriteAllText(Path.Combine(output, "floor.ply"), floor)
LinearImage.writePng (Path.Combine(output, "texture.png")) 2 2 [| 200uy; 150uy; 100uy; 120uy; 230uy; 160uy; 255uy; 128uy; 200uy; 150uy; 180uy; 240uy |]
let subject =
    { Id = "subject"; Kind = MaterialKind.Phong; Colour = [| 0.6; 0.25; 0.12 |]
      AmbientColour = [| 0.7; 0.2; 0.1 |]; SpecularColour = [| 0.8; 0.9; 1. |]; ReflectionColour = [| 0.9; 0.8; 0.7 |]
      Ambient = 0.2; Diffuse = 0.65; Specular = 0.3; Exponent = 12; Reflectivity = 0.; GlossExponent = 16
      Ior = 1.5; Filter = [| 0.9; 0.95; 0.98 |]; Emission = 0.; Texture = "texture.png" }
let scene =
    { SchemaVersion = 1; Id = "worker-fixture"; Title = "Tiny original tetrahedron diagnostic"
      Description = "Benchmark contract test fixture, not one of the four required presentation scenes."
      Camera = { Position = [| 3.; 2.; 5. |]; Target = [| 0.; -0.2; 0.5 |]; Up = [| 0.; 1.; 0. |]
                 ViewDistance = 4.; ViewWidth = 3.5; ViewHeight = 3.5; LensRadius = 0.; FocusDistance = 5. }
      AmbientColour = [| 1.; 1.; 1. |]; AmbientIntensity = 0.15; MaxBounces = 2
      Materials =
        [| subject
           { subject with Id = "floor"; Kind = MaterialKind.Matte; Colour = [| 0.25; 0.3; 0.35 |]
                          AmbientColour = null; SpecularColour = null; ReflectionColour = null; Specular = 0.; Texture = "" } |]
      Meshes = [| { Id = "tetra"; Path = "tetra.ply"; Smooth = false; Closed = true }
                  { Id = "floor"; Path = "floor.ply"; Smooth = false; Closed = false } |]
      Objects =
        [| { Id = "subject"; Mesh = "tetra"; Material = "subject"
             Transform = [| 1.; 0.12; 0.2; 0.; 0.; 1.1; 0.; -0.1; 0.1; 0.; 1.; 0.; 0.; 0.; 0.; 1. |] }
           { Id = "floor"; Mesh = "floor"; Material = "floor"; Transform = SceneFiles.identity } |]
      Lights =
        [| { Id = "key"; Kind = LightKind.Point; Position = [| -2.; 4.; 4. |]; Direction = [| 0.; 0.; 0. |]
             Colour = [| 1.; 0.95; 0.9 |]; Intensity = 1.5; Size = [| 0.; 0. |] }
           { Id = "fill"; Kind = LightKind.Rectangle; Position = [| 1.; 3.; 1. |]; Direction = [| 0.; -1.; 0. |]
             Colour = [| 0.8; 0.9; 1. |]; Intensity = 0.3; Size = [| 1.; 0.7 |] } |]
      Subjects = [| "subject" |] }
let scenePath = Path.Combine(output, "scene.json")
SceneFiles.save scenePath scene
let settings = { SceneFiles.preset "quick" with Width = 24; Height = 24; CameraSamples = 1; LightSamples = 1; GlossySamples = 1; Threads = 1; MaxBounces = 1 }

let render engine worker tag scenePath variant settings extra =
    let folder = Path.Combine(output, engine, tag)
    Directory.CreateDirectory folder |> ignore
    let settingsPath = Path.Combine(folder, "settings.json")
    let metricsPath = Path.Combine(folder, "metrics.json")
    Data.writeJson settingsPath settings
    let command =
        [ worker; "--scene"; scenePath; "--material"; variant; "--settings"; settingsPath
          "--output"; Path.Combine(folder, "image.png"); "--linear"; Path.Combine(folder, "linear.pfm"); "--metrics"; metricsPath ] @ extra
    let result = Data.runProcess root host command [ ("DOTNET_PROCESSOR_COUNT", string settings.Threads) ] 60. CancellationToken.None
    Data.writeText (Path.Combine(folder, "stdout.log")) result.Stdout
    Data.writeText (Path.Combine(folder, "stderr.log")) result.Stderr
    check (engine + "/" + tag + ": structured metrics exist") (File.Exists metricsPath)
    let metrics = Data.deserialize<RenderMetrics> metricsPath
    result, metrics

let successful engine tag (result: ProcessResult, metrics: RenderMetrics) =
    if result.ExitCode <> Nullable 0 then eprintfn "%s" result.Stderr
    check (engine + "/" + tag + ": worker succeeds") (result.Status = "exited" && result.ExitCode = Nullable 0 && metrics.Status = "success")
    check (engine + "/" + tag + ": correct engine and noninvalid film")
        (metrics.Engine = (if engine = "legacy" then "legacy-port" else "cpu") && metrics.InvalidPixels = 0)
    let film = LinearImage.read metrics.LinearPath
    check (engine + "/" + tag + ": PNG/raw dimensions") (LinearImage.pngDimensions metrics.OutputPath = (film.Width, film.Height))
    check (engine + "/" + tag + ": actual lit output") (film.Pixels |> Array.exists ((<) 0.f))
    if engine = "legacy" then
        let timings = metrics.Timings
        check (tag + ": legacy capture reports finite nonnegative phase durations")
            (timings.BuildMs.HasValue
             && [| timings.LoadMs; timings.BuildMs.Value; timings.TraceMs; timings.EncodeMs; timings.CleanupMs; timings.TotalMs |]
                |> Array.forall (fun value -> Double.IsFinite value && value >= 0.))
        use metadata = JsonDocument.Parse(File.ReadAllText(Path.Combine(Path.GetDirectoryName metrics.OutputPath, "metrics.json.worker.json")))
        let notes =
            metadata.RootElement.GetProperty("phaseNotes").EnumerateArray()
            |> Seq.map (fun note -> note.GetString()) |> Seq.toArray
        check (tag + ": original scene build is timed once by the capture hook")
            (notes |> Array.exists (fun note -> note.Contains "BuildMs measures the original single PreProcessing call"))
        check (tag + ": legacy trace/output preparation limitations are retained")
            ((notes |> Array.exists (fun note -> note.Contains "per-pixel Colour.ToColor/RGB8 writes"))
             && (notes |> Array.exists (fun note -> note.Contains "original final vertical flip")))
        let observed = timings.LoadMs + timings.BuildMs.Value + timings.TraceMs + timings.EncodeMs + timings.CleanupMs
        check (tag + ": capture phases do not double-count construction or output work") (observed <= timings.TotalMs + 1.)
    film

let images = System.Collections.Generic.Dictionary<string, LinearImage.Film>()
for engine, worker in workers do
    for variant in SceneFiles.materialVariants do
        let film = render engine worker variant scenePath variant settings [] |> successful engine variant
        images.Add(engine + "/" + variant, film)
    let unsupported = render engine worker "unsupported-precision" scenePath "matte" { settings with Precision = "float32" } []
    check (engine + ": unsupported precision fails explicitly") (fst unsupported).ExitCode.HasValue
    check (engine + ": no precision fallback") ((fst unsupported).ExitCode <> Nullable 0 && (snd unsupported).Status = "unavailable")
    let nonsquare = render engine worker "nonsquare-samples" scenePath "matte" { settings with CameraSamples = 2 } []
    check (engine + ": requested sample counts are not silently rounded")
        ((fst nonsquare).ExitCode <> Nullable 0 && (snd nonsquare).Error.Contains "square")
    let openScenePath = Path.Combine(output, "open-glass.json")
    SceneFiles.save openScenePath { scene with Subjects = [| "floor" |] }
    let openGlass = render engine worker "open-glass" openScenePath "glass" settings []
    check (engine + ": open mesh glass fails explicitly") ((fst openGlass).ExitCode <> Nullable 0 && (snd openGlass).Error.Contains "closed")
    if engine = "legacy" then
        let transfer = render engine worker "unsupported-transfer" scenePath "matte" { settings with Transfer = "srgb" } []
        check "legacy transfer is never silently replaced" ((fst transfer).ExitCode <> Nullable 0 && (snd transfer).Status = "unavailable")
    else
        let threaded = render engine worker "two-threads" scenePath "matte" { settings with Threads = 2; TileSize = 3 } [] |> successful engine "two-threads"
        check "modern worker is deterministic across thread/tile counts" (threaded.Pixels = images.["cpu/matte"].Pixels)
        for strategy in [ "kdtree"; "grid"; "brute" ] do
            let accelerated = render engine worker strategy scenePath "matte" settings [ "--acceleration"; strategy ] |> successful engine strategy
            check ("scene acceleration matches flat BVH: " + strategy) (accelerated.Pixels = images.["cpu/matte"].Pixels)
        render engine worker "srgb" scenePath "matte" { settings with Transfer = "srgb" } [] |> successful engine "srgb" |> ignore
        let endpointScenePath = Path.Combine(output, "display-endpoints.json")
        for emission in [ 1.; 2. ] do
            let white =
                { subject with
                    Kind = MaterialKind.Emissive
                    Colour = [| 1.; 1.; 1. |]
                    AmbientColour = null
                    SpecularColour = null
                    ReflectionColour = null
                    Ambient = 0.
                    Diffuse = 0.
                    Specular = 0.
                    Reflectivity = 0.
                    Emission = emission
                    Texture = "" }
            SceneFiles.save endpointScenePath
                { scene with
                    Id = "display-endpoints"
                    Materials = [| white |]
                    Objects = scene.Objects |> Array.filter (fun instance -> instance.Id = "subject")
                    Lights = [||]
                    AmbientIntensity = 0. }
            for transfer in [ "srgb"; "gamma2"; "linear" ] do
                let tag = $"display-{transfer}-{int emission}"
                let result, metrics = render engine worker tag endpointScenePath "authored" { settings with Transfer = transfer } []
                let film = successful engine tag (result, metrics)
                use png = RgbImage.Load metrics.OutputPath
                check (tag + ": fixture contains exact black and white/HDR endpoints")
                    (Array.contains 0.f film.Pixels && Array.contains (float32 emission) film.Pixels)
                let expected = film.Pixels |> Array.map (fun value -> if value = 0.f then 0uy else 255uy)
                check (tag + ": PNG black and white/HDR endpoints stay exactly 0/255") (png.Pixels = expected)

    let pathFolder = Path.Combine(output, engine, "protected-inputs")
    Directory.CreateDirectory pathFolder |> ignore
    let protectedSettings = Path.Combine(pathFolder, "settings.json")
    Data.writeJson protectedSettings settings
    let metadataScene = Path.Combine(output, engine + "-scene.worker.json")
    SceneFiles.save metadataScene scene
    let protectedInputs = [| scenePath; protectedSettings; metadataScene; Path.Combine(output, "tetra.ply"); Path.Combine(output, "texture.png") |]
    let originalHashes = protectedInputs |> Array.map SceneFiles.hashFile
    let aliases =
        [| "metrics-scene", scenePath, scenePath, []
           "metrics-settings", scenePath, protectedSettings, []
           "metrics-mesh", scenePath, Path.Combine(output, "tetra.ply"), []
           "metrics-texture", scenePath, Path.Combine(output, "texture.png"), []
           "metadata-scene", metadataScene, metadataScene.Substring(0, metadataScene.Length - ".worker.json".Length), []
           "malformed-input-alias", scenePath, scenePath, [ "--unknown"; "value" ]
           "unreadable-scene-alias", Path.Combine(output, "missing-scene.json"), Path.Combine(output, "texture.png"), [] |]
    let aliases =
        if OperatingSystem.IsWindows() then aliases
        else
            let linkedMetrics = Path.Combine(pathFolder, "input-link.json")
            File.CreateSymbolicLink(linkedMetrics, scenePath) |> ignore
            Array.append aliases [| "symlink-input-alias", scenePath, linkedMetrics, [] |]
    for tag, input, metrics, extra in aliases do
        let command =
            [ worker; "--scene"; input; "--material"; "matte"; "--settings"; protectedSettings
              "--output"; Path.Combine(pathFolder, tag + ".png"); "--linear"; Path.Combine(pathFolder, tag + ".pfm")
              "--metrics"; metrics ] @ extra
        let result = Data.runProcess root host command [ ("DOTNET_PROCESSOR_COUNT", "1") ] 60. CancellationToken.None
        check (engine + "/" + tag + ": rejected with stderr") (result.ExitCode.HasValue && result.ExitCode <> Nullable 0 && not (String.IsNullOrWhiteSpace result.Stderr))
        check (engine + "/" + tag + ": failure reporting preserves every input")
            (originalHashes = (protectedInputs |> Array.map SceneFiles.hashFile))

if not (Array.contains "--skip-orchestrator" (fsi.CommandLineArgs |> Array.skip 1)) then
    let report = Path.Combine(output, "orchestrator")
    let command =
        [ "fsi"; Path.Combine(root, "scripts/benchmark.fsx"); "--"; "--scene"; scenePath; "--presets"; "quick"; "--materials"; "matte"
          "--engines"; "legacy,cpu"; "--width"; "24"; "--height"; "24"; "--camera-samples"; "1"; "--light-samples"; "1"
          "--glossy-samples"; "1"; "--threads"; "1"; "--max-bounces"; "1"; "--warmups"; "1"; "--skip-build"; "--skip-prepare"
          "--timeout-seconds"; "60"; "--output"; report ]
    let first = Data.runProcess root host command [] 120. CancellationToken.None
    Data.writeText (Path.Combine(output, "orchestrator.stdout.log")) first.Stdout
    Data.writeText (Path.Combine(output, "orchestrator.stderr.log")) first.Stderr
    if first.ExitCode <> Nullable 0 then eprintfn "%s\n%s" first.Stdout first.Stderr
    check "orchestrator tiny legacy/CPU run succeeds" (first.ExitCode = Nullable 0)
    let firstTrials = Data.deserialize<Trial array> (Path.Combine(report, "trials.json"))
    check "separate warmups and measured trials retained" (firstTrials.Length = 4 && firstTrials |> Array.filter (fun trial -> trial.Warmup) |> Array.length = 2)
    let mutable resumedTrials = firstTrials
    let mutable stable, retry = false, 0
    while not stable && retry < 5 do
        retry <- retry + 1
        let previous = resumedTrials
        let resumed = Data.runProcess root host (command @ [ "--resume" ]) [] 120. CancellationToken.None
        Data.writeText (Path.Combine(output, $"orchestrator-resume-{retry}.stdout.log")) resumed.Stdout
        Data.writeText (Path.Combine(output, $"orchestrator-resume-{retry}.stderr.log")) resumed.Stderr
        if resumed.ExitCode <> Nullable 0 then eprintfn "%s\n%s" resumed.Stdout resumed.Stderr
        check "resumed invocation succeeds" (resumed.ExitCode = Nullable 0)
        resumedTrials <- Data.deserialize<Trial array> (Path.Combine(report, "trials.json"))
        stable <- Array.forall2 (fun a b -> a.Case.Fingerprint = b.Case.Fingerprint) previous resumedTrials
        if stable then
            check "fingerprinted resume succeeds without rerender" (resumed.Stdout.Contains "Resume" && not (resumed.Stdout.Contains "\nRun "))
            check "resume preserves trial identity and files" (Array.forall2 (fun a b -> a.StartedUtc = b.StartedUtc && a.Files = b.Files) previous resumedTrials)
        else printfn "Working-tree fingerprint changed between checks; correctly rerendered. Waiting for an identical-fingerprint check."
    check "identical-fingerprint resume verified within five attempts" stable
    let damaged = resumedTrials |> Array.find (fun trial -> not trial.Warmup && trial.Case.Engine = "cpu")
    File.AppendAllText(damaged.LinearPath, "corrupt")
    let repaired = Data.runProcess root host (command @ [ "--resume" ]) [] 120. CancellationToken.None
    if repaired.ExitCode <> Nullable 0 then eprintfn "%s\n%s" repaired.Stdout repaired.Stderr
    check "resume rerenders damaged artifacts instead of accepting them" (repaired.ExitCode = Nullable 0)
    let repairedTrials = Data.deserialize<Trial array> (Path.Combine(report, "trials.json"))
    let replacement = repairedTrials |> Array.find (fun trial -> not trial.Warmup && trial.Case.Engine = "cpu")
    let expectedAttempt = if replacement.Case.Fingerprint = damaged.Case.Fingerprint then damaged.Attempt + 1 else 1
    check "damaged attempt retained and new attempt created"
        (replacement.Attempt = expectedAttempt && replacement.LinearPath <> damaged.LinearPath && File.Exists damaged.LinearPath && Data.filesIntact replacement.Files)
    check "earlier source/environment provenance remains archived"
        (File.Exists(Path.Combine(report, "contexts", damaged.Case.ContextFingerprint, "metadata.json")))
    check "self-contained gallery generated" (File.Exists(Path.Combine(report, "index.html")) && File.Exists(Path.Combine(report, "differences.json")))
    Data.invalidateTrial "Input changed during the original invocation, then was restored." replacement |> ignore
    let invalidatedAttempt = Data.deserialize<Trial> (Path.Combine(Path.GetDirectoryName replacement.OutputPath, "trial.json"))
    check "contamination is retained in the original attempt history" (invalidatedAttempt.Status = "incomplete")
    let rerun = Data.runProcess root host (command @ [ "--resume" ]) [] 120. CancellationToken.None
    if rerun.ExitCode <> Nullable 0 then eprintfn "%s\n%s" rerun.Stdout rerun.Stderr
    check "resume can recover from invalidated inputs" (rerun.ExitCode = Nullable 0)
    let rerunTrials = Data.deserialize<Trial array> (Path.Combine(report, "trials.json"))
    let uncontaminated = rerunTrials |> Array.find (fun trial -> not trial.Warmup && trial.Case.Engine = "cpu")
    check "restoring inputs never reuses a contaminated successful measurement"
        (uncontaminated.Status = "success" && uncontaminated.LinearPath <> replacement.LinearPath && uncontaminated.StartedUtc <> replacement.StartedUtc)
    if not (OperatingSystem.IsWindows()) then
        let changingReport = Path.Combine(output, "changed-input-invocation")
        let launcher = Path.Combine(output, "mutating-host")
        let marker = Path.Combine(output, "mutate-enabled")
        let shellLiteral (value: string) = "'" + value.Replace("'", "'\"'\"'") + "'"
        let cpuWorker = workers |> Array.find (fun (engine, _) -> engine = "cpu") |> snd
        let wrapper =
            String.concat "\n"
                [ "#!/bin/sh"
                  shellLiteral host + " \"$@\""
                  "status=$?"
                  "if [ \"$status\" -eq 0 ] && [ \"$1\" = " + shellLiteral cpuWorker + " ] && [ -f " + shellLiteral marker + " ]; then"
                  "  printf '\\n' >> " + shellLiteral scenePath
                  "fi"
                  "exit \"$status\""; "" ]
        Data.writeText launcher wrapper
        File.SetUnixFileMode(launcher, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)
        Data.writeText marker "enabled"
        let original = File.ReadAllBytes scenePath
        let changingCommand =
            [ "fsi"; Path.Combine(root, "scripts/benchmark.fsx"); "--"; "--scene"; scenePath; "--presets"; "quick"
              "--materials"; "matte"; "--engines"; "cpu"; "--width"; "4"; "--height"; "4"; "--camera-samples"; "1"
              "--light-samples"; "1"; "--glossy-samples"; "1"; "--threads"; "1"; "--max-bounces"; "1"
              "--warmups"; "0"; "--skip-build"; "--skip-prepare"; "--dotnet"; launcher; "--output"; changingReport ]
        let mutable changedTrials = [||]
        try
            let changed = Data.runProcess root host changingCommand [] 120. CancellationToken.None
            Data.writeText (Path.Combine(output, "changed-input.stdout.log")) changed.Stdout
            Data.writeText (Path.Combine(output, "changed-input.stderr.log")) changed.Stderr
            check "actual in-flight input change makes invocation incomplete" (changed.ExitCode = Nullable 1)
            changedTrials <- Data.deserialize<Trial array> (Path.Combine(changingReport, "trials.json"))
            check "changed-input detection persistently invalidates a successful worker"
                (changedTrials.Length = 1 && changedTrials.[0].Status = "incomplete"
                 && changedTrials.[0].Metrics.Status = "success" && changedTrials.[0].Error.Contains "changed during")
            let saved = Data.deserialize<Trial> changedTrials.[0].TrialPath
            let history = Data.deserialize<Trial> (Path.Combine(Path.GetDirectoryName saved.OutputPath, "trial.json"))
            check "detected contamination invalidates both resume pointer and attempt history" (saved.Status = "incomplete" && history = saved)
        finally
            File.WriteAllBytes(scenePath, original)
            File.Delete marker
        let restored = Data.runProcess root host (changingCommand @ [ "--resume" ]) [] 120. CancellationToken.None
        if restored.ExitCode <> Nullable 0 then eprintfn "%s\n%s" restored.Stdout restored.Stderr
        check "restored original input renders successfully" (restored.ExitCode = Nullable 0)
        let restoredTrials = Data.deserialize<Trial array> (Path.Combine(changingReport, "trials.json"))
        check "identical restored fingerprint rerenders instead of accepting contaminated output"
            (restoredTrials.Length = 1 && restoredTrials.[0].Case.Fingerprint = changedTrials.[0].Case.Fingerprint
             && restoredTrials.[0].Attempt = changedTrials.[0].Attempt + 1 && restoredTrials.[0].Status = "success")
    let failedRun name extra =
        let folder = Path.Combine(output, name)
        let arguments =
            [ "fsi"; Path.Combine(root, "scripts/benchmark.fsx"); "--"; "--scene"; scenePath; "--presets"; "quick"
              "--materials"; "matte"; "--width"; "4"; "--height"; "4"; "--threads"; "1"; "--warmups"; "0"
              "--skip-build"; "--skip-prepare"; "--output"; folder ] @ extra
        let result = Data.runProcess root host arguments [] 120. CancellationToken.None
        Data.writeText (Path.Combine(output, name + ".stdout.log")) result.Stdout
        Data.writeText (Path.Combine(output, name + ".stderr.log")) result.Stderr
        check (name + ": incomplete benchmark exits nonzero") (result.ExitCode = Nullable 1)
        let trials = Data.deserialize<Trial array> (Path.Combine(folder, "trials.json"))
        check (name + ": failed case retained") (trials.Length = 1 && trials.[0].Status <> "success")
        let summaries = Data.deserialize<CaseSummary array> (Path.Combine(folder, "summary.json"))
        check (name + ": failure is not a zero timing")
            (summaries.[0].Completed = 0 && summaries.[0].Measurements |> Array.forall (fun value -> obj.ReferenceEquals(value.Distribution, null)))
        trials.[0]
    let unavailable =
        failedRun "unavailable-gpu"
            [ "--engines"; "gpu"; "--gpu-worker"; Path.Combine(output, "intentionally-missing-gpu.dll"); "--camera-samples"; "1" ]
    check "explicit GPU unavailability has no substitute PNG" (unavailable.Status = "unavailable" && not (File.Exists unavailable.OutputPath))
    let failure = failedRun "failure-metrics" [ "--engines"; "cpu"; "--camera-samples"; "2" ]
    check "worker structured error is surfaced in trial" (failure.Status = "failure" && failure.Error.Contains "square")
    let timedOut =
        failedRun "process-timeout" [ "--engines"; "cpu"; "--camera-samples"; "1"; "--timeout-seconds"; "0.001" ]
    check "exact worker timeout is retained, not dropped" (timedOut.Status = "timeout" && timedOut.ColdMs.HasValue)

printfn "All %d worker integration assertions passed. Diagnostic images retained: %s" assertions output
