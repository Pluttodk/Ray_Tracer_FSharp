#r "../bin/Release/net10.0/SceneFormat.dll"

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open Tracer.SceneFormat

let worker, reportPath =
    match fsi.CommandLineArgs |> Array.skip 1 with
    | [| "--"; worker; report |]
    | [| worker; report |] -> Path.GetFullPath worker, Path.GetFullPath report
    | _ -> invalidArg "arguments" "worker-guard-tests.fsx <GPU worker.dll> <report.json>"

let root = Path.Combine(Path.GetDirectoryName reportPath, "worker-guard-inputs", Guid.NewGuid().ToString("N"))
Directory.CreateDirectory root |> ignore
let original = SceneFiles.load (Path.Combine(__SOURCE_DIRECTORY__, "sampled-lights-scene.json"))
let settings =
    { SceneFiles.preset "quick" with
        Width = 1; Height = 1; CameraSamples = 1; LightSamples = 1; GlossySamples = 1
        MaxBounces = 0; Threads = 1; TileSize = 1 }
let writeJson path value = File.WriteAllText(path, JsonSerializer.Serialize(value, SceneFiles.jsonOptions))

let probes =
    [| "metrics-scene", "scene", false, false, false
       "metrics-settings", "settings", false, false, false
       "metrics-mesh-load-failure", "mesh", false, false, false
       "metrics-texture-load-failure", "texture", false, false, false
       "metrics-mesh-invalid-settings", "mesh", true, false, false
       "metrics-texture-float32", "texture", false, true, false
       "metrics-texture-invalid-extension", "texture", false, false, true
       "metrics-pending-mesh", "pending-mesh", false, false, false
       "metrics-file-symlink", "file-symlink", false, false, false
       "metrics-directory-symlink", "directory-symlink", false, false, false
       "safe-metrics-load-failure", "safe", false, false, false
       "safe-metrics-invalid-settings", "safe", true, false, false |]

let results =
    probes
    |> Array.map (fun (name, target, invalidSettings, float32, invalidExtension) ->
        let directory = Path.Combine(root, name)
        Directory.CreateDirectory directory |> ignore
        let scenePath = Path.Combine(directory, "scene.json")
        let settingsPath = Path.Combine(directory, "settings.json")
        let safeMetrics = Path.Combine(directory, "metrics.json")
        let meshPath =
            if target = "pending-mesh" then safeMetrics + ".gpu-pending"
            else Path.Combine(directory, "mesh.ply")
        let texturePath = Path.Combine(directory, "texture.png")
        File.WriteAllText(meshPath, "protected deliberately invalid PLY\n")
        File.WriteAllText(texturePath, "protected deliberately invalid PNG\n")
        let scene =
            { original with
                Meshes = original.Meshes |> Array.map (fun mesh -> { mesh with Path = meshPath })
                Materials = original.Materials |> Array.map (fun material -> { material with Texture = texturePath }) }
        writeJson scenePath scene
        if invalidSettings then File.WriteAllText(settingsPath, "{")
        else writeJson settingsPath { settings with Precision = if float32 then "float32" else "float64" }
        let mutable linkPath = ""
        let mutable linkedDirectory = ""
        let metricsPath =
            match target with
            | "scene" -> scenePath
            | "settings" -> settingsPath
            | "mesh" -> meshPath
            | "texture" -> texturePath
            | "file-symlink" ->
                linkPath <- Path.Combine(directory, "metrics-link.json")
                File.CreateSymbolicLink(linkPath, meshPath) |> ignore
                linkPath
            | "directory-symlink" ->
                linkedDirectory <- Path.Combine(directory, "linked")
                Directory.CreateSymbolicLink(linkedDirectory, directory) |> ignore
                Path.Combine(linkedDirectory, Path.GetFileName meshPath)
            | _ -> safeMetrics
        let protectedInputs = [| scenePath; settingsPath; meshPath; texturePath |]
        let before = protectedInputs |> Array.map (fun path -> path, SceneFiles.hashFile path)
        let output = Path.Combine(directory, if invalidExtension then "render.invalid" else "render.png")
        let linear = Path.Combine(directory, "linear.pfm")
        let start = ProcessStartInfo(Environment.ProcessPath)
        start.UseShellExecute <- false
        start.RedirectStandardOutput <- true
        start.RedirectStandardError <- true
        for argument in
            [| worker; "--scene"; scenePath; "--material"; "authored"; "--settings"; settingsPath
               "--output"; output; "--linear"; linear; "--metrics"; metricsPath |] do
            start.ArgumentList.Add argument
        use child = new Process()
        child.StartInfo <- start
        if not (child.Start()) then failwith "Could not start the GPU worker."
        let stdout = child.StandardOutput.ReadToEndAsync()
        let stderr = child.StandardError.ReadToEndAsync()
        child.WaitForExit()
        stdout.GetAwaiter().GetResult() |> ignore
        let error = stderr.GetAwaiter().GetResult()
        let failure = JsonSerializer.Deserialize<RenderMetrics>(error, SceneFiles.jsonOptions)
        let structuredError =
            not (obj.ReferenceEquals(failure, null))
            && failure.Engine = "gpu" && failure.Backend = "cuda-compute"
            && (failure.Status = "failure" || failure.Status = "unavailable")
            && not (String.IsNullOrWhiteSpace failure.Error)
        let changed =
            before |> Array.choose (fun (path, digest) ->
                if not (File.Exists path) || SceneFiles.hashFile path <> digest then Some path else None)
        let linkPreserved = linkPath = "" || not (isNull (FileInfo(linkPath).LinkTarget))
        let safeFailureSaved =
            if target <> "safe" then true
            elif not (File.Exists safeMetrics) then false
            else
                use saved = JsonDocument.Parse(File.ReadAllText safeMetrics)
                saved.RootElement.GetProperty("status").GetString() = "failure"
        let passed =
            child.ExitCode <> 0 && changed.Length = 0 && linkPreserved && safeFailureSaved
            && not (File.Exists output || File.Exists linear)
            && structuredError
        let result =
            {| name = name; passed = passed; exitCode = child.ExitCode
               changedInputs = changed; linkPreserved = linkPreserved
               safeFailureSaved = safeFailureSaved; structuredError = structuredError |}
        if linkedDirectory <> "" then Directory.Delete linkedDirectory
        if linkPath <> "" && File.Exists linkPath then File.Delete linkPath
        for path in [| scenePath; settingsPath; meshPath; texturePath; safeMetrics; safeMetrics + ".gpu-pending" |]
                    |> Array.distinct do
            if File.Exists path then File.Delete path
        Directory.Delete directory
        result)

Directory.Delete root
let passed = results |> Array.forall (fun result -> result.passed)
writeJson reportPath
    {| status = (if passed then "passed" else "failed")
       workerSha256 = SceneFiles.hashFile worker; cases = results |}
printfn "GPU failure-output guards: %d/%d passed; report=%s"
        (results |> Array.filter (fun result -> result.passed) |> Array.length) results.Length reportPath
if not passed then
    for result in results |> Array.filter (fun result -> not result.passed) do printfn "%A" result
    exit 1
