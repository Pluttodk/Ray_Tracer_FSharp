#load "../SceneFormat/SceneFormat.fs"
#load "BenchmarkData.fs"

open System
open System.IO
open System.Threading
open Tracer.SceneFormat
open Tracer.Benchmarks.Reporting

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let directory = Path.Combine(root, "artifacts", "benchmark-selection-tests", Guid.NewGuid().ToString("N"))
let sceneDirectory = Path.Combine(directory, "scenes")
Directory.CreateDirectory sceneDirectory |> ignore
let host = Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" |> Option.ofObj |> Option.defaultValue Environment.ProcessPath
let mutable assertions = 0
let check name condition =
    if not condition then failwith $"FAIL: {name}"
    assertions <- assertions + 1

try
    let fixture id =
        { SchemaVersion = 1; Id = id; Title = "CLI routing fixture; no renderer is invoked"
          Description = "Empty routing fixture, not presentation geometry or a substitute image."
          Camera = { Position = [| 0.; 0.; 2. |]; Target = [| 0.; 0.; 0. |]; Up = [| 0.; 1.; 0. |]
                     ViewDistance = 1.; ViewWidth = 1.; ViewHeight = 1.; LensRadius = 0.; FocusDistance = 2. }
          AmbientColour = [| 0.; 0.; 0. |]; AmbientIntensity = 0.; MaxBounces = 0
          Materials = [||]; Meshes = [||]; Objects = [||]; Lights = [||]; Subjects = [||] }
    for id in [ "chair"; "roman-bust"; "space-sentinel"; "sky-arena"; "gold-dragon" ] do
        SceneFiles.save (Path.Combine(sceneDirectory, id + ".json")) (fixture id)
    let invoke name presets arguments =
        let output = Path.Combine(directory, name)
        let command =
            [ "fsi"; Path.Combine(root, "scripts/benchmark.fsx"); "--"
              "--scene-dir"; sceneDirectory; "--engines"; "gpu"
              "--gpu-worker"; Path.Combine(directory, "intentionally-unavailable-worker.dll")
              "--presets"; presets; "--warmups"; "0"; "--output"; output ] @ arguments
        let outcome = Data.runProcess root host command [] 120. CancellationToken.None
        Data.writeText (Path.Combine(output, "selection-test.stdout.log")) outcome.Stdout
        Data.writeText (Path.Combine(output, "selection-test.stderr.log")) outcome.Stderr
        outcome, output
    let primary, primaryOutput = invoke "primary" "standard,high" [ "--scenes"; "all"; "--materials"; "all"; "--skip-prepare" ]
    check "routing-only matrix reports unavailable worker, not success" (primary.ExitCode = Nullable 1)
    let primaryManifest = Data.deserialize<RunManifest> (Path.Combine(primaryOutput, "run-manifest.json"))
    check "actual standard/high CLI matrix remains 48 cases per engine" (primaryManifest.Cases.Length = 48)
    check "gold dragon is excluded even when present in the scene directory" (primaryManifest.Cases |> Array.forall (fun scene -> scene.Scene <> "gold-dragon"))
    check "all four primary IDs and six variants are retained"
        ((primaryManifest.Cases |> Array.map (fun scene -> scene.Scene) |> Array.distinct |> Array.length) = 4
         && (primaryManifest.Cases |> Array.map (fun scene -> scene.Material) |> Array.distinct |> Array.length) = 6)
    let canonicalProvenance = Path.Combine(root, "benchmarks/scenes/provenance.json")
    let expectedProvenance = [| canonicalProvenance |] |> Array.filter File.Exists |> Array.map Data.hashFile
    let obsoleteManifest = Path.Combine(root, "artifacts/scene-assets/manifest.json")
    check "prepared canonical provenance is fingerprinted without an alternate manifest alias"
        (primaryManifest.Cases |> Array.forall (fun scene ->
            (scene.InputFiles |> Array.filter (fun input -> input.Path = canonicalProvenance)) = expectedProvenance
            && scene.InputFiles |> Array.forall (fun input -> input.Path <> obsoleteManifest)))
    check "standard/high defaults require five measured repeats for every case"
        (primaryManifest.RequestedMeasuredTrials = 240
         && primaryManifest.Cases |> Array.forall (fun scene -> scene.Repeats = 5))
    let primaryTrials = Data.deserialize<Trial array> (Path.Combine(primaryOutput, "trials.json"))
    check "all 240 unavailable measured trials are retained without synthetic images"
        (primaryTrials.Length = 240
         && primaryTrials |> Array.forall (fun trial -> not trial.Warmup && trial.Status = "unavailable" && not (File.Exists trial.OutputPath)))
    let quick, quickOutput =
        invoke "quick-default" "quick" [ "--scenes"; "chair"; "--materials"; "matte"; "--skip-prepare" ]
    check "quick default reaches the explicitly unavailable worker" (quick.ExitCode = Nullable 1)
    let quickManifest = Data.deserialize<RunManifest> (Path.Combine(quickOutput, "run-manifest.json"))
    check "quick defaults to one measured repeat"
        (quickManifest.RequestedMeasuredTrials = 1 && quickManifest.Cases.Length = 1 && quickManifest.Cases.[0].Repeats = 1)
    let bonus, bonusOutput = invoke "bonus" "standard,high" [ "--scenes"; "gold-dragon"; "--materials"; "authored" ]
    check "extra scene selector reaches workers rather than rejecting its ID" (bonus.ExitCode = Nullable 1)
    check "bonus-only command never invokes procedural generation" (not (bonus.Stdout.Contains "scene-assets"))
    let bonusManifest = Data.deserialize<RunManifest> (Path.Combine(bonusOutput, "run-manifest.json"))
    check "bonus is authored only, once per explicitly requested preset"
        (bonusManifest.Cases.Length = 2 && bonusManifest.Cases |> Array.forall (fun scene -> scene.Scene = "gold-dragon" && scene.Material = "authored"))
    let bonusProvenance =
        [| Path.Combine(root, "benchmarks/gold-dragon.json")
           Path.Combine(root, "artifacts/scene-assets/gold-dragon/staging-plane.provenance.json") |]
    let expectedBonusProvenance = bonusProvenance |> Array.filter File.Exists |> Array.map Data.hashFile
    check "bonus parent and generated floor provenance are separately fingerprinted"
        (bonusManifest.Cases |> Array.forall (fun scene ->
            expectedBonusProvenance |> Array.forall (fun provenance -> Array.contains provenance scene.InputFiles)))
    let bonusTrials = Data.deserialize<Trial array> (Path.Combine(bonusOutput, "trials.json"))
    check "unavailable GPU produces no bonus replacement image"
        (bonusTrials |> Array.forall (fun trial -> trial.Status = "unavailable" && not (File.Exists trial.OutputPath)))
    let bonusHtml = File.ReadAllText(Path.Combine(bonusOutput, "index.html"))
    check "report labels bonus outside the primary matrix"
        (bonusHtml.Contains "explicitly selected authored-only bonus")
    check "bonus report credits Stanford without relicensing optional data"
        (bonusHtml.Contains "Stanford University Computer Graphics Laboratory" && bonusHtml.Contains "not relicensed")
    check "bonus report links the research and redistribution terms"
        (bonusHtml.Contains "https://graphics.stanford.edu/data/3Dscanrep/"
         && bonusHtml.Contains "noncommercial research/free redistribution"
         && bonusHtml.Contains "commercial use requires Stanford permission")
    check "bonus report distinguishes finite-floor benchmark from direct original geometry"
        (bonusHtml.Contains "finite 200000x200000 slab floor"
         && bonusHtml.Contains "direct original reproduction uses a truly infinite plane"
         && bonusHtml.Contains "Matching original settings does not make these two geometries identical")
    let rejected, _ = invoke "rejected-sweep" "standard,high" [ "--scenes"; "gold-dragon"; "--materials"; "all" ]
    check "whole bonus material sweep is rejected before execution"
        (rejected.ExitCode = Nullable 2 && rejected.Stderr.Contains "authored/gold-only")
    let direct, _ =
        invoke "direct-rejected-sweep" "standard,high"
            [ "--scene"; Path.Combine(sceneDirectory, "gold-dragon.json"); "--materials"; "glass"; "--skip-prepare" ]
    check "custom --scene path cannot bypass authored-only policy"
        (direct.ExitCode = Nullable 2 && direct.Stderr.Contains "authored/gold-only")
    File.Delete(Path.Combine(sceneDirectory, "gold-dragon.json"))
    let missing, _ = invoke "missing-neutral-input" "standard,high" [ "--scenes"; "gold-dragon"; "--materials"; "authored" ]
    check "missing bonus scene has explicit preparation diagnostic, never a download"
        (missing.ExitCode = Nullable 2 && missing.Stderr.Contains "prepared neutral scene" && not (missing.Stdout.Contains "scene-assets"))
    printfn "All %d CLI scene-selection assertions passed; no renderer or asset downloader was invoked." assertions
finally
    Directory.Delete(directory, true)
