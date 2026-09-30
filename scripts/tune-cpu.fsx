#r "../BenchmarkRunner/bin/Release/net10.0/Basics.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/SceneFormat.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/RayTracer.ImageIO.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/StbImageSharp.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll"

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open Tracer.Basics
open Tracer.Basics.Render
open Tracer.Benchmarks
open Tracer.Imaging
open Tracer.SceneFormat

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
Directory.SetCurrentDirectory root
let arguments = fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--")
let mutable output = Path.Combine(root, "artifacts", "cpu-tuning")
let mutable sweep = "acceleration"
if arguments.Length % 2 <> 0 then invalidArg "arguments" "Each option requires a value."
for index in 0 .. 2 .. arguments.Length - 1 do
    match arguments.[index] with
    | "--output" -> output <- Path.GetFullPath arguments.[index + 1]
    | "--sweep" when List.contains arguments.[index + 1] ["acceleration"; "bvh-settings"] -> sweep <- arguments.[index + 1]
    | _ -> invalidArg "arguments" "Usage: dotnet fsi scripts/tune-cpu.fsx -- [--output directory] [--sweep acceleration|bvh-settings]"
Directory.CreateDirectory output |> ignore

let decode (path: string) =
    use image = RgbImage.Load path
    { RgbTexture.Width = image.Width; Height = image.Height; Pixels = Array.copy image.Pixels }

let fingerprint (values: float array) =
    let bytes = Array.zeroCreate<byte> (values.Length * sizeof<float>)
    Buffer.BlockCopy(values, 0, bytes, 0, bytes.Length)
    Convert.ToHexString(SHA256.HashData bytes).ToLowerInvariant()

let settings = { SceneFiles.preset "quick" with Width = 192; Height = 192; CameraSamples = 4; LightSamples = 4; MaxBounces = 3 }
let threads = Environment.ProcessorCount
let baseConfigurations =
    [| "flat-default", Acceleration.FlatBVH, threads, 16
       "brute-reference", Acceleration.BruteForce, threads, 16
       "corrected-kdtree", Acceleration.KDTree, threads, 16
       "corrected-bvh", Acceleration.BVH, threads, 16
       "corrected-grid", Acceleration.RegularGrid, threads, 16
       "flat-eight-threads", Acceleration.FlatBVH, min 8 threads, 16
       "flat-sixteen-threads", Acceleration.FlatBVH, min 16 threads, 16
       "flat-small-tiles", Acceleration.FlatBVH, threads, 8
       "flat-large-tiles", Acceleration.FlatBVH, threads, 32 |]
let configurations =
    if sweep = "acceleration" then
        baseConfigurations |> Array.map (fun (name, kind, count, tile) -> name, kind, count, tile, RenderOptions.Default.BvhOptions)
    else
        [| for leaf, bins in [1,16; 2,16; 4,16; 8,16; 1,8; 1,32] do
               yield $"flat-leaf{leaf}-bins{bins}", Acceleration.FlatBVH, threads, 16,
                     { FlatBVH.defaultOptions with LeafSize = leaf; BinCount = bins } |]
let rows = ResizeArray<obj>()
let mutable equivalent = true
for sceneId in ["chair"; "space-sentinel"] do
    let scenePath = Path.Combine(root, "benchmarks", "scenes", sceneId + ".json")
    let built = SceneBuilder.build scenePath "authored" settings decode
    let options =
        { RenderOptions.Default with Threads = threads; TileSize = 16; Seed = settings.Seed
                                     Acceleration = Some Acceleration.BruteForce }
    let reference = Render(built.Scene, built.Camera, options).RenderLinear
    let referenceHash = fingerprint reference.Pixels
    for name, acceleration, count, tile, bvh in configurations do
        let options = { options with Acceleration = Some acceleration; Threads = count; TileSize = tile; BvhOptions = bvh }
        Render(built.Scene, built.Camera, options).RenderLinear |> ignore
        for trial = 1 to 3 do
            let allocated = GC.GetTotalAllocatedBytes true
            let film = Render(built.Scene, built.Camera, options).RenderLinear
            let allocated = GC.GetTotalAllocatedBytes(true) - allocated
            let mutable maximum = 0.
            let mutable squared = 0.
            for index = 0 to film.Pixels.Length - 1 do
                let difference = abs (film.Pixels.[index] - reference.Pixels.[index])
                maximum <- max maximum difference
                squared <- squared + difference * difference
            let rmse = sqrt (squared / float film.Pixels.Length)
            let passed = Double.IsFinite maximum && maximum <= 1e-9 && rmse <= 1e-11
            equivalent <- equivalent && passed
            rows.Add(
                {| scene = sceneId; configuration = name; acceleration = acceleration.ToString()
                   threads = count; tileSize = tile; trial = trial
                   bvhLeafSize = bvh.LeafSize; bvhBinCount = bvh.BinCount; bvhMaxDepth = bvh.MaxDepth
                   buildMs = film.BuildMilliseconds; traceMs = film.TraceMilliseconds
                   allocatedBytes = allocated; referenceFilmSha256 = referenceHash
                   filmSha256 = fingerprint film.Pixels; maximumAbsoluteError = maximum; rootMeanSquareError = rmse
                   equivalent = passed |})
        printfn "%s / %s completed" sceneId name
let report =
    {| schemaVersion = 1; sweep = sweep
       scope = "Same corrected integrator and prepared immutable scene; scene-level acceleration/thread/tile ablation."
       exclusions = "Mesh BLAS remains FlatBVH. Load and PNG/PFM encoding are excluded. This in-process diagnostic is not the cold process-isolated legacy comparison."
       policy = "One unmeasured warmup then three trials per configuration, no forced GC; reference uses brute scene traversal. Exact film hashes and 1e-9 max/1e-11 RMSE gates retained."
       coreAssemblySha256 = SceneFiles.hashFile "BenchmarkRunner/bin/Release/net10.0/Basics.dll"
       workerAssemblySha256 = SceneFiles.hashFile "BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll"
       settings = settings; allEquivalent = equivalent; trials = rows.ToArray() |}
let path = Path.Combine(output, "results.json")
File.WriteAllText(path, JsonSerializer.Serialize(report, SceneFiles.jsonOptions))
printfn "CPU ablation: %s" path
if not equivalent then failwith "A CPU configuration differed from the corrected brute-force reference."
