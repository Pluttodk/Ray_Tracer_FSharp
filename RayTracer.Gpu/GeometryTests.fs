namespace Tracer.Gpu

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.Json
open ILGPU
open ILGPU.Backends.PTX
open ILGPU.Runtime
open ILGPU.Runtime.Cuda
open Tracer.SceneFormat
open DeviceMath
open DeviceGeometry

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type BoundaryProbe =
    { Ray: DeviceRay
      Hit: DeviceHit
      LinearHit: DeviceHit
      Object: int
      FrontFace: int }

module GeometryTests =
    let private probeEdge (index: Index1D) (vertices: ArrayView<V3>) (output: ArrayView<V2>) =
        let a = vertices.[index.X * 2]
        let b = vertices.[index.X * 2 + 1]
        output.[index.X] <- v2 (a.X * b.Y - a.Y * b.X) (edge a b)

    let private probe (index: Index1D) (scene: DeviceScene) (settings: DeviceSettings)
                      (workspace: DeviceWorkspace) (output: ArrayView<BoundaryProbe>) =
        let lane = index.X
        workspace.Errors.[lane] <- 0
        let camera = settings.Camera
        let px = camera.PixelWidth * (float32 lane - float32 settings.Width / 2.f + 0.5f)
        let py = camera.PixelHeight * (float32 lane - float32 settings.Height / 2.f + 0.5f)
        let direction =
            normalize (sub (add (scale camera.V px) (scale camera.U py)) (scale camera.W camera.ViewDistance))
        let mutable ray = { Origin = camera.Position; Direction = direction }
        for step = 0 to 2 do
            let hit = closest scene settings workspace lane ray 0.f Single.PositiveInfinity 0
            let mutable linearHit = { Time = Single.PositiveInfinity; Beta = 0.f; Gamma = 0.f; Triangle = -1 }
            for triangleIndex = 0 to settings.TriangleCount - 1 do
                let triangle = scene.Triangles.[triangleIndex]
                let inverse = scene.WorldToObject.[triangle.Object]
                let localRay =
                    { Origin = transformPoint inverse ray.Origin; Direction = transformVector inverse ray.Direction }
                let candidate = triangleHit triangle triangleIndex localRay 0.f linearHit.Time
                if candidate.Triangle >= 0 && (candidate.Time < linearHit.Time || linearHit.Triangle < 0) then
                    linearHit <- candidate
            let mutable objectId = -1
            let mutable frontFace = -1
            let previousRay = ray
            if hit.Triangle >= 0 then
                let boundary = surface scene ray hit
                objectId <- boundary.Object
                frontFace <- boundary.FrontFace
                let etaI = if step = 0 then 1.2f else 1.5f
                let etaT = if step = 0 then 1.5f else 1.f
                let transmission = dielectric (normalize ray.Direction) (neg boundary.Geometric) etaI etaT
                ray <- spawn boundary transmission.Direction
            output.[lane * 3 + step] <-
                { Ray = previousRay; Hit = hit; LinearHit = linearHit; Object = objectId; FrontFace = frontFace }

    let run reportPath =
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath reportPath)) |> ignore
        let settings =
            { SceneFiles.preset "quick" with
                Width = 64; Height = 64; CameraSamples = 1; LightSamples = 1
                GlossySamples = 1; MaxBounces = 2; Sampler = "regular"; Seed = 2026
                // The shared preset is FP64 for the CPU workers; the CUDA
                // renderer is FP32.
                Precision = "float32" }
        let prepared = (HostScene.load "RayTracer.Gpu/Tests/nested-glass-scene.json" "authored" settings).Prepared
        let createdContext, device = CudaRuntime.createContext ()
        use context = createdContext
        use accelerator = device.CreateCudaAccelerator context
        let kernel =
            accelerator.LoadAutoGroupedStreamKernel<Index1D, DeviceScene, DeviceSettings, DeviceWorkspace, ArrayView<BoundaryProbe>>(
                Action<Index1D, DeviceScene, DeviceSettings, DeviceWorkspace, ArrayView<BoundaryProbe>>(probe))
        match kernel.GetCompiledKernel() with
        | :? PTXCompiledKernel as compiled -> File.WriteAllText(Path.ChangeExtension(reportPath, ".ptx"), compiled.PTXAssembly)
        | _ -> invalidOp "The boundary probe requires a compiled CUDA PTX kernel."
        use triangles = accelerator.Allocate1D<DeviceTriangle>(int64 prepared.Triangles.Length)
        use nodes = accelerator.Allocate1D<DeviceNode>(int64 prepared.Nodes.Length)
        use indices = accelerator.Allocate1D<int>(int64 prepared.PrimitiveIndices.Length)
        use transforms = accelerator.Allocate1D<DeviceTransform>(int64 prepared.WorldToObject.Length)
        use traversal = accelerator.Allocate1D<int>(int64 (64 * (prepared.BvhDepth + 2)))
        use errors = accelerator.Allocate1D<int>(64L)
        use output = accelerator.Allocate1D<BoundaryProbe>(192L)
        triangles.CopyFromCPU prepared.Triangles
        nodes.CopyFromCPU prepared.Nodes
        indices.CopyFromCPU prepared.PrimitiveIndices
        transforms.CopyFromCPU prepared.WorldToObject
        let scene =
            { Unchecked.defaultof<DeviceScene> with
                Triangles = triangles.View.BaseView; Nodes = nodes.View.BaseView
                PrimitiveIndices = indices.View.BaseView; WorldToObject = transforms.View.BaseView }
        let workspace =
            { Unchecked.defaultof<DeviceWorkspace> with
                Traversal = traversal.View.BaseView; Errors = errors.View.BaseView }
        let deviceSettings =
            { Unchecked.defaultof<DeviceSettings> with
                Camera = prepared.Camera; Width = 64; Height = 64
                TriangleCount = prepared.Triangles.Length; NodeCount = prepared.Nodes.Length
                TraversalCapacity = prepared.BvhDepth + 2 }
        kernel.Invoke(Index1D 64, scene, deviceSettings, workspace, output.View.BaseView)
        accelerator.Synchronize()
        let observed = output.GetAsArray1D()
        let deviceErrors = errors.GetAsArray1D()
        accelerator.Synchronize()
        let edgeInputs =
            [| for lane = 0 to 63 do
                   let ray = observed.[lane * 3 + 1].Ray
                   let triangle = prepared.Triangles.[2]
                   let inverse = prepared.WorldToObject.[triangle.Object]
                   let localRay =
                       { Origin = transformPoint inverse ray.Origin; Direction = transformVector inverse ray.Direction }
                   let sx = -localRay.Direction.X / localRay.Direction.Z
                   let sy = -localRay.Direction.Y / localRay.Direction.Z
                   let sz = 1.f / localRay.Direction.Z
                   let b = shearVertex triangle.B localRay 0 1 2 sx sy sz
                   let c = shearVertex triangle.C localRay 0 1 2 sx sy sz
                   yield b
                   yield c
                   yield c
                   yield b
               let point = v3 0.7f -0.7f 0.f
               yield point
               yield point
               yield point
               yield point |]
        let edgeKernel =
            accelerator.LoadAutoGroupedStreamKernel<Index1D, ArrayView<V3>, ArrayView<V2>>(
                Action<Index1D, ArrayView<V3>, ArrayView<V2>>(probeEdge))
        use edgeVertices = accelerator.Allocate1D<V3>(int64 edgeInputs.Length)
        use edgeOutput = accelerator.Allocate1D<V2>(int64 (edgeInputs.Length / 2))
        edgeVertices.CopyFromCPU edgeInputs
        edgeKernel.Invoke(Index1D (edgeInputs.Length / 2), edgeVertices.View.BaseView, edgeOutput.View.BaseView)
        accelerator.Synchronize()
        let edgeValues = edgeOutput.GetAsArray1D()
        accelerator.Synchronize()
        let edgeCases =
            Array.init (edgeInputs.Length / 4) (fun index ->
                let forward, backward = edgeValues.[index * 2], edgeValues.[index * 2 + 1]
                {| a = edgeInputs.[index * 4]; b = edgeInputs.[index * 4 + 1]
                   rawForward = forward.X; rawReverse = backward.X
                   canonicalForward = forward.Y; canonicalReverse = backward.Y
                   rawAntisymmetric = forward.X = -backward.X
                   passed = finite forward.Y && finite backward.Y && forward.Y = -backward.Y |})
        let cases =
            observed
            |> Array.mapi (fun index actual ->
                let mutable reference = { Time = Single.PositiveInfinity; Beta = 0.f; Gamma = 0.f; Triangle = -1 }
                for triangleIndex = 0 to prepared.Triangles.Length - 1 do
                    let triangle = prepared.Triangles.[triangleIndex]
                    let inverse = prepared.WorldToObject.[triangle.Object]
                    let localRay =
                        { Origin = transformPoint inverse actual.Ray.Origin
                          Direction = transformVector inverse actual.Ray.Direction }
                    let candidate = triangleHit triangle triangleIndex localRay 0.f reference.Time
                    if candidate.Triangle >= 0 && (candidate.Time < reference.Time || reference.Triangle < 0) then
                        reference <- candidate
                let step = index % 3
                let expectedObject = if step = 0 then 1 elif step = 1 then 0 else -1
                let hostObject = if reference.Triangle < 0 then -1 else prepared.Triangles.[reference.Triangle].Object
                let linearObject =
                    if actual.LinearHit.Triangle < 0 then -1 else prepared.Triangles.[actual.LinearHit.Triangle].Object
                {| x = index / 3; y = 63 - index / 3; step = step
                   passed = actual.Object = expectedObject && linearObject = expectedObject && hostObject = expectedObject
                   expectedObject = expectedObject; cudaObject = actual.Object; cudaLinearObject = linearObject
                   scalarObject = hostObject; ray = actual.Ray; triangle = actual.Hit.Triangle
                   frontFace = actual.FrontFace; time = actual.Hit.Time.ToString("R", Globalization.CultureInfo.InvariantCulture)
                   scalarTime = reference.Time.ToString("R", Globalization.CultureInfo.InvariantCulture) |})
        let failures = cases |> Array.filter (fun item -> not item.passed)
        let edgeFailures = edgeCases |> Array.filter (fun item -> not item.passed)
        let passed = failures.Length = 0 && edgeFailures.Length = 0 && (deviceErrors |> Array.forall ((=) 0))
        let report =
            {| status = if passed then "passed" else "failed"
               device = device.Name; generatedUtc = DateTimeOffset.UtcNow
               note = "Actual CUDA shared-diagonal inner/outer/air transmission, with BVH and linear CUDA traversal plus scalar F# intersections of downloaded rays. Not a CPU accelerator or rendering fallback."
               cases = cases; failures = failures.Length; deviceErrors = deviceErrors
               edgeCases = edgeCases; edgeFailures = edgeFailures.Length
               rawAntisymmetryFailures = edgeCases |> Array.filter (fun item -> not item.rawAntisymmetric) |> Array.length |}
        File.WriteAllText(reportPath, JsonSerializer.Serialize(report, SceneFiles.jsonOptions) + Environment.NewLine)
        printfn "CUDA boundary probe: %d failures across %d boundary queries; report=%s" failures.Length cases.Length reportPath
        if not passed then invalidOp "CUDA shared-diagonal boundary probe failed."
