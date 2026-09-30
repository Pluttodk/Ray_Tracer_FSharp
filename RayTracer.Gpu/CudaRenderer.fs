namespace Tracer.Gpu

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text.Json
open ILGPU
open ILGPU.Algorithms
open ILGPU.Runtime
open ILGPU.Runtime.Cuda
open Tracer.SceneFormat

type PreparedGpuScene =
    { Triangles: DeviceTriangle array
      Nodes: DeviceNode array
      PrimitiveIndices: int array
      Materials: DeviceMaterial array
      ObjectMaterials: int array
      WorldToObject: DeviceTransform array
      ObjectToWorld: DeviceTransform array
      Lights: DeviceLight array
      TexturePixels: V3 array
      Samples: V2 array
      Camera: DeviceCamera
      Ambient: V3
      Background: V3
      CameraSampleOffset: int
      CameraSampleSets: int
      BvhDepth: int }

type GpuRenderResult =
    { Pixels: V3 array
      Device: string
      CompileMs: double
      UploadMs: double
      TraceMs: double
      DownloadMs: double
      CleanupMs: double
      PeakDeviceBytes: int64
      BatchPixels: int
      Batches: int
      RayStackCapacity: int
      MediumStackCapacity: int
      TraversalStackCapacity: int }

type CudaExecutionException(message: string, invalidPixels: int) =
    inherit InvalidOperationException(message)
    member _.InvalidPixels = invalidPixels

type private ResourceScope() =
    let resources = ResizeArray<IDisposable>()
    let mutable cleanupMs = 0.
    member _.Add<'T when 'T :> IDisposable>(resource: 'T) =
        resources.Add resource
        resource
    member _.CleanupMs = cleanupMs
    member _.Dispose(throwOnFailure: bool) =
        let clock = Stopwatch.StartNew()
        let mutable firstError: exn option = None
        for index = resources.Count - 1 downto 0 do
            try resources.[index].Dispose()
            with error -> if firstError.IsNone then firstError <- Some error
        resources.Clear()
        cleanupMs <- clock.Elapsed.TotalMilliseconds
        match firstError with
        | Some error when throwOnFailure -> raise error
        | Some error ->
            eprintfn "%s" (JsonSerializer.Serialize({| status = "error"; backend = "cuda-compute"; cleanupError = error.Message |}))
        | None -> ()

module CudaRenderer =
    let private errorMessage code =
        match code with
        | 1 -> "BVH traversal stack capacity exceeded."
        | 2 -> "Dielectric medium stack capacity exceeded."
        | 3 -> "Transparent visibility exceeded 2048 surface crossings."
        | 4 -> "A transparent shadow ray failed to advance."
        | 5 -> "CUDA detected non-finite/negative radiance or an invalid ray/light sample."
        | 6 -> "Secondary-ray work exceeded its validated bound."
        | 7 -> "Secondary-ray stack capacity exceeded."
        | 8 -> "Camera-medium classification exceeded 2048 surface crossings."
        | _ -> $"Unknown device error {code}."

    let private checkErrors (errors: int array) count start =
        let mutable invalidPixels = 0
        for index = 0 to count - 1 do
            if errors.[index] = 5 then invalidPixels <- invalidPixels + 1
        for index = 0 to count - 1 do
            if errors.[index] <> 0 then
                raise (CudaExecutionException(
                    $"CUDA failed at pixel {start + index}: {errorMessage errors.[index]} No CPU fallback was attempted.", invalidPixels))

    let private capacities (scene: PreparedGpuScene) (settings: RenderSettings) =
        let mutable branching = 1
        for index in scene.ObjectMaterials do
            let material = scene.Materials.[index]
            if material.Kind = 4 then branching <- max branching 2
            elif material.Kind = 3 && material.Reflectivity > 0.f then branching <- max branching settings.GlossySamples
        let rayCapacity64 = 1L + int64 settings.MaxBounces * int64 (branching - 1)
        if rayCapacity64 > 4096L then invalidOp "The requested bounce/glossy branching needs more than 4096 pending rays per sample."
        let mutable levelWork = 1L
        let mutable maximumWork = 1L
        for _ = 1 to settings.MaxBounces do
            levelWork <- levelWork * int64 branching
            maximumWork <- maximumWork + levelWork
            if maximumWork > 1048576L then
                invalidOp "The requested bounce/glossy branching exceeds the explicit 1,048,576-rays-per-sample work bound."
        let traversalCapacity = max 1 (scene.BvhDepth + 2)
        if traversalCapacity > 128 then invalidOp "This CUDA backend supports BVH depth at most 126."
        int rayCapacity64, min 64 (max 1 scene.ObjectMaterials.Length), traversalCapacity, int maximumWork

    let private estimateBytes<'T when 'T: unmanaged> (items: 'T array) =
        int64 (max 1 items.Length) * int64 (Marshal.SizeOf<'T>())

    let private allocate<'T when 'T: unmanaged and 'T: struct and 'T: (new: unit -> 'T) and 'T :> ValueType>
        (scope: ResourceScope) (accelerator: CudaAccelerator) count : MemoryBuffer1D<'T, Stride1D.Dense> =
        scope.Add(accelerator.Allocate1D<'T>(int64 (max 1 count)))

    let private upload<'T when 'T: unmanaged and 'T: struct and 'T: (new: unit -> 'T) and 'T :> ValueType>
        scope accelerator (items: 'T array) =
        let buffer = allocate<'T> scope accelerator items.Length
        if items.Length > 0 then buffer.CopyFromCPU items
        buffer

    let render (scene: PreparedGpuScene) (settings: RenderSettings) =
        SceneFiles.validateSettings settings |> ignore
        if settings.Precision <> "float32" then
            raise (NotSupportedException
                     "The CUDA renderer is FP32. Ada runs FP64 at 1/64 the FP32 rate, so an FP64 device path \
                      cost roughly an order of magnitude for no visible benefit. Use \"precision\": \"float32\", \
                      or the CPU worker if you genuinely need FP64.")
        if scene.WorldToObject.Length <> scene.ObjectMaterials.Length || scene.ObjectToWorld.Length <> scene.ObjectMaterials.Length then
            invalidOp "Every GPU object requires both affine transform records."
        let rayCapacity, mediumCapacity, traversalCapacity, maximumWork = capacities scene settings
        let resources = ResourceScope()
        let mutable completed = false
        let result =
            try
                let clock = Stopwatch.StartNew()
                let createdContext, device = CudaRuntime.createContext ()
                let context = resources.Add createdContext
                let accelerator = resources.Add(device.CreateCudaAccelerator context)
                let classify =
                    accelerator.LoadAutoGroupedStreamKernel<Index1D, DeviceScene, DeviceSettings, DeviceWorkspace>(
                        Action<Index1D, DeviceScene, DeviceSettings, DeviceWorkspace>(DeviceKernels.classifyCamera))
                let kernel =
                    accelerator.LoadAutoGroupedStreamKernel<Index1D, DeviceScene, DeviceSettings, DeviceWorkspace, ArrayView<V3>>(
                        Action<Index1D, DeviceScene, DeviceSettings, DeviceWorkspace, ArrayView<V3>>(DeviceKernels.render))
                accelerator.Synchronize()
                let compileMs = clock.Elapsed.TotalMilliseconds
                let totalPixels = settings.Width * settings.Height
                let fixedBytes =
                    estimateBytes scene.Triangles + estimateBytes scene.Nodes + estimateBytes scene.PrimitiveIndices
                    + estimateBytes scene.Materials + estimateBytes scene.ObjectMaterials + estimateBytes scene.WorldToObject
                    + estimateBytes scene.Lights
                    + estimateBytes scene.TexturePixels + estimateBytes scene.Samples
                    + int64 (mediumCapacity + max 1 scene.ObjectMaterials.Length + 1) * 4L
                let perLaneBytes =
                    int64 rayCapacity * int64 (Marshal.SizeOf<DeviceWork>())
                    + int64 ((rayCapacity + 2) * mediumCapacity + traversalCapacity + 1) * 4L
                    + int64 (Marshal.SizeOf<V3>())
                let freeMemory = accelerator.GetFreeMemory()
                let budget = min (device.MemorySize * 3L / 4L) (freeMemory - 256L * 1024L * 1024L)
                let availableLanes = (budget - fixedBytes) / perLaneBytes
                if availableLanes < 1L then
                    invalidOp $"Insufficient CUDA memory: {fixedBytes} scene bytes and {perLaneBytes} bytes per pixel exceed the safe budget {budget}."
                // Occupancy.
                //
                // This used to be capped at a flat 8,192 lanes, which is about
                // 1.8 threads per CUDA core on this GPU - far too few to hide
                // memory latency in a traversal-bound kernel. The cap was never
                // the real constraint: `availableLanes` already bounds the batch
                // by actual free device memory. FP32 halved the per-lane state,
                // so roughly twice as many lanes now fit in the same budget.
                //
                // The remaining ceiling only exists to keep a single allocation
                // from becoming unreasonable on a huge image; memory still has
                // the final say.
                let laneCeiling = 1 <<< 20
                let batchSize = int (min (int64 (min laneCeiling totalPixels)) availableLanes)
                let deviceBytes = fixedBytes + int64 batchSize * perLaneBytes
                clock.Restart()
                let triangles = upload resources accelerator scene.Triangles
                let nodes = upload resources accelerator scene.Nodes
                let primitiveIndices = upload resources accelerator scene.PrimitiveIndices
                let materials = upload resources accelerator scene.Materials
                let objectMaterials = upload resources accelerator scene.ObjectMaterials
                let worldToObject = upload resources accelerator scene.WorldToObject
                let lights = upload resources accelerator scene.Lights
                let textures = upload resources accelerator scene.TexturePixels
                let samples = upload resources accelerator scene.Samples
                let initialMedia = allocate<int> resources accelerator mediumCapacity
                let rays = allocate<DeviceWork> resources accelerator (batchSize * rayCapacity)
                let media = allocate<int> resources accelerator (batchSize * (rayCapacity + 2) * mediumCapacity)
                let traversal = allocate<int> resources accelerator (batchSize * traversalCapacity)
                let errors = allocate<int> resources accelerator batchSize
                let seen = allocate<int> resources accelerator scene.ObjectMaterials.Length
                let initialCount = allocate<int> resources accelerator 1
                let output = allocate<V3> resources accelerator batchSize
                let sceneView: DeviceScene =
                    { Triangles = triangles.View.BaseView; Nodes = nodes.View.BaseView
                      PrimitiveIndices = primitiveIndices.View.BaseView; Materials = materials.View.BaseView
                      ObjectMaterials = objectMaterials.View.BaseView; Lights = lights.View.BaseView
                      WorldToObject = worldToObject.View.BaseView
                      TexturePixels = textures.View.BaseView; Samples = samples.View.BaseView
                      InitialMedia = initialMedia.View.BaseView }
                let workspace: DeviceWorkspace =
                    { Rays = rays.View.BaseView; Media = media.View.BaseView; Traversal = traversal.View.BaseView
                      Errors = errors.View.BaseView; SeenObjects = seen.View.BaseView; InitialMediumCount = initialCount.View.BaseView }
                let deviceSettings =
                    { Camera = scene.Camera; Ambient = scene.Ambient; Background = scene.Background
                      Width = settings.Width; Height = settings.Height
                      CameraSamples = settings.CameraSamples; CameraSampleSets = scene.CameraSampleSets
                      CameraSampleOffset = scene.CameraSampleOffset
                      GlossySamples = settings.GlossySamples
                      MaxBounces = settings.MaxBounces; Seed = settings.Seed
                      TriangleCount = scene.Triangles.Length; NodeCount = scene.Nodes.Length
                      LightCount = scene.Lights.Length; ObjectCount = scene.ObjectMaterials.Length
                      AllOpaque =
                        if scene.ObjectMaterials |> Array.exists (fun index -> scene.Materials.[index].Kind = 4) then 0 else 1
                      RayCapacity = rayCapacity; MediumCapacity = mediumCapacity; TraversalCapacity = traversalCapacity
                      MaximumRayWork = maximumWork; BatchStart = 0; BatchCount = batchSize }
                accelerator.Synchronize()
                let uploadMs = clock.Elapsed.TotalMilliseconds
                let pixels = Array.zeroCreate<V3> totalPixels
                let mutable traceMs = 0.
                let mutable downloadMs = 0.
                let mutable batches = 0
                clock.Restart()
                classify.Invoke(Index1D 1, sceneView, deviceSettings, workspace)
                accelerator.Synchronize()
                traceMs <- traceMs + clock.Elapsed.TotalMilliseconds
                clock.Restart()
                let classificationError = errors.GetAsArray1D()
                accelerator.Synchronize()
                downloadMs <- downloadMs + clock.Elapsed.TotalMilliseconds
                checkErrors classificationError 1 0
                let mutable batchStart = 0
                while batchStart < totalPixels do
                    let batchCount = min batchSize (totalPixels - batchStart)
                    let batchSettings = { deviceSettings with BatchStart = batchStart; BatchCount = batchCount }
                    clock.Restart()
                    kernel.Invoke(Index1D batchCount, sceneView, batchSettings, workspace, output.View.BaseView)
                    accelerator.Synchronize()
                    traceMs <- traceMs + clock.Elapsed.TotalMilliseconds
                    clock.Restart()
                    let batchPixels = output.GetAsArray1D()
                    let batchErrors = errors.GetAsArray1D()
                    accelerator.Synchronize()
                    downloadMs <- downloadMs + clock.Elapsed.TotalMilliseconds
                    checkErrors batchErrors batchCount batchStart
                    Array.Copy(batchPixels, 0, pixels, batchStart, batchCount)
                    batchStart <- batchStart + batchCount
                    batches <- batches + 1
                let rendered =
                    { Pixels = pixels; Device = $"{device.Name}; CUDA {device.DriverVersion}; sm_{accelerator.Architecture}"
                      CompileMs = compileMs; UploadMs = uploadMs; TraceMs = traceMs; DownloadMs = downloadMs
                      CleanupMs = 0.; PeakDeviceBytes = deviceBytes; BatchPixels = batchSize; Batches = batches
                      RayStackCapacity = rayCapacity; MediumStackCapacity = mediumCapacity; TraversalStackCapacity = traversalCapacity }
                completed <- true
                rendered
            finally
                resources.Dispose completed
        { result with CleanupMs = resources.CleanupMs }
