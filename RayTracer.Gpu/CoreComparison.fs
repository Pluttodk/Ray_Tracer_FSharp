namespace Tracer.Gpu

open System
open System.IO
open System.Text.Json
open Tracer.Basics
open Tracer.Basics.Render
open Tracer.SceneFormat

/// Agreement bounds between the FP32 CUDA renderer and the FP64 CPU renderer.
/// See the note at the comparison site for why exact equality is no longer
/// available.
[<AutoOpen>]
module private Tolerances =
    /// Maximum absolute error is the WRONG primary metric for FP32 against
    /// FP64, so it is not the gate.
    ///
    /// Measured over the four benchmark scenes at 256x256: the median pixel
    /// differs by ~1e-8, the 99th percentile by ~3e-6 - but roughly 15 pixels
    /// in 65,536 differ by more than 1e-2. Those are silhouette pixels, where a
    /// ray passing within an FP32 ULP of a triangle edge lands on different
    /// geometry and returns a completely different colour. That is inherent to
    /// reducing precision, not a defect, and no max-error bound can distinguish
    /// it from a real fault.
    ///
    /// The gate is therefore RMSE, which is insensitive to a handful of edge
    /// flips but moves immediately if transport is actually wrong, plus a cap
    /// on how many pixels may flip.
    [<Literal>]
    let MaximumRootMeanSquareError = 2.e-3

    /// Fraction of pixels allowed to exceed EdgeFlipThreshold. Measured 0.02%.
    [<Literal>]
    let MaximumEdgeFlipFraction = 0.002

    [<Literal>]
    let EdgeFlipThreshold = 1.e-2

module private ReferenceData =
    /// Device data is FP32; the CPU reference renderer is FP64. Widening here is
    /// exact - every float32 is representable as a double - so the reference is
    /// built from precisely the values the device was given, and any difference
    /// in the result is the device's arithmetic, not its inputs.
    let inline private d (value: float32) = double value

    let point (value: V3) = Point(d value.X, d value.Y, d value.Z)
    let vector (value: V3) = Vector(d value.X, d value.Y, d value.Z)
    let colour (value: V3) = Colour(d value.X, d value.Y, d value.Z)

    let sampler (scene: PreparedGpuScene) offset count sets =
        Sampling.Sampler(
            Array.init sets (fun setIndex ->
                Array.init count (fun index ->
                    let value = scene.Samples.[offset + setIndex * count + index]
                    d value.X, d value.Y)))

    let material (data: DeviceMaterial) (colourValue: V3) (ambientValue: V3) (sampler: Sampling.Sampler) : Material =
        let c, ambient = colour colourValue, colour ambientValue
        let specular, reflection = colour data.SpecularColour, colour data.ReflectionColour
        match data.Kind with
        | 0 -> MatteMaterial(ambient, (d data.Ambient), c, (d data.Diffuse))
        | 1 -> PhongMaterial(ambient, (d data.Ambient), c, (d data.Diffuse), specular, (d data.Specular), data.Exponent)
        | 2 -> PhongReflectiveMaterial(ambient, (d data.Ambient), c, (d data.Diffuse), specular, (d data.Specular),
                                      reflection, (d data.Reflectivity), data.Exponent)
        | 3 -> PhongGlossyReflectiveMaterial(ambient, (d data.Ambient), c, (d data.Diffuse), specular, (d data.Specular),
                                             reflection, (d data.Reflectivity), data.Exponent, data.GlossExponent, sampler)
        | 4 -> TransparentMaterial(colour data.Filter, Colour.White, (d data.Ior), 1.)
        | 5 -> EmissiveMaterial(c, (d data.Emission))
        | kind -> invalidOp $"Unknown reference material kind {kind}."

    let materialAt (scene: PreparedGpuScene) (data: DeviceMaterial) sampler =
        if data.TextureWidth = 0 then
            let constant = material data data.Colour data.AmbientColour sampler
            fun _ _ -> constant
        else
            let materials =
                Array.init (data.TextureWidth * data.TextureHeight) (fun index ->
                    let texel = scene.TexturePixels.[data.TextureOffset + index]
                    material data (DeviceMath.mul data.Colour texel) (DeviceMath.mul data.AmbientColour texel) sampler)
            fun (u: double) (v: double) ->
                let u = max 0. (min 1. u)
                let v = max 0. (min 1. v)
                let x = min (data.TextureWidth - 1) (int (u * double data.TextureWidth))
                let y = min (data.TextureHeight - 1) (int ((1. - v) * double data.TextureHeight))
                materials.[y * data.TextureWidth + x]

type private ReferenceTriangle(data: DeviceTriangle, materialAt: double -> double -> Material, blank: Material) =
    inherit Shape()
    let basis = Triangle(ReferenceData.point data.A, ReferenceData.point data.B, ReferenceData.point data.C, blank)
    member _.Data = data
    override _.getBoundingBox() = basis.getBoundingBox()
    override _.isInside _ = false
    override this.hitFunction ray =
        let hit = basis.hitFunction ray
        if not hit.DidHit then hit
        else
            let alpha, beta, gamma = hit.BarycentricAlpha, hit.BarycentricBeta, hit.BarycentricGamma
            let normal =
                alpha * ReferenceData.vector data.NormalA + beta * ReferenceData.vector data.NormalB
                + gamma * ReferenceData.vector data.NormalC
            // Barycentrics come from the FP64 CPU hit; the UVs are device FP32.
            // Widen the UVs so the interpolation matches the CPU reference.
            let uA, uB, uC = double data.UvA.X, double data.UvB.X, double data.UvC.X
            let vA, vB, vC = double data.UvA.Y, double data.UvB.Y, double data.UvC.Y
            let u = uA + beta * (uB - uA) + gamma * (uC - uA)
            let v = vA + beta * (vB - vA) + gamma * (vC - vA)
            HitPoint(ray, hit.Time, ReferenceData.vector data.GeometricNormal, normal,
                     materialAt u v, this, u, v, beta, gamma, true)

type private ReferenceObject(triangles: Shape array) =
    inherit Shape()
    let acceleration = Acceleration.buildWith Acceleration.FlatBVH triangles
    let bounds =
        let boxes = triangles |> Array.map (fun triangle -> triangle.getBoundingBox())
        let low, high = BVH.findOuterBoundingBoxLowHighPoints boxes
        BBox(low, high)
    override _.getBoundingBox() = bounds
    member _.TriangleHit ray = Acceleration.traverseClosest acceleration ray 0. infinity
    override _.isInside _ = false
    override this.hitFunction ray =
        let hit = Acceleration.traverseClosest acceleration ray 0. infinity
        if hit.DidHit then hit.WithShape this else hit

module CoreComparison =
    let run scenePath variant settingsPath reportPath =
        use settingsStream = File.OpenRead settingsPath
        let settings = JsonSerializer.Deserialize<RenderSettings>(settingsStream, SceneFiles.jsonOptions) |> SceneFiles.validateSettings
        let loaded = HostScene.load scenePath variant settings
        if loaded.Spec.Lights |> Array.exists (fun light -> light.Kind <> LightKind.Point && light.Kind <> LightKind.Directional) then
            invalidOp "This CPU-core comparison isolates the point/directional primary benchmark scenes."
        let prepared = loaded.Prepared
        let gpu = CudaRenderer.render prepared settings
        let materialAt =
            prepared.Materials
            |> Array.map (fun data ->
                let sampler =
                    if data.Kind = 3 then
                        ReferenceData.sampler prepared data.GlossySampleOffset settings.GlossySamples data.GlossySampleSets
                    else Sampling.regular 1
                ReferenceData.materialAt prepared data sampler)
        let blank = Material.None
        let objects =
            prepared.Triangles
            |> Array.map (fun triangle ->
                let matrix = prepared.ObjectToWorld.[triangle.Object]
                { triangle with A = DeviceMath.transformPoint matrix triangle.A
                                B = DeviceMath.transformPoint matrix triangle.B
                                C = DeviceMath.transformPoint matrix triangle.C })
            |> Array.groupBy (fun triangle -> triangle.Object)
            |> Array.sortBy fst
            |> Array.map (fun (_, triangles) ->
                triangles
                |> Array.map (fun data -> ReferenceTriangle(data, materialAt.[data.Material], blank) :> Shape)
                |> fun triangles -> ReferenceObject(triangles) :> Shape)
        let lights =
            loaded.Spec.Lights
            |> Array.map (fun light ->
                let radiance = Colour(light.Colour.[0], light.Colour.[1], light.Colour.[2])
                if light.Kind = LightKind.Point then
                    PointLight(radiance, light.Intensity, Point(light.Position.[0], light.Position.[1], light.Position.[2])) :> Light
                else
                    DirectionalLight(radiance, light.Intensity, Vector(light.Direction.[0], light.Direction.[1], light.Direction.[2])) :> Light)
        let scene =
            Scene(Array.toList objects, Array.toList lights, AmbientLight(ReferenceData.colour prepared.Ambient, 1.), settings.MaxBounces)
        let c = loaded.Spec.Camera
        let point (value: double array) = Point(value.[0], value.[1], value.[2])
        let camera =
            PinholeCamera(point c.Position, point c.Target, Vector(c.Up.[0], c.Up.[1], c.Up.[2]),
                          c.ViewDistance, c.ViewWidth, c.ViewHeight, settings.Width, settings.Height,
                          ReferenceData.sampler prepared prepared.CameraSampleOffset settings.CameraSamples prepared.CameraSampleSets)
        let options =
            { RenderOptions.Default with Threads = settings.Threads; TileSize = settings.TileSize
                                         Seed = settings.Seed; Transfer = settings.Transfer; Acceleration = Some Acceleration.FlatBVH }
        let renderer = Render(scene, camera, options)
        let cpu = renderer.RenderLinear
        let mutable squaredError = 0.
        let mutable maximumError = 0.
        let mutable compared = 0
        for index = 0 to gpu.Pixels.Length - 1 do
            let pixel = gpu.Pixels.[index]
            for channel = 0 to 2 do
                let value = double (if channel = 0 then pixel.X elif channel = 1 then pixel.Y else pixel.Z)
                let error = abs (value - cpu.Pixels.[index * 3 + channel])
                squaredError <- squaredError + error * error
                maximumError <- max maximumError error
                compared <- compared + 1
        let rmse = sqrt (squaredError / double compared)
        // FP32 device against the FP64 CPU renderer. Exact agreement is
        // impossible by construction, and the old gate (1e-6 max / 1e-8 RMSE)
        // only ever held because both sides were FP64.
        let edgeFlips =
            gpu.Pixels
            |> Array.mapi (fun index pixel ->
                let dx = abs (double pixel.X - cpu.Pixels.[index * 3])
                let dy = abs (double pixel.Y - cpu.Pixels.[index * 3 + 1])
                let dz = abs (double pixel.Z - cpu.Pixels.[index * 3 + 2])
                max dx (max dy dz))
            |> Array.filter (fun error -> error > EdgeFlipThreshold)
            |> Array.length
        let edgeFlipFraction = float edgeFlips / float (max 1 gpu.Pixels.Length)
        let passed = rmse <= MaximumRootMeanSquareError && edgeFlipFraction <= MaximumEdgeFlipFraction
        let differences =
            gpu.Pixels
            |> Array.mapi (fun index pixel ->
                let reference =
                    DeviceMath.v3 (float32 cpu.Pixels.[index * 3]) (float32 cpu.Pixels.[index * 3 + 1])
                                  (float32 cpu.Pixels.[index * 3 + 2])
                let error = max (abs (pixel.X - reference.X)) (max (abs (pixel.Y - reference.Y)) (abs (pixel.Z - reference.Z)))
                {| x = index % settings.Width; y = index / settings.Width; cuda = pixel; cpu = reference; maxError = error |})
        let largestDifferences = differences |> Array.sortByDescending (fun pixel -> pixel.maxError) |> Array.truncate 12
        let primaryAcceleration = renderer.PreProcessing
        let primaryDiagnostics =
            largestDifferences
            |> Array.filter (fun pixel -> pixel.maxError > float32 EdgeFlipThreshold)
            |> Array.map (fun pixel ->
                let y = settings.Height - 1 - pixel.y
                let key = Sampling.mixKey (uint64 (uint32 settings.Seed) ^^^ uint64 (y * settings.Width + pixel.x))
                let ray = camera.CreateRaysAt pixel.x y key |> Array.head
                let hit = Acceleration.traverseClosest primaryAcceleration ray 0. infinity
                let mutable material = ""
                let mutable texture = ""
                if hit.DidHit then
                    let local = (hit.Shape :?> ReferenceObject).TriangleHit ray
                    let data = (local.Shape :?> ReferenceTriangle).Data
                    material <- loaded.Spec.Materials.[data.Material].Id
                    texture <- loaded.Spec.Materials.[data.Material].Texture
                {| x = pixel.x; y = pixel.y; u = hit.U; v = hit.V; time = hit.Time
                   material = material; texture = texture |})
        let report =
            {| status = if passed then "passed" else "failed"
               generatedUtc = DateTimeOffset.UtcNow
               gpuAssemblySha256 = SceneFiles.hashFile (typeof<V3>.Assembly.Location)
               cpuCoreAssemblySha256 = SceneFiles.hashFile (typeof<Render>.Assembly.Location)
               sceneJsonSha256 = SceneFiles.hashFile scenePath
               settingsJsonSha256 = SceneFiles.hashFile settingsPath
               texturePolicy = "nearest/clamp[0,1], gamma2 decoding, authored tint, anchored affine UV interpolation"
               scene = loaded.Spec.Id; material = variant; settings = settings; device = gpu.Device
               comparedRgbComponents = compared; maxAbsoluteRgbError = maximumError; rootMeanSquareError = rmse
               edgeFlipPixels = edgeFlips; edgeFlipFraction = edgeFlipFraction
               pixelsAboveTolerance = differences |> Array.filter (fun pixel -> pixel.maxError > float32 EdgeFlipThreshold) |> Array.length
               largestDifferences = largestDifferences
               primaryDiagnostics = primaryDiagnostics
               cudaTraceMs = gpu.TraceMs; cpuTraceMs = cpu.TraceMilliseconds
               reference = "Current corrected CPU Render/ClassicIntegrator and CPU intersections/BVH over exported GPU-prepared world geometry. No CPU fallback; diagnostic only."
               limitation = "This isolates kernel transport/traversal and camera math. Independently constructed whole-worker adapters must also be compared by the common benchmark runner." |}
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath reportPath)) |> ignore
        let json = JsonSerializer.Serialize(report, SceneFiles.jsonOptions)
        File.WriteAllText(reportPath, json + Environment.NewLine)
        printfn "%s" json
        if not passed then invalidOp $"CUDA/CPU-core comparison exceeded FP64 accuracy limits (max={maximumError:G17}, RMSE={rmse:G17})."
