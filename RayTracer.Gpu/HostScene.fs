namespace Tracer.Gpu

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Tracer.Basics
open Tracer.Imaging
open Tracer.SceneFormat
open DeviceMath

type LoadedGpuScene =
    { Spec: SceneSpec
      Prepared: PreparedGpuScene
      LoadMs: double
      BuildMs: double
      SkippedDegenerateTriangles: int }

type private BoundsOnlyShape(bounds: BBox) =
    inherit Shape()
    override _.getBoundingBox() = bounds
    override _.isInside _ = false
    override _.hitFunction _ = invalidOp "A GPU preparation-only bounding shape cannot trace CPU rays."

module HostScene =
    /// Host scene data is FP64 - that is the CPU renderer's precision and the
    /// scene format's. The device is FP32. Narrowing happens HERE, once, at the
    /// upload boundary, so no device code ever sees a double and no host code
    /// has to think about precision.
    let inline private f32 (value: double) = float32 value
    let private v3f (x: double) (y: double) (z: double) = v3 (f32 x) (f32 y) (f32 z)

    let private vector (values: double array) = v3f values.[0] values.[1] values.[2]
    let private affine (matrix: double array) : DeviceTransform =
        { RowX = v3f matrix.[0] matrix.[1] matrix.[2]
          RowY = v3f matrix.[4] matrix.[5] matrix.[6]
          RowZ = v3f matrix.[8] matrix.[9] matrix.[10]
          Translation = v3f matrix.[3] matrix.[7] matrix.[11] }

    let private normalMatrix (matrix: double array) =
        let a, b, c = matrix.[0], matrix.[1], matrix.[2]
        let d, e, f = matrix.[4], matrix.[5], matrix.[6]
        let g, h, i = matrix.[8], matrix.[9], matrix.[10]
        let determinant = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
        [| (e * i - f * h) / determinant; (f * g - d * i) / determinant; (d * h - e * g) / determinant
           (c * h - b * i) / determinant; (a * i - c * g) / determinant; (b * g - a * h) / determinant
           (b * f - c * e) / determinant; (c * d - a * f) / determinant; (a * e - b * d) / determinant |]

    let private transformNormal (matrix: double array) (normal: V3) =
        // Widen to FP64 for the multiply-add, then narrow once. The inverse
        // transpose can be poorly conditioned for large or anisotropic scales,
        // and accumulating it at FP32 loses normals that FP64 keeps.
        let x, y, z = double normal.X, double normal.Y, double normal.Z
        v3f (matrix.[0] * x + matrix.[1] * y + matrix.[2] * z)
            (matrix.[3] * x + matrix.[4] * y + matrix.[5] * z)
            (matrix.[6] * x + matrix.[7] * y + matrix.[8] * z)

    let private inverseAffine (normalMatrix: double array) (translation: V3) =
        let rowX = v3f normalMatrix.[0] normalMatrix.[3] normalMatrix.[6]
        let rowY = v3f normalMatrix.[1] normalMatrix.[4] normalMatrix.[7]
        let rowZ = v3f normalMatrix.[2] normalMatrix.[5] normalMatrix.[8]
        { RowX = rowX; RowY = rowY; RowZ = rowZ
          // Already device-precision: rows and translation are both V3.
          Translation = v3 (-dot rowX translation) (-dot rowY translation) (-dot rowZ translation) }

    let private bounds (triangle: DeviceTriangle) (objectToWorld: DeviceTransform) =
        let a = transformPoint objectToWorld triangle.A
        let b = transformPoint objectToWorld triangle.B
        let c = transformPoint objectToWorld triangle.C
        // Widen to FP64 for the bounding box: the BVH is built and stored by the
        // shared CPU builder, which is FP64.
        let lowX = double (min a.X (min b.X c.X))
        let lowY = double (min a.Y (min b.Y c.Y))
        let lowZ = double (min a.Z (min b.Z c.Z))
        let highX = double (max a.X (max b.X c.X))
        let highY = double (max a.Y (max b.Y c.Y))
        let highZ = double (max a.Z (max b.Z c.Z))
        // The box must conservatively contain the triangle as the DEVICE sees
        // it, and the device works in FP32. A fixed 1e-6 pad is smaller than one
        // FP32 ULP once coordinates exceed ~8, so it would stop being
        // conservative on any large scene and drop hits along shared edges.
        // Scale the pad with the coordinate instead.
        let pad value = max 1e-6 (abs value * 4. * double DeviceMath.Epsilon)
        let lower value = let padded = value - pad value in if padded = value then Math.BitDecrement value else padded
        let upper value = let padded = value + pad value in if padded = value then Math.BitIncrement value else padded
        BBox(Point(lower lowX, lower lowY, lower lowZ), Point(upper highX, upper highY, upper highZ))

    let private buildBvh (triangles: DeviceTriangle array) (objectToWorld: DeviceTransform array) =
        if triangles.Length = 0 then [||], [||], 0
        else
            let shapes =
                triangles |> Array.map (fun triangle -> BoundsOnlyShape(bounds triangle objectToWorld.[triangle.Object]) :> Shape)
            let acceleration = Acceleration.buildWith Acceleration.FlatBVH shapes
            let snapshot =
                Acceleration.tryExportFlat acceleration
                |> Option.defaultWith (fun () -> invalidOp "The shared CPU flat BVH builder did not provide its serialized snapshot.")
            if not snapshot.IsFullyBounded then invalidOp "GPU triangle BVH unexpectedly contains unbounded primitives."
            let nodes =
                snapshot.Nodes.ToArray()
                |> Array.map (fun node ->
                    { Low = v3f node.MinX node.MinY node.MinZ; High = v3f node.MaxX node.MaxY node.MaxZ
                      Left = if node.Count = 0 then node.First else -1
                      Right = node.Right
                      Start = if node.Count > 0 then node.First else 0
                      Count = node.Count })
            nodes, snapshot.PrimitiveIndices.ToArray(), snapshot.MaxDepth

    let private createSampler (settings: RenderSettings) count =
        let side = int (Math.Sqrt(double count))
        if side <= 0 || int64 side * int64 side <> int64 count then
            invalidOp "The shared GPU samplers require square camera/light/glossy sample counts."
        match settings.Sampler with
        | "regular" -> Sampling.regular side
        | "multi-jittered" -> Sampling.multiJittered side 83
        | name -> invalidOp $"Unsupported GPU sampler {name}."

    let private appendSamples (samples: ResizeArray<V2>) (sampler: Sampling.Sampler) =
        let offset = samples.Count
        for setIndex = 0 to sampler.SetCount - 1 do
            for x, y in sampler.SampleSetAt (uint64 setIndex) do samples.Add(v2 (f32 x) (f32 y))
        offset, sampler.SetCount

    let private camera (camera: CameraSpec) (settings: RenderSettings) =
        let point (values: double array) = Point(values.[0], values.[1], values.[2])
        let up = camera.Up
        let shared =
            PinholeCamera(point camera.Position, point camera.Target, Vector(up.[0], up.[1], up.[2]),
                          camera.ViewDistance, camera.ViewWidth, camera.ViewHeight,
                          settings.Width, settings.Height, Sampling.regular 1)
        let vector (value: Vector) = v3f value.X value.Y value.Z
        { Position = v3f shared.Position.X shared.Position.Y shared.Position.Z
          W = vector shared.W; V = vector shared.V; U = vector shared.U
          ViewDistance = f32 shared.Zoom; PixelWidth = f32 shared.Pw; PixelHeight = f32 shared.Ph }

    let private createTriangle material objectId a b c =
        let normal = normalize (cross (sub b a) (sub c a))
        { A = a; B = b; C = c; GeometricNormal = normal
          NormalA = normal; NormalB = normal; NormalC = normal
          UvA = v2 0.f 0.f; UvB = v2 0.f 0.f; UvC = v2 0.f 0.f; Material = material; Object = objectId }

    let loadWith scenePath variant (settings: RenderSettings) (loadSpecification: unit -> SceneSpec) =
        let clock = Stopwatch.StartNew()
        SceneFiles.validateSettings settings |> ignore
        if settings.Precision <> "float32" then
            raise (NotSupportedException
                     "The CUDA renderer is FP32. Ada runs FP64 at 1/64 the FP32 rate, so an FP64 device path                       was costing roughly an order of magnitude for no visible benefit. Use \"precision\": \"float32\",                       or the CPU worker if you need FP64.")
        let source = loadSpecification ()
        if source.Id = "gold-dragon" && variant <> "authored" then
            invalidOp "gold-dragon is an extra authored/gold-only case; its open scan must not enter a material or glass sweep."
        let spec = SceneFiles.applyMaterialVariant variant source
        if spec.Camera.LensRadius <> 0. then
            raise (NotSupportedException "This GPU worker supports pinhole cameras only; use the CPU worker for thin-lens scenes.")
        Sampling.setRandomSeed settings.Seed
        // Match SceneBuilder's validation calls before constructing material, light, and camera samplers.
        for count in [ settings.CameraSamples; settings.LightSamples; settings.GlossySamples ] do
            createSampler settings count |> ignore
        let materialById = spec.Materials |> Array.mapi (fun index material -> material.Id, index) |> dict
        let meshById = spec.Meshes |> Array.map (fun mesh -> mesh.Id, mesh) |> dict
        let usedMaterials = HashSet<string>(spec.Objects |> Array.map (fun instance -> instance.Material))
        let usedMeshes = HashSet<string>(spec.Objects |> Array.map (fun instance -> instance.Mesh))
        for instance in spec.Objects do
            if spec.Materials.[materialById.[instance.Material]].Kind = MaterialKind.Glass && not meshById.[instance.Mesh].Closed then
                invalidOp $"Glass object {instance.Id} must reference a closed, consistently oriented mesh."
        let meshes = Dictionary<string, PLYParser.Vertex array * int array array>()
        for mesh in spec.Meshes |> Array.filter (fun mesh -> usedMeshes.Contains mesh.Id) do
            let path = SceneFiles.resolveAsset scenePath mesh.Path
            let vertices, faces = PLYParser.parseIndexedPLY path
            if faces |> Array.exists (fun face -> face.Length <> 3) then
                raise (NotSupportedException $"GPU mesh {mesh.Id} must be explicitly triangulated. The shared CPU reader supports additional polygon cases.")
            if mesh.Smooth && (vertices |> Array.exists (fun vertex -> vertex.normal = Vector.Zero)) then
                raise (NotSupportedException $"GPU smooth mesh {mesh.Id} needs authored vertex normals.")
            meshes.Add(mesh.Id, (vertices, faces))
        let textureData = Dictionary<string, int * int * byte array>()
        for material in spec.Materials do
            if usedMaterials.Contains material.Id && not (String.IsNullOrWhiteSpace material.Texture) then
                if material.Kind = MaterialKind.Glass then
                    raise (NotSupportedException $"GPU glass material {material.Id} requires a constant filter; image-varying absorption is a CPU feature.")
                if settings.Sampler = "multi-jittered" && material.Kind = MaterialKind.Glossy then
                    raise (NotSupportedException $"GPU multi-jittered material {material.Id} cannot use per-texel glossy samplers; use an untextured glossy material or the CPU worker.")
                let path = SceneFiles.resolveAsset scenePath material.Texture
                if not (textureData.ContainsKey path) then
                    use image = RgbImage.Load path
                    textureData.Add(path, (image.Width, image.Height, Array.copy image.Pixels))
        let loadMs = clock.Elapsed.TotalMilliseconds
        clock.Restart()
        let texturePixels = ResizeArray<V3>()
        let textureOffsets = Dictionary<string, int * int * int>()
        for KeyValue(path, (width, height, bytes)) in textureData do
            let offset = texturePixels.Count
            for index = 0 to width * height - 1 do
                let colour =
                    Colour(System.Drawing.Color.FromArgb(int bytes.[index * 3], int bytes.[index * 3 + 1], int bytes.[index * 3 + 2]))
                texturePixels.Add(v3f colour.R colour.G colour.B)
            textureOffsets.Add(path, (offset, width, height))
        let samples = ResizeArray<V2>()
        let glossySamplers = Dictionary<string, int * int>()
        for instance in spec.Objects do
            let material = spec.Materials.[materialById.[instance.Material]]
            if material.Kind = MaterialKind.Glossy && not (glossySamplers.ContainsKey material.Id) then
                let sampleData = appendSamples samples (createSampler settings settings.GlossySamples)
                glossySamplers.Add(material.Id, sampleData)
        let materials = ResizeArray<DeviceMaterial>()
        for material in spec.Materials do
            let textureOffset, textureWidth, textureHeight =
                if not (usedMaterials.Contains material.Id) || String.IsNullOrWhiteSpace material.Texture then 0, 0, 0
                else textureOffsets.[SceneFiles.resolveAsset scenePath material.Texture]
            let glossyOffset, glossySets =
                match glossySamplers.TryGetValue material.Id with
                | true, sampleData -> sampleData
                | false, _ -> 0, 1
            materials.Add
                { Colour = vector material.Colour; AmbientColour = vector (SceneFiles.ambientColour material)
                  SpecularColour = vector (SceneFiles.specularColour material)
                  ReflectionColour = vector (SceneFiles.reflectionColour material)
                  Filter = vector material.Filter; Ambient = f32 material.Ambient; Diffuse = f32 material.Diffuse
                  Specular = f32 material.Specular; Reflectivity = f32 material.Reflectivity
                  Ior = f32 material.Ior; Emission = f32 material.Emission
                  Kind = int material.Kind; Exponent = material.Exponent; GlossExponent = material.GlossExponent
                  GlossySampleOffset = glossyOffset; GlossySampleSets = glossySets
                  TextureOffset = textureOffset; TextureWidth = textureWidth; TextureHeight = textureHeight }
        let triangles = ResizeArray<DeviceTriangle>()
        let objectMaterials = ResizeArray<int>()
        let worldToObject = ResizeArray<DeviceTransform>()
        let objectToWorld = ResizeArray<DeviceTransform>()
        let mutable skippedDegenerateTriangles = 0
        for instance in spec.Objects do
            let materialIndex = materialById.[instance.Material]
            let mesh = meshById.[instance.Mesh]
            let vertices, faces = meshes.[mesh.Id]
            if materials.[materialIndex].TextureWidth > 0 &&
               (vertices |> Array.exists (fun vertex -> vertex.u.IsNone || vertex.v.IsNone)) then
                invalidOp $"Textured GPU mesh {mesh.Id} needs complete vertex UVs."
            let transform = affine instance.Transform
            if vertices |> Array.exists (fun vertex -> not (finite3 (transformPoint transform (v3f vertex.x vertex.y vertex.z)))) then
                invalidOp $"Object {instance.Id} has a non-finite transformed vertex."
            let normalTransform = normalMatrix instance.Transform
            let inverse = inverseAffine normalTransform transform.Translation
            if not (finite3 inverse.RowX && finite3 inverse.RowY && finite3 inverse.RowZ && finite3 inverse.Translation) then
                invalidOp $"Object {instance.Id} has a non-finite inverse transform."
            let transformedNormals =
                vertices |> Array.map (fun vertex ->
                    let normal = vertex.normal.Normalise
                    transformNormal normalTransform (v3f normal.X normal.Y normal.Z))
            let objectId = objectMaterials.Count
            objectMaterials.Add materialIndex
            worldToObject.Add inverse
            objectToWorld.Add transform
            for face in faces do
                let ia, ib, ic = face.[0], face.[1], face.[2]
                let va, vb, vc = vertices.[ia], vertices.[ib], vertices.[ic]
                let localGeometric =
                    normalize (cross (v3f (vb.x - va.x) (vb.y - va.y) (vb.z - va.z))
                                     (v3f (vc.x - va.x) (vc.y - va.y) (vc.z - va.z)))
                let geometric = normalize (transformNormal normalTransform localGeometric)
                if not (finite3 localGeometric && finite3 geometric) then
                    invalidOp $"Mesh {mesh.Id} contains a non-finite transformed triangle normal."
                if isBlack localGeometric then
                    // The shared MeshGeometry treats zero-area source triangles as nonintersecting.
                    skippedDegenerateTriangles <- skippedDegenerateTriangles + 1
                else
                    if isBlack geometric then invalidOp $"Object {instance.Id} collapses a nondegenerate triangle normal."
                    triangles.Add
                        { A = v3f va.x va.y va.z; B = v3f vb.x vb.y vb.z; C = v3f vc.x vc.y vc.z; GeometricNormal = geometric
                          NormalA = if mesh.Smooth then transformedNormals.[ia] else geometric
                          NormalB = if mesh.Smooth then transformedNormals.[ib] else geometric
                          NormalC = if mesh.Smooth then transformedNormals.[ic] else geometric
                          UvA = v2 (f32 (defaultArg va.u 0.)) (f32 (defaultArg va.v 0.))
                          UvB = v2 (f32 (defaultArg vb.u 0.)) (f32 (defaultArg vb.v 0.))
                          UvC = v2 (f32 (defaultArg vc.u 0.)) (f32 (defaultArg vc.v 0.))
                          Material = materialIndex; Object = objectId }
        let lights = ResizeArray<DeviceLight>()
        let mutable background = zero ()
        for light in spec.Lights do
            let radiance = scale (vector light.Colour) (f32 light.Intensity)
            let position = vector light.Position
            let direction =
                if light.Kind = LightKind.Directional || light.Kind = LightKind.Rectangle then normalize (vector light.Direction)
                else zero ()
            let mutable edgeU = zero ()
            let mutable edgeV = zero ()
            let mutable lowerLeft = position
            let mutable area = 0.f
            if light.Kind = LightKind.Rectangle then
                let helper = if abs direction.Y < 0.99f then v3 0.f 1.f 0.f else v3 1.f 0.f 0.f
                let u = normalize (cross helper direction)
                let v = cross direction u
                edgeU <- scale u (f32 light.Size.[0])
                edgeV <- scale v (f32 light.Size.[1])
                lowerLeft <- sub (sub position (scale u (f32 (light.Size.[0] * 0.5)))) (scale v (f32 (light.Size.[1] * 0.5)))
                area <- magnitude (cross edgeU edgeV)
                let materialId = materials.Count
                materials.Add
                    { Colour = radiance; AmbientColour = zero (); SpecularColour = zero (); ReflectionColour = zero ()
                      Filter = one (); Ambient = 0.f; Diffuse = 0.f; Specular = 0.f; Reflectivity = 0.f; Ior = 1.f; Emission = 1.f
                      Kind = 5; Exponent = 0; GlossExponent = 0; GlossySampleOffset = 0; GlossySampleSets = 1
                      TextureOffset = 0; TextureWidth = 0; TextureHeight = 0 }
                let objectId = objectMaterials.Count
                objectMaterials.Add materialId
                worldToObject.Add(identityTransform ())
                objectToWorld.Add(identityTransform ())
                let a, b, c, d = lowerLeft, add lowerLeft edgeU, add lowerLeft edgeV, add lowerLeft (add edgeU edgeV)
                triangles.Add(createTriangle materialId objectId a b c)
                triangles.Add(createTriangle materialId objectId b d c)
            elif light.Kind = LightKind.Environment then background <- add background radiance
            let count = if light.Kind = LightKind.Rectangle || light.Kind = LightKind.Environment then settings.LightSamples else 1
            let lightOffset, lightSets =
                if light.Kind = LightKind.Rectangle || light.Kind = LightKind.Environment then
                    appendSamples samples (createSampler settings count)
                else 0, 1
            lights.Add
                { Position = lowerLeft; Direction = direction; EdgeU = edgeU; EdgeV = edgeV
                  Radiance = radiance; Area = area; Kind = int light.Kind
                  SampleOffset = lightOffset; SampleCount = count; SampleSets = lightSets }
        let cameraOffset, cameraSets = appendSamples samples (createSampler settings settings.CameraSamples)
        let triangleArray = triangles.ToArray()
        let objectToWorldArray = objectToWorld.ToArray()
        let nodes, indices, depth = buildBvh triangleArray objectToWorldArray
        let prepared =
            { Triangles = triangleArray; Nodes = nodes; PrimitiveIndices = indices; Materials = materials.ToArray()
              ObjectMaterials = objectMaterials.ToArray(); Lights = lights.ToArray(); TexturePixels = texturePixels.ToArray()
              WorldToObject = worldToObject.ToArray(); ObjectToWorld = objectToWorldArray
              Samples = samples.ToArray(); Camera = camera spec.Camera settings
              Ambient = scale (vector spec.AmbientColour) (f32 spec.AmbientIntensity); Background = background
              CameraSampleOffset = cameraOffset; CameraSampleSets = cameraSets
              BvhDepth = depth }
        { Spec = spec; Prepared = prepared; LoadMs = loadMs; BuildMs = clock.Elapsed.TotalMilliseconds
          SkippedDegenerateTriangles = skippedDegenerateTriangles }

    let load scenePath variant settings =
        loadWith scenePath variant settings (fun () -> SceneFiles.load scenePath)
