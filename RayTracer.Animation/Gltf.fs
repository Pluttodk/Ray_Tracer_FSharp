namespace Tracer.Animation

open System
open System.Collections.Generic
open System.IO
open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Basics.Transformation
open Tracer.Imaging

type private V2 = System.Numerics.Vector2
type private V3 = System.Numerics.Vector3
type private V4 = System.Numerics.Vector4
type private NQ = System.Numerics.Quaternion

/// glTF 2.0 import and export. glTF is the interchange format: Blender and most DCC tools export node
/// hierarchies, TRS animation (step, linear and cubic spline), perspective cameras and KHR_lights_punctual.
/// Skins and morph targets are not supported and are reported as warnings.
module Gltf =
    type ImportOptions =
        { /// Plays only the named animation; by default every animation in the file plays at once.
          Clip: string option
          /// Multiplier from glTF point/spot intensity (candela) to this renderer's unattenuated point lights.
          PointLightScale: float
          /// Multiplier from glTF directional intensity (lux) to this renderer's directional lights.
          DirectionalLightScale: float
          SmoothShading: bool
          /// Edits each material's description, given the glTF material name, before it is built. Regions
          /// (see `MaterialRegion`) arrive as "material:region".
          MaterialOverride: string -> PbrParams -> PbrParams
          /// Splits skinned primitives into material regions: given the material name and the joint that
          /// most influences a triangle, the region the triangle belongs to, if any. Lets one glTF material
          /// be overridden differently on, say, the wings and the body.
          MaterialRegion: string -> string -> string option }
        static member Default =
            { Clip = None; PointLightScale = 0.02; DirectionalLightScale = 0.3; SmoothShading = true
              MaterialOverride = (fun _ p -> p); MaterialRegion = fun _ _ -> None }

    type ImportResult = { Scene: AnimatedScene; Warnings: string list }

    let private vector (v: V3) = Vector(float v.X, float v.Y, float v.Z)
    let private quaternion (q: NQ) = Quaternion.normalise { X = float q.X; Y = float q.Y; Z = float q.Z; W = float q.W }
    let private colour (v: V3) = Colour(float v.X, float v.Y, float v.Z)
    let private clamp01 (v: float) = if Double.IsFinite v then max 0. (min 1. v) else 0.

    /// True when a primitive has no usable texture coordinates: none at all, or every vertex at one point
    /// (common in palette-coloured low-poly models).
    let private degenerateUvs (uvs: (float * float)[]) = uvs.Length = 0 || uvs |> Array.forall (fun uv -> uv = uvs.[0])

    /// Texture coordinates for a primitive without usable ones, so procedural material maps have something
    /// to vary over. Each triangle is projected along the axis its rest-pose normal is closest to, taking
    /// the position relative to the mesh's bounding-box `centre` in units of its largest side `extent`.
    /// The three projections are laid out as separate charts 4 apart along u, so they never overlap.
    /// Every triangle needs vertices of its own: returns, per new vertex, its source vertex and its
    /// coordinates. The new triangles are 0, 1, 2, ...
    let private projectedUvs (positions: Point[]) (triangles: int[]) (struct (centre: Point, extent: float)) =
        let scale = if extent > 0. && Double.IsFinite extent then 1. / extent else 1.
        let uvs = Array.zeroCreate triangles.Length
        for t in 0 .. triangles.Length / 3 - 1 do
            let a, b, c = positions.[triangles.[3 * t]], positions.[triangles.[3 * t + 1]], positions.[triangles.[3 * t + 2]]
            let normal = (b - a) % (c - a)
            let nx, ny, nz = abs normal.X, abs normal.Y, abs normal.Z
            for k in 0 .. 2 do
                let p = positions.[triangles.[3 * t + k]] - centre
                uvs.[3 * t + k] <-
                    if nx >= ny && nx >= nz then p.Z * scale, p.Y * scale
                    elif ny >= nz then 4. + p.X * scale, p.Z * scale
                    else 8. + p.X * scale, p.Y * scale
        Array.copy triangles, uvs

    // ---------------------------------------------------------------- import

    type private Importer(model: SharpGLTF.Schema2.ModelRoot, options: ImportOptions) =
        let warnings = ResizeArray<string>()
        let warned = HashSet<string>()
        let warn (message: string) = if warned.Add message then warnings.Add message
        let names = Dictionary<int, string>()
        let used = HashSet<string>()
        let textures = Dictionary<struct (int * string), Texture * bool>()
        let meshes = Dictionary<int, Shape list>()

        let nameOf (node: SharpGLTF.Schema2.Node) =
            match names.TryGetValue node.LogicalIndex with
            | true, name -> name
            | _ ->
                let baseName = if String.IsNullOrWhiteSpace node.Name then $"node{node.LogicalIndex}" else node.Name
                let mutable name = baseName
                let mutable suffix = 1
                while not (used.Add name) do
                    name <- $"{baseName}.{suffix}"
                    suffix <- suffix + 1
                names.[node.LogicalIndex] <- name
                name

        /// Decoded images, keyed by (image index, sRGB, with alpha). Only the base colour of alpha-masked
        /// materials keeps an alpha channel, so other textures stay at three floats per texel.
        let images = System.Collections.Concurrent.ConcurrentDictionary<struct (int * bool * bool), FilterImage>()

        let decodeImage (image: SharpGLTF.Schema2.Image) (srgb: bool) (alpha: bool) =
            use stream = image.Content.Open()
            let lut = Array.init 256 (fun b -> let v = float b / 255. in float32 (if srgb then TextureFilter.srgbToLinear v else v))
            if alpha then
                let struct (rgb, coverage) = RgbImage.LoadWithAlpha stream
                use rgb = rgb
                if coverage.Length = 0 then TextureFilter.create rgb.Width rgb.Height 3 (rgb.Pixels |> Array.map (fun b -> lut.[int b]))
                else
                    let pixels = rgb.Pixels
                    let data = Array.zeroCreate<float32> (4 * coverage.Length)
                    for i in 0 .. coverage.Length - 1 do
                        data.[4 * i] <- lut.[int pixels.[3 * i]]
                        data.[4 * i + 1] <- lut.[int pixels.[3 * i + 1]]
                        data.[4 * i + 2] <- lut.[int pixels.[3 * i + 2]]
                        // Alpha is linear coverage, never sRGB-encoded.
                        data.[4 * i + 3] <- float32 coverage.[i] / 255.f
                    TextureFilter.create rgb.Width rgb.Height 4 data
            else
                use rgb = RgbImage.Load stream
                TextureFilter.create rgb.Width rgb.Height 3 (rgb.Pixels |> Array.map (fun b -> lut.[int b]))

        let decode (image: SharpGLTF.Schema2.Image) (srgb: bool) (alpha: bool) =
            images.GetOrAdd(struct (image.LogicalIndex, srgb, alpha), fun _ -> decodeImage image srgb alpha)

        /// glTF alphaMode as this renderer handles it: MASK cuts out at alphaCutoff; BLEND is a cut-out at
        /// 0.5 too, unless the material's base colour factor is itself translucent (a decal or glass-like
        /// film), which becomes stochastic coverage.
        let isMasked (material: SharpGLTF.Schema2.Material) =
            not (isNull material) && material.Alpha <> SharpGLTF.Schema2.AlphaMode.OPAQUE

        /// Decodes every texture the materials use, in parallel, before meshes are built.
        let prefetch () =
            let keys = HashSet<struct (int * bool * bool)>()
            for material in model.LogicalMaterials do
                for channel in material.Channels do
                    let texture = channel.Texture
                    if not (isNull texture) && not (isNull texture.PrimaryImage) then
                        match channel.Key with
                        | "BaseColor" -> keys.Add(struct (texture.PrimaryImage.LogicalIndex, true, isMasked material)) |> ignore
                        | "Emissive" -> keys.Add(struct (texture.PrimaryImage.LogicalIndex, true, false)) |> ignore
                        | "MetallicRoughness" | "Normal" | "Occlusion" -> keys.Add(struct (texture.PrimaryImage.LogicalIndex, false, false)) |> ignore
                        | _ -> ()
            let work = Array.ofSeq keys
            let parallelism = max 1 (min 16 (Environment.ProcessorCount / 2))
            System.Threading.Tasks.Parallel.ForEach(work, System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = parallelism),
                fun (struct (image, srgb, alpha)) -> decode model.LogicalImages.[image] srgb alpha |> ignore) |> ignore

        let wrapMode (mode: SharpGLTF.Schema2.TextureWrapMode) =
            match mode with
            | SharpGLTF.Schema2.TextureWrapMode.CLAMP_TO_EDGE -> ClampToEdge
            | SharpGLTF.Schema2.TextureWrapMode.MIRRORED_REPEAT -> MirroredRepeat
            | _ -> Repeat

        /// A filtered lookup for a material channel's texture, over mesh texture coordinates (u, v up).
        /// Applies KHR_texture_transform and the sampler's wrap modes, filtering bilinearly unless the
        /// sampler asks for nearest magnification.
        let channelSamplerWith (channel: SharpGLTF.Schema2.MaterialChannel) (srgb: bool) (alpha: bool) =
            match channel.Texture with
            | null -> None
            | texture when isNull texture.PrimaryImage -> None
            | texture ->
                let image = decode texture.PrimaryImage srgb alpha
                let transform = channel.TextureTransform
                let coordinateSet =
                    if isNull transform || not transform.TextureCoordinateOverride.HasValue then channel.TextureCoordinate
                    else transform.TextureCoordinateOverride.Value
                if coordinateSet <> 0 then warn "Only TEXCOORD_0 is supported; other texture coordinate sets are ignored."
                let sampler = channel.TextureSampler
                let wrapS, wrapT, nearest =
                    if isNull sampler then Repeat, Repeat, false
                    else wrapMode sampler.WrapS, wrapMode sampler.WrapT, sampler.MagFilter = SharpGLTF.Schema2.TextureInterpolationFilter.NEAREST
                let lookup = if nearest then TextureFilter.nearest image wrapS wrapT else TextureFilter.bilinear image wrapS wrapT
                // Meshes store v flipped (v up); glTF texture space has t = 0 at the top of the image.
                if isNull transform then Some (fun (u: float) (v: float) -> lookup u (1. - v))
                else
                    // KHR_texture_transform: uv' = T * R * S * uv in glTF texture space.
                    let ox, oy = float transform.Offset.X, float transform.Offset.Y
                    let sx, sy = float transform.Scale.X, float transform.Scale.Y
                    let c, s = cos (float transform.Rotation), sin (float transform.Rotation)
                    Some (fun (u: float) (v: float) ->
                        let t = 1. - v
                        lookup (c * sx * u + s * sy * t + ox) (-s * sx * u + c * sy * t + oy))

        let channelSampler channel srgb = channelSamplerWith channel srgb false

        let colourMap (channel: SharpGLTF.Schema2.MaterialChannel) srgb =
            channelSampler channel srgb
            |> Option.map (fun lookup -> fun u v -> let struct (r, g, b, _) = lookup u v in Colour(max 0. r, max 0. g, max 0. b))

        let materialName (material: SharpGLTF.Schema2.Material) =
            if String.IsNullOrWhiteSpace material.Name then $"material{material.LogicalIndex}" else material.Name

        /// The material description of a glTF material, before any override.
        let describeMaterial (material: SharpGLTF.Schema2.Material) =
            let channel key = material.FindChannel key |> Option.ofNullable
            let factor (c: SharpGLTF.Schema2.MaterialChannel) (name: string) fallback =
                try float (c.GetFactor name) with _ -> fallback
            let rgb (c: SharpGLTF.Schema2.MaterialChannel) = Colour(max 0. (float c.Color.X), max 0. (float c.Color.Y), max 0. (float c.Color.Z))
            let mutable p = PbrParams.defaults
            match channel "BaseColor" with
            | Some c ->
                // Masked materials share one RGBA decode between colour and coverage.
                let map =
                    channelSamplerWith c true (isMasked material)
                    |> Option.map (fun lookup -> fun u v -> let struct (r, g, b, _) = lookup u v in Colour(max 0. r, max 0. g, max 0. b))
                p <- { p with BaseColour = rgb c; BaseColourMap = map }
            | None -> p <- { p with BaseColour = Colour.White }
            match channel "MetallicRoughness" with
            | Some c ->
                let map =
                    channelSampler c false
                    |> Option.map (fun lookup -> fun u v -> let struct (_, g, b, _) = lookup u v in struct (b, g))
                p <- { p with Metallic = factor c "MetallicFactor" 1.; Roughness = factor c "RoughnessFactor" 1.; MetallicRoughnessMap = map }
            | None -> p <- { p with Metallic = 1.; Roughness = 1. }
            match channel "Normal" with
            | Some c ->
                let map =
                    channelSampler c false
                    |> Option.map (fun lookup -> fun u v ->
                        let struct (r, g, b, _) = lookup u v
                        Vector(2. * r - 1., 2. * g - 1., 2. * b - 1.))
                p <- { p with NormalMap = map; NormalScale = factor c "NormalScale" 1. }
            | None -> ()
            match channel "Occlusion" with
            | Some c ->
                let map = channelSampler c false |> Option.map (fun lookup -> fun u v -> let struct (r, _, _, _) = lookup u v in r)
                p <- { p with OcclusionMap = map; OcclusionStrength = factor c "OcclusionStrength" 1. }
            | None -> ()
            match channel "Emissive" with
            | Some c -> p <- { p with Emissive = rgb c * factor c "EmissiveStrength" 1.; EmissiveMap = colourMap c true }
            | None -> ()
            match channel "Transmission" with
            | Some c ->
                if not (isNull c.Texture) then warn "Transmission textures are ignored; the factor is used."
                p <- { p with Transmission = factor c "TransmissionFactor" 0. }
            | None -> ()
            match channel "DiffuseTransmissionFactor" with
            | Some c ->
                if not (isNull c.Texture) then warn "Diffuse transmission textures are ignored; the factor is used."
                p <- { p with DiffuseTransmission = factor c "DiffuseTransmissionFactor" 0. }
            | None -> ()
            match channel "DiffuseTransmissionColor" with
            | Some c -> p <- { p with DiffuseTransmissionColour = rgb c }
            | None -> ()
            match channel "SheenColor" with
            | Some c ->
                let colour = rgb c
                let peak = max colour.R (max colour.G colour.B)
                if peak > 0. then p <- { p with Sheen = min 1. peak; SheenColour = colour / peak }
            | None -> ()
            let ior = let v = float material.IndexOfRefraction in if Double.IsFinite v && v > 0. then v else 1.5
            { p with Ior = ior }

        /// A material description as a texture. Uniform glTF emitters and clear glass keep their dedicated
        /// materials, which both integrators treat as lights and refracting media; everything else is a
        /// `PbrMaterial`.
        let pbrTexture (p: PbrParams) =
            let emitter = p.Emissive.R + p.Emissive.G + p.Emissive.B > 0.
            if PbrParams.isUniform p && emitter then
                let peak = max p.Emissive.R (max p.Emissive.G p.Emissive.B)
                mkMatTexture (EmissiveMaterial(p.Emissive / peak, peak))
            elif PbrParams.isUniform p && p.Transmission > 0.5 && p.Metallic < 0.5 then
                let c = (PbrParams.sampleAt p 0. 0.).BaseColour
                mkMatTexture (TransparentMaterial(c, Colour.White, p.Ior, 1.))
            elif PbrParams.isUniform p then mkMatTexture (PbrMaterial(PbrParams.sampleAt p 0. 0.))
            else mkTexture (fun u v -> PbrMaterial(PbrParams.sampleAt p u v) :> Tracer.Basics.Material) |> markOpaque

        /// The material's texture, and whether it varies over the surface.
        let convertMaterial (material: SharpGLTF.Schema2.Material) (region: string option) =
            if isNull material then mkMatTexture (PbrMaterial({ PbrSample.defaults with Roughness = 0.8 })), false
            else
                let key = struct (material.LogicalIndex, defaultArg region "")
                match textures.TryGetValue key with
                | true, texture -> texture
                | _ ->
                    let name = match region with Some r -> $"{materialName material}:{r}" | None -> materialName material
                    let p = options.MaterialOverride name (describeMaterial material)
                    let texture = pbrTexture p, not (PbrParams.isUniform p)
                    textures.[key] <- texture
                    texture

        let masks = Dictionary<int, TriangleMesh.AlphaMask>()

        /// The coverage test of a MASK or BLEND material (null for opaque ones): base colour alpha times
        /// the base colour factor's alpha.
        let alphaMaskOf (material: SharpGLTF.Schema2.Material) =
            if not (isMasked material) then null
            else
                match masks.TryGetValue material.LogicalIndex with
                | true, mask -> mask
                | _ ->
                    let channel = material.FindChannel "BaseColor" |> Option.ofNullable
                    let factor = match channel with Some c -> clamp01 (float c.Color.W) | None -> 1.
                    let lookup = channel |> Option.bind (fun c -> channelSamplerWith c true true)
                    let coverage =
                        match lookup with
                        | Some lookup -> fun u v -> let struct (_, _, _, a) = lookup u v in factor * a
                        | None -> fun _ _ -> factor
                    let mask =
                        if material.Alpha = SharpGLTF.Schema2.AlphaMode.MASK then
                            let cutoff = let c = float material.AlphaCutoff in if Double.IsFinite c then c else 0.5
                            if lookup.IsNone && factor >= cutoff then null
                            else TriangleMesh.AlphaMask.Cutout(coverage, cutoff)
                        elif factor < 1. then
                            warn $"BLEND material {materialName material} (alpha {factor:g3}) renders with stochastic coverage."
                            TriangleMesh.AlphaMask.Stochastic coverage
                        elif lookup.IsNone then null
                        else
                            warn $"BLEND material {materialName material} renders as a cut-out at alpha 0.5."
                            TriangleMesh.AlphaMask.Cutout(coverage, 0.5)
                    masks.[material.LogicalIndex] <- mask
                    mask

        /// Whether a material's (overridden) description has a normal map, so its meshes need tangents.
        let hasNormalMap (material: SharpGLTF.Schema2.Material) =
            not (isNull material)
            && (material.FindChannel "Normal" |> Option.ofNullable |> Option.exists (fun c -> not (isNull c.Texture)))

        /// Bounding-box centre and largest side over all of a mesh's primitives: the origin and unit of
        /// generated texture coordinates.
        let meshExtent (mesh: SharpGLTF.Schema2.Mesh) =
            let mutable low = V3(Single.PositiveInfinity)
            let mutable high = V3(Single.NegativeInfinity)
            for primitive in mesh.Primitives do
                match primitive.GetVertexAccessor "POSITION" with
                | null -> ()
                | accessor ->
                    for p in accessor.AsVector3Array() do
                        low <- V3.Min(low, p)
                        high <- V3.Max(high, p)
            let size = high - low
            let centre = (low + high) * 0.5f
            struct (Point(float centre.X, float centre.Y, float centre.Z), float (max size.X (max size.Y size.Z)))

        let convertMesh (mesh: SharpGLTF.Schema2.Mesh) =
            match meshes.TryGetValue mesh.LogicalIndex with
            | true, shapes -> shapes
            | _ ->
                let shapes =
                    [ for primitive in mesh.Primitives do
                        match primitive.DrawPrimitiveType with
                        | SharpGLTF.Schema2.PrimitiveType.TRIANGLES | SharpGLTF.Schema2.PrimitiveType.TRIANGLE_STRIP | SharpGLTF.Schema2.PrimitiveType.TRIANGLE_FAN ->
                            if primitive.MorphTargetsCount > 0 then warn "Morph targets are ignored; meshes use their base shape."
                            let positions = primitive.GetVertexAccessor("POSITION").AsVector3Array() |> Seq.map (fun p -> Point(float p.X, float p.Y, float p.Z)) |> Array.ofSeq
                            let normals =
                                match primitive.GetVertexAccessor "NORMAL" with
                                | null -> [||]
                                | accessor -> accessor.AsVector3Array() |> Seq.map vector |> Array.ofSeq
                            let uvs =
                                match primitive.GetVertexAccessor "TEXCOORD_0" with
                                | null -> [||]
                                | accessor -> accessor.AsVector2Array() |> Seq.map (fun (uv: V2) -> float uv.X, 1. - float uv.Y) |> Array.ofSeq
                            let triangles = primitive.GetTriangleIndices() |> Seq.collect (fun struct (a, b, c) -> [ a; b; c ]) |> Array.ofSeq
                            if triangles.Length > 0 then
                                let texture, varies = convertMaterial primitive.Material None
                                let projected = varies && degenerateUvs uvs
                                let positions, normals, uvs, triangles =
                                    if projected then
                                        let source, generated = projectedUvs positions triangles (meshExtent mesh)
                                        let pick (values: _[]) = if values.Length = 0 then values else source |> Array.map (fun i -> values.[i])
                                        pick positions, pick normals, generated, Array.init source.Length id
                                    else positions, normals, uvs, triangles
                                let smooth = options.SmoothShading && normals.Length > 0
                                // Normal-mapped surfaces carry a tangent frame: the file's own (glTF TANGENT, whose
                                // bitangent w * (n x t) already points along v up), else one generated from the UVs.
                                let tangents =
                                    if not (hasNormalMap primitive.Material) || uvs.Length = 0 then TriangleMesh.NoTangents
                                    else
                                        match primitive.GetVertexAccessor "TANGENT" with
                                        | accessor when not (isNull accessor) && not projected && accessor.Count = positions.Length ->
                                            accessor.AsVector4Array()
                                            |> Seq.map (fun (t: V4) -> { TriangleMesh.Tx = t.X; TriangleMesh.Ty = t.Y; TriangleMesh.Tz = t.Z; TriangleMesh.W = (if t.W < 0.f then -1.f else 1.f) })
                                            |> Array.ofSeq |> TriangleMesh.GivenTangents
                                        | _ -> TriangleMesh.GenerateTangents
                                let mask = alphaMaskOf primitive.Material
                                let baseShape = TriangleMesh.fromArraysWith tangents mask positions normals uvs triangles smooth
                                yield baseShape.toShape texture
                        | other -> warn $"Primitives of type {other} are skipped; only triangles render." ]
                meshes.[mesh.LogicalIndex] <- shapes
                shapes

        /// System.Numerics matrices use row vectors (translation in M41..M43); ours use column vectors.
        let matrixOf (m: System.Numerics.Matrix4x4) =
            { identityMatrix with
                Pos1x1 = float m.M11; Pos1x2 = float m.M21; Pos1x3 = float m.M31; Pos1x4 = float m.M41
                Pos2x1 = float m.M12; Pos2x2 = float m.M22; Pos2x3 = float m.M32; Pos2x4 = float m.M42
                Pos3x1 = float m.M13; Pos3x2 = float m.M23; Pos3x3 = float m.M33; Pos3x4 = float m.M43 }

        let convertSkinned (mesh: SharpGLTF.Schema2.Mesh) (skin: SharpGLTF.Schema2.Skin) =
            let joints = Array.init skin.JointsCount (fun i -> skin.GetJoint i)
            let jointNames = joints |> Array.map (fun struct (node, _) -> nameOf node)
            let inverseBind = joints |> Array.map (fun struct (_, m) -> matrixOf m)
            [ for primitive in mesh.Primitives do
                match primitive.DrawPrimitiveType with
                | SharpGLTF.Schema2.PrimitiveType.TRIANGLES | SharpGLTF.Schema2.PrimitiveType.TRIANGLE_STRIP | SharpGLTF.Schema2.PrimitiveType.TRIANGLE_FAN ->
                    if primitive.MorphTargetsCount > 0 then warn "Morph targets are ignored; meshes use their base shape."
                    let vec4 name =
                        match primitive.GetVertexAccessor name with
                        | null -> None
                        | accessor -> Some (accessor.AsVector4Array() |> Seq.collect (fun (v: V4) -> [ float v.X; float v.Y; float v.Z; float v.W ]) |> Array.ofSeq)
                    match vec4 "JOINTS_0", vec4 "WEIGHTS_0" with
                    | Some jointSlots, Some weights ->
                        if not (isNull (primitive.GetVertexAccessor "JOINTS_1")) then warn "Only four joint influences per vertex are used."
                        let positions = primitive.GetVertexAccessor("POSITION").AsVector3Array() |> Seq.map (fun p -> Point(float p.X, float p.Y, float p.Z)) |> Array.ofSeq
                        let normals =
                            match primitive.GetVertexAccessor "NORMAL" with
                            | null -> [||]
                            | accessor -> accessor.AsVector3Array() |> Seq.map vector |> Array.ofSeq
                        let uvs =
                            match primitive.GetVertexAccessor "TEXCOORD_0" with
                            | null -> [||]
                            | accessor -> accessor.AsVector2Array() |> Seq.map (fun (uv: V2) -> float uv.X, 1. - float uv.Y) |> Array.ofSeq
                        let triangles = primitive.GetTriangleIndices() |> Seq.collect (fun struct (a, b, c) -> [ a; b; c ]) |> Array.ofSeq
                        // Group triangles by material region, each by the joint with the largest summed weight
                        // over its corners.
                        let regionOf =
                            if isNull primitive.Material then fun _ -> None
                            else
                                let name = materialName primitive.Material
                                fun (t: int) ->
                                    let influence = Dictionary<int, float>()
                                    for k in 0 .. 2 do
                                        let v = triangles.[3 * t + k]
                                        for slot in 0 .. 3 do
                                            let joint = int jointSlots.[4 * v + slot]
                                            let w = weights.[4 * v + slot]
                                            influence.[joint] <- (match influence.TryGetValue joint with | true, x -> x | _ -> 0.) + w
                                    let joint = influence |> Seq.maxBy (fun kv -> kv.Value) |> fun kv -> kv.Key
                                    if joint >= 0 && joint < jointNames.Length then options.MaterialRegion name jointNames.[joint] else None
                        let groups = Array.init (triangles.Length / 3) id |> Array.groupBy regionOf
                        for region, group in groups do
                            // Keep only the vertices this group uses.
                            let used = Dictionary<int, int>()
                            let source = ResizeArray<int>()
                            let groupTriangles =
                                group |> Array.collect (fun t ->
                                    Array.init 3 (fun k ->
                                        let v = triangles.[3 * t + k]
                                        match used.TryGetValue v with
                                        | true, i -> i
                                        | _ ->
                                            used.[v] <- source.Count
                                            source.Add v
                                            source.Count - 1))
                            let source = source.ToArray()
                            let pick (values: _[]) = if values.Length = 0 then values else source |> Array.map (fun i -> values.[i])
                            let pick4 (values: float[]) = source |> Array.collect (fun i -> values.[4 * i .. 4 * i + 3])
                            let positions, normals, uvs, triangles, jointSlots, weights =
                                if groups.Length = 1 then positions, normals, uvs, triangles, jointSlots, weights
                                else pick positions, pick normals, pick uvs, groupTriangles, pick4 jointSlots, pick4 weights
                            let texture, varies = convertMaterial primitive.Material region
                            let positions, normals, uvs, triangles, jointSlots, weights =
                                if varies && degenerateUvs uvs then
                                    let source, generated = projectedUvs positions triangles (meshExtent mesh)
                                    let pick (values: _[]) = if values.Length = 0 then values else source |> Array.map (fun i -> values.[i])
                                    let pick4 (values: float[]) = source |> Array.collect (fun i -> values.[4 * i .. 4 * i + 3])
                                    pick positions, pick normals, generated, Array.init source.Length id, pick4 jointSlots, pick4 weights
                                else positions, normals, uvs, triangles, jointSlots, weights
                            yield Skinned
                                { Positions = positions; Normals = (if options.SmoothShading then normals else [||]); Uvs = uvs
                                  Triangles = triangles; Joints = jointSlots |> Array.map int; Weights = weights
                                  JointNodes = jointNames; InverseBind = inverseBind; Texture = texture }
                    | _ -> warn "A skinned primitive lacks JOINTS_0/WEIGHTS_0 and is skipped."
                | other -> warn $"Primitives of type {other} are skipped; only triangles render." ]

        let convertLight (light: SharpGLTF.Schema2.PunctualLight) =
            let c = colour light.Color
            match light.LightType with
            | SharpGLTF.Schema2.PunctualLightType.Directional ->
                // A glTF directional light shines along its node's -Z, so the direction towards it is +Z.
                DirectionalLight(c, float light.Intensity * options.DirectionalLightScale, Vector(0., 0., 1.)) :> Light
            | SharpGLTF.Schema2.PunctualLightType.Spot ->
                warn "Spot lights are approximated by point lights (no cone)."
                PointLight(c, float light.Intensity * options.PointLightScale, Point.Zero) :> Light
            | _ -> PointLight(c, float light.Intensity * options.PointLightScale, Point.Zero) :> Light

        let rec convertNode (node: SharpGLTF.Schema2.Node) : Node =
            let local = node.LocalTransform
            let rest = { Translation = vector local.Translation; Rotation = quaternion local.Rotation; Scale = vector local.Scale }
            let content =
                [ if not (isNull node.Mesh) then
                      if isNull node.Skin then
                          for shape in convertMesh node.Mesh do yield Geometry shape
                      else yield! convertSkinned node.Mesh node.Skin
                  if not (isNull node.Camera) then
                      match node.Camera.Settings with
                      | :? SharpGLTF.Schema2.CameraPerspective as perspective ->
                          yield CameraRig { CameraSpec.Default with YFov = float perspective.VerticalFOV }
                      | _ -> warn "Orthographic cameras are not supported and are skipped."
                  if not (isNull node.PunctualLight) then yield LightSource (convertLight node.PunctualLight) ]
            { Name = nameOf node; Rest = rest; Content = content; Children = node.VisualChildren |> Seq.map convertNode |> List.ofSeq }

        let convertSampler (mode: SharpGLTF.Schema2.AnimationInterpolationMode) (linear: unit -> seq<struct (float32 * 'G)>) (cubic: unit -> seq<struct (float32 * struct ('G * 'G * 'G))>) (map: 'G -> 'T) : Sampler<'T> =
            match mode with
            | SharpGLTF.Schema2.AnimationInterpolationMode.CUBICSPLINE ->
                Sampler.cubic [ for struct (t, struct (a, v, b)) in cubic () -> float t, map a, map v, map b ]
            | SharpGLTF.Schema2.AnimationInterpolationMode.STEP -> Sampler.step [ for struct (t, v) in linear () -> float t, map v ]
            | _ -> Sampler.linear [ for struct (t, v) in linear () -> float t, map v ]

        let convertAnimation (animation: SharpGLTF.Schema2.Animation) =
            let channels =
                [ for channel in animation.Channels do
                    let node = nameOf channel.TargetNode
                    match channel.TargetNodePath with
                    | SharpGLTF.Schema2.PropertyPath.translation ->
                        let s = channel.GetTranslationSampler()
                        yield Clip.translate node (convertSampler s.InterpolationMode s.GetLinearKeys s.GetCubicKeys vector)
                    | SharpGLTF.Schema2.PropertyPath.rotation ->
                        let s = channel.GetRotationSampler()
                        yield Clip.rotate node (convertSampler s.InterpolationMode s.GetLinearKeys s.GetCubicKeys (fun (q: NQ) -> { X = float q.X; Y = float q.Y; Z = float q.Z; W = float q.W }))
                    | SharpGLTF.Schema2.PropertyPath.scale ->
                        let s = channel.GetScaleSampler()
                        yield Clip.scale node (convertSampler s.InterpolationMode s.GetLinearKeys s.GetCubicKeys vector)
                    | other -> warn $"Animation of {other} is not supported and is skipped." ]
            Clip.create (if String.IsNullOrWhiteSpace animation.Name then $"animation{animation.LogicalIndex}" else animation.Name) channels

        member _.Import(sceneName: string) =
            prefetch ()
            let scene = if isNull model.DefaultScene then model.LogicalScenes.[0] else model.DefaultScene
            let roots = scene.VisualChildren |> Seq.map convertNode |> List.ofSeq
            let clips =
                let all = model.LogicalAnimations |> Seq.map convertAnimation |> List.ofSeq
                match options.Clip with
                | None -> all
                | Some name ->
                    match all |> List.filter (fun clip -> clip.Name = name) with
                    | [] -> invalidArg (nameof options) $"""The file has no animation named {name}; it has: {String.Join(", ", all |> List.map (fun c -> c.Name))}."""
                    | chosen -> chosen
            let rec allNodes (nodes: Node list) = nodes |> Seq.collect Node.descendants
            let cameras = allNodes roots |> Seq.filter (fun n -> n.Content |> List.exists (function CameraRig _ -> true | _ -> false)) |> List.ofSeq
            let hasLights = allNodes roots |> Seq.exists (fun n -> n.Content |> List.exists (function LightSource _ -> true | _ -> false))
            let roots, activeCamera =
                match cameras with
                | camera :: rest ->
                    if not rest.IsEmpty then warn $"The file has several cameras; using {camera.Name}."
                    roots, camera.Name
                | [] ->
                    warn "The file has no camera; using a default one at (0, 1.5, 6) looking down -Z."
                    let name = if used.Contains "camera" then "camera.default" else "camera"
                    roots @ [ Node.create name |> Node.at 0. 1.5 6. |> Node.withContent [ CameraRig CameraSpec.Default ] ], name
            let lights =
                if hasLights then []
                else
                    warn "The file has no KHR_lights_punctual lights; adding a default sun and sky."
                    [ Stage.sun (Vector(0.4, 1., 0.6)) 0.85; Stage.sky (Colour(0.85, 0.9, 1.)) (Colour(0.3, 0.5, 0.9)) 0.35 2 ]
            let duration = clips |> List.map (fun clip -> clip.Duration) |> List.fold max 0.
            let scene =
                { Name = sceneName; Roots = roots; Clips = clips; ActiveCamera = activeCamera; Cuts = []; StaticShapes = []
                  StaticLights = lights; Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 4; Atmosphere = None
                  Duration = if duration > 0. then duration else 1. }
            { Scene = AnimatedScene.validate scene; Warnings = List.ofSeq warnings }

    /// Loads a .gltf or .glb file as an animated scene.
    let load (path: string) (options: ImportOptions) =
        let model = SharpGLTF.Schema2.ModelRoot.Load path
        Importer(model, options).Import(Path.GetFileNameWithoutExtension path)

    // ---------------------------------------------------------------- export

    let private v3 (v: Vector) = V3(float32 v.X, float32 v.Y, float32 v.Z)
    let private nq (q: Quaternion) = NQ(float32 q.X, float32 q.Y, float32 q.Z, float32 q.W)

    type private Surface = { Base: Colour; Emission: Colour; Metallic: float; Roughness: float; Transmission: float }

    let private surfaceOf (material: Tracer.Basics.Material) =
        let plain c = { Base = c; Emission = Colour.Black; Metallic = 0.; Roughness = 0.8; Transmission = 0. }
        let roughnessOf exponent = PathTracing.MaterialAdapter.roughnessOfExponent exponent
        match material with
        | :? EmissiveMaterial as m -> { plain Colour.Black with Emission = m.LightColour * m.LightIntensity }
        | :? PbrMaterial as m ->
            let p = m.Sample
            { Base = p.BaseColour; Emission = p.Emissive; Metallic = p.Metallic; Roughness = p.Roughness; Transmission = p.Transmission }
        | :? TransparentMaterial as m -> { plain m.InnerFilterColour with Roughness = 0.; Transmission = 1. }
        | :? PhongReflectiveMaterial as m ->
            { plain m.MatteColour with Metallic = (if m.ReflectionCoefficient >= 0.5 then 1. else 0.); Roughness = roughnessOf m.SpecularExponent }
        | :? PhongMaterial as m -> { plain m.MatteColour with Roughness = roughnessOf m.SpecularExponent }
        | :? MatteMaterial as m -> plain m.MatteColour
        | _ -> plain (Colour(0.8, 0.8, 0.8))

    type private Exporter(model: SharpGLTF.Schema2.ModelRoot, scene: AnimatedScene, resampleRate: float) =
        let warnings = ResizeArray<string>()
        let warned = HashSet<string>()
        let warn (message: string) = if warned.Add message then warnings.Add message
        let nodes = Dictionary<string, SharpGLTF.Schema2.Node>()
        let materials = Dictionary<obj, SharpGLTF.Schema2.Material>(HashIdentity.Reference)

        /// Bakes a texture to a PNG over the UV square [0, uScale) x [0, vScale) and returns a glTF material.
        let material (texture: Texture) (uScale: float) (vScale: float) =
            match materials.TryGetValue (box texture) with
            | true, m -> m
            | _ ->
                let m = model.CreateMaterial($"material{materials.Count}")
                m.InitializePBRMetallicRoughness([||])
                let channel (key: string) = (m.FindChannel key).Value
                let apply (s: Surface) (baseColour: V4) =
                    let mutable baseChannel = channel "BaseColor"
                    baseChannel.Color <- baseColour
                    let metallicRoughness = channel "MetallicRoughness"
                    metallicRoughness.SetFactor("MetallicFactor", float32 s.Metallic)
                    metallicRoughness.SetFactor("RoughnessFactor", float32 s.Roughness)
                    if s.Emission.R + s.Emission.G + s.Emission.B > 0. then
                        let peak = max s.Emission.R (max s.Emission.G s.Emission.B)
                        if peak > 1. then warn "Emission brighter than 1 is normalised; glTF emissive factors are limited to [0, 1]."
                        let scale = if peak > 1. then 1. / peak else 1.
                        let mutable emissive = channel "Emissive"
                        emissive.Color <- V4(float32 (s.Emission.R * scale), float32 (s.Emission.G * scale), float32 (s.Emission.B * scale), 1.f)
                    if s.Transmission > 0. then warn "Transparent materials export as opaque base colour."
                match tryGetMaterial texture with
                | Some constant ->
                    let s = surfaceOf constant
                    apply s (V4(float32 (clamp01 s.Base.R), float32 (clamp01 s.Base.G), float32 (clamp01 s.Base.B), 1.f))
                | None ->
                    let w, h = 256, 128
                    let pixels = Array.zeroCreate<byte> (w * h * 3)
                    let mutable first = None
                    for y in 0 .. h - 1 do
                        for x in 0 .. w - 1 do
                            let u = (float x + 0.5) / float w * uScale
                            let v = (1. - (float y + 0.5) / float h) * vScale
                            let s = surfaceOf (getFunc texture u v)
                            if first.IsNone then first <- Some s
                            let encode (c: float) = byte (Math.Round(255. * Math.Pow(clamp01 c, 1. / 2.2)))
                            let i = (y * w + x) * 3
                            pixels.[i] <- encode s.Base.R
                            pixels.[i + 1] <- encode s.Base.G
                            pixels.[i + 2] <- encode s.Base.B
                    use image = RgbImage.FromPixels(w, h, pixels)
                    use stream = new MemoryStream()
                    image.SavePng(stream)
                    let memoryImage = SharpGLTF.Memory.MemoryImage(stream.ToArray())
                    let s = first.Value
                    apply s V4.One
                    (channel "BaseColor").SetTexture(
                        0, model.UseImage memoryImage, null,
                        SharpGLTF.Schema2.TextureWrapMode.REPEAT, SharpGLTF.Schema2.TextureWrapMode.REPEAT,
                        SharpGLTF.Schema2.TextureMipMapFilter.DEFAULT, SharpGLTF.Schema2.TextureInterpolationFilter.DEFAULT) |> ignore
                materials.[box texture] <- m
                m

        let addPrimitive (mesh: SharpGLTF.Schema2.Mesh) (mat: SharpGLTF.Schema2.Material)
                         (positions: V3[]) (normals: V3[]) (uvs: V2[]) (indices: int[]) =
            let primitive = mesh.CreatePrimitive()
            SharpGLTF.Schema2.Toolkit.WithVertexAccessor(primitive, "POSITION", (positions :> IReadOnlyList<V3>), false) |> ignore
            SharpGLTF.Schema2.Toolkit.WithVertexAccessor(primitive, "NORMAL", (normals :> IReadOnlyList<V3>), false) |> ignore
            SharpGLTF.Schema2.Toolkit.WithVertexAccessor(primitive, "TEXCOORD_0", (uvs :> IReadOnlyList<V2>), false) |> ignore
            SharpGLTF.Schema2.Toolkit.WithIndicesAccessor(primitive, SharpGLTF.Schema2.PrimitiveType.TRIANGLES, (indices :> IReadOnlyList<int>)) |> ignore
            SharpGLTF.Schema2.Toolkit.WithMaterial(primitive, mat) |> ignore

        let sphereMesh (mesh: SharpGLTF.Schema2.Mesh) (sphere: SphereShape) =
            let segments, rings = 64, 32
            let positions = ResizeArray<V3>()
            let normals = ResizeArray<V3>()
            let uvs = ResizeArray<V2>()
            for ring in 0 .. rings do
                let v = 1. - float ring / float rings
                let polar = Math.PI * float ring / float rings
                for segment in 0 .. segments do
                    let u = float segment / float segments
                    let azimuth = 2. * Math.PI * u
                    // Matches SphereShape's mapping: u = atan2(x, z) / 2pi, v = 1 - acos(y) / pi.
                    let n = Vector(sin polar * sin azimuth, cos polar, sin polar * cos azimuth)
                    let p = sphere.origin + sphere.radius * n
                    positions.Add(V3(float32 p.X, float32 p.Y, float32 p.Z))
                    normals.Add(v3 n)
                    uvs.Add(V2(float32 u, float32 (1. - v)))
            let indices =
                [| for ring in 0 .. rings - 1 do
                     for segment in 0 .. segments - 1 do
                         let a = ring * (segments + 1) + segment
                         let b = a + segments + 1
                         yield! [ a; b; a + 1; a + 1; b; b + 1 ] |]
            addPrimitive mesh (material sphere.tex 1. 1.) (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) indices

        let boxMesh (mesh: SharpGLTF.Schema2.Mesh) (box: Box) =
            let l, h = box.low, box.high
            let faces =
                [ Vector(1., 0., 0.), [ Point(h.X, l.Y, h.Z); Point(h.X, l.Y, l.Z); Point(h.X, h.Y, l.Z); Point(h.X, h.Y, h.Z) ]
                  Vector(-1., 0., 0.), [ Point(l.X, l.Y, l.Z); Point(l.X, l.Y, h.Z); Point(l.X, h.Y, h.Z); Point(l.X, h.Y, l.Z) ]
                  Vector(0., 1., 0.), [ Point(l.X, h.Y, h.Z); Point(h.X, h.Y, h.Z); Point(h.X, h.Y, l.Z); Point(l.X, h.Y, l.Z) ]
                  Vector(0., -1., 0.), [ Point(l.X, l.Y, l.Z); Point(h.X, l.Y, l.Z); Point(h.X, l.Y, h.Z); Point(l.X, l.Y, h.Z) ]
                  Vector(0., 0., 1.), [ Point(l.X, l.Y, h.Z); Point(h.X, l.Y, h.Z); Point(h.X, h.Y, h.Z); Point(l.X, h.Y, h.Z) ]
                  Vector(0., 0., -1.), [ Point(h.X, l.Y, l.Z); Point(l.X, l.Y, l.Z); Point(l.X, h.Y, l.Z); Point(h.X, h.Y, l.Z) ] ]
            let positions = [| for _, corners in faces do for p in corners -> V3(float32 p.X, float32 p.Y, float32 p.Z) |]
            let normals = [| for n, _ in faces do for _ in 1 .. 4 -> v3 n |]
            let uvs = [| for _ in faces do yield! [ V2(0.f, 1.f); V2(1.f, 1.f); V2(1.f, 0.f); V2(0.f, 0.f) ] |]
            let indices = [| for f in 0 .. 5 do let a = 4 * f in yield! [ a; a + 1; a + 2; a; a + 2; a + 3 ] |]
            warn "Boxes export with the material of their top face."
            addPrimitive mesh (material box.top 1. 1.) positions normals uvs indices

        let cylinderMesh (mesh: SharpGLTF.Schema2.Mesh) (centre: Point) radius height texture capped =
            let segments = 48
            let positions = ResizeArray<V3>()
            let normals = ResizeArray<V3>()
            let uvs = ResizeArray<V2>()
            let indices = ResizeArray<int>()
            let bottom, top = centre.Y - height / 2., centre.Y + height / 2.
            for segment in 0 .. segments do
                let u = float segment / float segments
                let angle = 2. * Math.PI * u
                let n = Vector(sin angle, 0., cos angle)
                for y, v in [ bottom, 0.; top, 1. ] do
                    positions.Add(V3(float32 (centre.X + radius * n.X), float32 y, float32 (centre.Z + radius * n.Z)))
                    normals.Add(v3 n)
                    uvs.Add(V2(float32 u, float32 (1. - v)))
            for segment in 0 .. segments - 1 do
                let a = 2 * segment
                indices.AddRange [ a; a + 2; a + 1; a + 1; a + 2; a + 3 ]
            if capped then
                for y, ny in [ top, 1.; bottom, -1. ] do
                    let hub = positions.Count
                    positions.Add(V3(float32 centre.X, float32 y, float32 centre.Z))
                    normals.Add(V3(0.f, float32 ny, 0.f))
                    uvs.Add(V2(0.5f, 0.5f))
                    for segment in 0 .. segments do
                        let angle = 2. * Math.PI * float segment / float segments
                        positions.Add(V3(float32 (centre.X + radius * sin angle), float32 y, float32 (centre.Z + radius * cos angle)))
                        normals.Add(V3(0.f, float32 ny, 0.f))
                        uvs.Add(V2(float32 (0.5 + 0.5 * sin angle), float32 (0.5 + 0.5 * cos angle)))
                    for segment in 0 .. segments - 1 do
                        let a, b = hub + 1 + segment, hub + 2 + segment
                        if ny > 0. then indices.AddRange [ hub; a; b ] else indices.AddRange [ hub; b; a ]
            addPrimitive mesh (material texture 1. 1.) (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) (indices.ToArray())

        let planeMesh (mesh: SharpGLTF.Schema2.Mesh) (plane: InfinitePlane) =
            // An unbounded plane becomes a 200 x 200 quad; its texture is baked over a 2 x 2 tile and repeated.
            let size = 100.f
            let positions = [| V3(-size, -size, 0.f); V3(size, -size, 0.f); V3(size, size, 0.f); V3(-size, size, 0.f) |]
            let normals = Array.create 4 (V3(0.f, 0.f, 1.f))
            let uvs = positions |> Array.map (fun p -> V2(p.X / 2.f, 1.f - p.Y / 2.f))
            warn "Infinite planes export as 200 x 200 quads."
            addPrimitive mesh (material plane.tex 2. 2.) positions normals uvs [| 0; 1; 2; 0; 2; 3 |]

        let triangleMesh (mesh: SharpGLTF.Schema2.Mesh) (shape: TriangleMesh.MeshShape) =
            let data = shape.Geometry.Export()
            let vertices = data.Vertices.ToArray()
            let positions = vertices |> Array.map (fun v -> V3(float32 v.X, float32 v.Y, float32 v.Z))
            let normals =
                vertices |> Array.map (fun v ->
                    let n = Vector(v.Nx, v.Ny, v.Nz)
                    if n.IsZero then V3.UnitY else v3 n.Normalise)
            let uvs = vertices |> Array.map (fun v -> V2(float32 v.U, float32 (1. - v.V)))
            let indices = data.Triangles.ToArray() |> Array.collect (fun t -> [| t.A; t.B; t.C |])
            if not data.SmoothShading then warn "Flat-shaded meshes export with per-vertex normals."
            addPrimitive mesh (material shape.Texture 1. 1.) positions normals uvs indices

        let exportGeometry (target: SharpGLTF.Schema2.Node) (shape: Shape) =
            let mesh = model.CreateMesh(target.Name)
            match shape with
            | :? SphereShape as sphere -> sphereMesh mesh sphere; target.Mesh <- mesh
            | :? Box as box -> boxMesh mesh box; target.Mesh <- mesh
            | :? InfinitePlane as plane -> planeMesh mesh plane; target.Mesh <- mesh
            | :? SolidCylinder as c -> cylinderMesh mesh c.center c.radius c.height c.cylinder true; target.Mesh <- mesh
            | :? HollowCylinder as c -> cylinderMesh mesh c.center c.radius c.height c.tex false; target.Mesh <- mesh
            | :? TriangleMesh.MeshShape as triangles -> triangleMesh mesh triangles; target.Mesh <- mesh
            | other -> warn $"Shapes of type {other.GetType().Name} cannot be exported and are skipped."

        let exportLight (target: SharpGLTF.Schema2.Node) (light: Light) =
            match light with
            | :? DirectionalLight as sun ->
                // glTF directional lights shine along -Z; aim a child node so -Z points away from the light.
                let child = target.CreateNode(target.Name + "-light")
                child.LocalTransform <- SharpGLTF.Transforms.AffineTransform(V3.One, nq (Quaternion.lookRotation (-sun.Direction) (Vector(0., 1., 0.))), V3.Zero)
                let l = model.CreatePunctualLight(SharpGLTF.Schema2.PunctualLightType.Directional)
                l.SetColor(V3(float32 sun.BaseColour.R, float32 sun.BaseColour.G, float32 sun.BaseColour.B), float32 (sun.Intensity / ImportOptions.Default.DirectionalLightScale), Single.PositiveInfinity)
                child.PunctualLight <- l
            | :? PointLight as point ->
                let child = target.CreateNode(target.Name + "-light")
                child.LocalTransform <- SharpGLTF.Transforms.AffineTransform(V3.One, NQ.Identity, V3(float32 point.Position.X, float32 point.Position.Y, float32 point.Position.Z))
                let l = model.CreatePunctualLight(SharpGLTF.Schema2.PunctualLightType.Point)
                l.SetColor(V3(float32 point.BaseColour.R, float32 point.BaseColour.G, float32 point.BaseColour.B), float32 (point.Intensity / ImportOptions.Default.PointLightScale), Single.PositiveInfinity)
                child.PunctualLight <- l
            | other -> warn $"Lights of type {other.GetType().Name} (e.g. environment skies) cannot be exported and are skipped."

        let rec exportNode (parent: Choice<SharpGLTF.Schema2.Scene, SharpGLTF.Schema2.Node>) (node: Node) =
            let target =
                match parent with
                | Choice1Of2 s -> s.CreateNode(node.Name)
                | Choice2Of2 p -> p.CreateNode(node.Name)
            target.LocalTransform <- SharpGLTF.Transforms.AffineTransform(v3 node.Rest.Scale, nq node.Rest.Rotation, v3 node.Rest.Translation)
            nodes.[node.Name] <- target
            let geometry = node.Content |> List.choose (function Geometry shape -> Some shape | _ -> None)
            match geometry with
            | [ shape ] -> exportGeometry target shape
            | shapes ->
                // glTF allows one mesh per node; give extra shapes their own child nodes.
                shapes |> List.iteri (fun i shape -> exportGeometry (target.CreateNode($"{node.Name}-shape{i}")) shape)
            for content in node.Content do
                match content with
                | LightSource light -> exportLight target light
                | CameraRig spec ->
                    let camera = model.CreateCamera(node.Name)
                    camera.SetPerspectiveMode(Nullable(), float32 spec.YFov, 0.05f, 1000.f)
                    target.Camera <- camera
                    if spec.ApertureRadius > 0. then warn "Depth of field is not part of glTF and is not exported."
                | Skinned _ -> warn "Skinned meshes are not exported."
                | Procedural _ -> warn "Procedural content is not exported."
                | Geometry _ -> ()
            for child in node.Children do exportNode (Choice2Of2 target) child

        let times (duration: float) =
            let count = max 1 (int (ceil (duration * resampleRate)))
            [ for i in 0 .. count -> duration * float i / float count ]

        let exportClip (clip: Clip) =
            let animation = model.CreateAnimation(clip.Name)
            for channel in clip.Channels do
                let node = nodes.[channel.Node]
                let resampled (s: Sampler<'T>) = s.Ease.IsSome
                if (match channel.Track with Translation s | Scale s -> resampled s | Rotation s -> resampled s) then
                    warn "Eased tracks are resampled to linear keys."
                let dict (pairs: seq<float * 'T>) =
                    let d = Dictionary<float32, 'T>()
                    for t, v in pairs do d.[float32 t] <- v
                    d :> IReadOnlyDictionary<float32, 'T>
                let cubic (s: Sampler<'T>) (convert: 'T -> 'G) =
                    let d = Dictionary<float32, struct ('G * 'G * 'G)>()
                    for i in 0 .. s.Times.Length - 1 do d.[float32 s.Times.[i]] <- struct (convert s.InTangents.[i], convert s.Values.[i], convert s.OutTangents.[i])
                    d :> IReadOnlyDictionary<float32, struct ('G * 'G * 'G)>
                match channel.Track with
                | Translation s | Scale s when s.Ease.IsNone && s.Interpolation = CubicSpline ->
                    let d = cubic s v3
                    match channel.Track with
                    | Translation _ -> animation.CreateTranslationChannel(node, d)
                    | _ -> animation.CreateScaleChannel(node, d)
                | Translation s | Scale s ->
                    let keys, linear =
                        if s.Ease.IsNone then Array.zip s.Times s.Values |> Seq.map (fun (t, v) -> t, v3 v), s.Interpolation = Linear
                        else times s.Times.[s.Times.Length - 1] |> Seq.map (fun t -> t, v3 (Sampler.evaluateVector s t)), true
                    match channel.Track with
                    | Translation _ -> animation.CreateTranslationChannel(node, dict keys, linear)
                    | _ -> animation.CreateScaleChannel(node, dict keys, linear)
                | Rotation s when s.Ease.IsNone && s.Interpolation = CubicSpline ->
                    animation.CreateRotationChannel(node, cubic s nq)
                | Rotation s ->
                    let keys, linear =
                        if s.Ease.IsNone then Array.zip s.Times s.Values |> Seq.map (fun (t, q) -> t, nq q), s.Interpolation = Linear
                        else times s.Times.[s.Times.Length - 1] |> Seq.map (fun t -> t, nq (Sampler.evaluateRotation s t)), true
                    animation.CreateRotationChannel(node, dict keys, linear)

        /// glTF has no aim constraint, so a targeted camera gets a baked rotation track.
        let exportCameraAim () =
            if not scene.Cuts.IsEmpty then warn "Camera cuts are not part of glTF; only the first camera is animated."
            let scene = { scene with Cuts = [] }
            let cameraNode = AnimatedScene.nodes scene |> Seq.find (fun n -> n.Name = scene.ActiveCamera)
            let spec = cameraNode.Content |> List.pick (function CameraRig s -> Some s | _ -> None)
            match spec.Target with
            | None -> ()
            | Some _ ->
                let animated = scene.Clips |> List.exists (fun c -> c.Channels |> List.exists (fun ch -> ch.Node = cameraNode.Name && (match ch.Track with Rotation _ -> true | _ -> false)))
                if animated then warn "The camera's own rotation track is replaced by its baked aim."
                let parent = AnimatedScene.nodes scene |> Seq.tryFind (fun n -> n.Children |> List.exists (fun c -> c.Name = cameraNode.Name))
                let keys =
                    times scene.Duration |> List.map (fun t ->
                        let pose = AnimatedScene.cameraPose scene t
                        let world = Quaternion.lookRotation (pose.LookAt - pose.Position) pose.Up
                        let local =
                            match parent with
                            | None -> world
                            | Some p ->
                                let m = (AnimatedScene.worldMatrices scene t).[p.Name]
                                let column (x: float) y z = Vector(x, y, z).Normalise
                                let c1, c2, c3 = column m.Pos1x1 m.Pos2x1 m.Pos3x1, column m.Pos1x2 m.Pos2x2 m.Pos3x2, column m.Pos1x3 m.Pos2x3 m.Pos3x3
                                let parentRotation =
                                    Quaternion.ofRotationMatrix
                                        { identityMatrix with
                                            Pos1x1 = c1.X; Pos2x1 = c1.Y; Pos3x1 = c1.Z
                                            Pos1x2 = c2.X; Pos2x2 = c2.Y; Pos3x2 = c2.Z
                                            Pos1x3 = c3.X; Pos2x3 = c3.Y; Pos3x3 = c3.Z }
                                Quaternion.multiply (Quaternion.conjugate parentRotation) world
                        t, nq local)
                let animation = model.CreateAnimation("camera-aim")
                let d = Dictionary<float32, NQ>()
                for t, q in keys do d.[float32 t] <- q
                animation.CreateRotationChannel(nodes.[cameraNode.Name], d, true)

        member _.Export() =
            let root = model.UseScene("default")
            for node in scene.Roots do exportNode (Choice1Of2 root) node
            if not scene.StaticShapes.IsEmpty then warn "StaticShapes are not exported; put geometry in nodes to export it."
            let statics = scene.StaticLights
            if not statics.IsEmpty then
                let holder = root.CreateNode("static-lights")
                for light in statics do exportLight holder light
            for clip in scene.Clips do
                if not clip.Channels.IsEmpty then exportClip clip
            exportCameraAim ()
            List.ofSeq warnings

    /// Writes the scene, its hierarchy, meshes (analytic shapes are tessellated), lights, camera and animation to
    /// a .gltf or .glb file. Eased tracks and aimed cameras are resampled at `resampleRate` Hz. Returns warnings.
    let save (scene: AnimatedScene) (path: string) (resampleRate: float) =
        let model = SharpGLTF.Schema2.ModelRoot.CreateModel()
        let warnings = Exporter(model, scene, resampleRate).Export()
        let directory = Path.GetDirectoryName(Path.GetFullPath path)
        Directory.CreateDirectory directory |> ignore
        if Path.GetExtension(path).Equals(".glb", StringComparison.OrdinalIgnoreCase) then model.SaveGLB path
        else model.SaveGLTF path
        warnings
