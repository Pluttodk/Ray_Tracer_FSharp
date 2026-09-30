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
          SmoothShading: bool }
        static member Default = { Clip = None; PointLightScale = 0.02; DirectionalLightScale = 0.3; SmoothShading = true }

    type ImportResult = { Scene: AnimatedScene; Warnings: string list }

    let private vector (v: V3) = Vector(float v.X, float v.Y, float v.Z)
    let private quaternion (q: NQ) = Quaternion.normalise { X = float q.X; Y = float q.Y; Z = float q.Z; W = float q.W }
    let private colour (v: V3) = Colour(float v.X, float v.Y, float v.Z)
    let private clamp01 (v: float) = if Double.IsFinite v then max 0. (min 1. v) else 0.

    /// Phong exponent whose GGX roughness (see MaterialAdapter) matches a glTF roughness.
    let exponentOfRoughness (roughness: float) =
        let r = max 0.02 (clamp01 roughness)
        int (Math.Clamp(2. / (r * r * r * r) - 2., 1., 10000.))

    // ---------------------------------------------------------------- import

    type private Importer(model: SharpGLTF.Schema2.ModelRoot, options: ImportOptions) =
        let warnings = ResizeArray<string>()
        let warned = HashSet<string>()
        let warn (message: string) = if warned.Add message then warnings.Add message
        let names = Dictionary<int, string>()
        let used = HashSet<string>()
        let textures = Dictionary<int, Texture>()
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

        let imageTexture (channel: SharpGLTF.Schema2.MaterialChannel) =
            match channel.Texture with
            | null -> None
            | texture when isNull texture.PrimaryImage -> None
            | texture ->
                if channel.TextureCoordinate <> 0 then warn "Only TEXCOORD_0 is supported; other texture coordinate sets are ignored."
                if not (isNull channel.TextureTransform) then warn "KHR_texture_transform is ignored."
                use stream = texture.PrimaryImage.Content.Open()
                use image = RgbImage.Load stream
                let w, h = image.Width, image.Height
                let pixels = Array.copy image.Pixels
                // glTF UV (0,0) is the top-left of the image; meshes store v flipped (v up), so undo that here.
                Some (fun (u: float) (v: float) ->
                    let wrap (x: float) = x - floor x
                    let x = min (w - 1) (int (wrap u * float w))
                    let y = min (h - 1) (int (wrap (1. - v) * float h))
                    let i = (y * w + x) * 3
                    let srgb (b: byte) = Math.Pow(float b / 255., 2.2)
                    Colour(srgb pixels.[i], srgb pixels.[i + 1], srgb pixels.[i + 2]))

        let convertMaterial (material: SharpGLTF.Schema2.Material) =
            if isNull material then mkMatTexture (MatteMaterial(Colour(0.8, 0.8, 0.8), 0.2, Colour(0.8, 0.8, 0.8), 0.8)) |> markOpaque
            else
                match textures.TryGetValue material.LogicalIndex with
                | true, texture -> texture
                | _ ->
                    let channel key = material.FindChannel key |> Option.ofNullable
                    let baseFactor, baseImage =
                        match channel "BaseColor" with
                        | Some c -> Colour(float c.Color.X, float c.Color.Y, float c.Color.Z), imageTexture c
                        | None -> Colour.White, None
                    let metallic, roughness =
                        match channel "MetallicRoughness" with
                        | Some c ->
                            if not (isNull c.Texture) then warn "Metallic-roughness textures are ignored; their factors are used."
                            float (c.GetFactor "MetallicFactor"), float (c.GetFactor "RoughnessFactor")
                        | None -> 1., 1.
                    let emission =
                        match channel "Emissive" with
                        | Some c ->
                            let strength = try float (c.GetFactor "EmissiveStrength") with _ -> 1.
                            Colour(float c.Color.X, float c.Color.Y, float c.Color.Z) * strength
                        | None -> Colour.Black
                    let transmission =
                        match channel "Transmission" with
                        | Some c -> float (c.GetFactor "TransmissionFactor")
                        | None -> 0.
                    if material.Alpha <> SharpGLTF.Schema2.AlphaMode.OPAQUE then warn "Alpha blending and masking are not supported; surfaces render opaque."
                    let ior = let v = float material.IndexOfRefraction in if Double.IsFinite v && v > 0. then v else 1.5
                    let make (c: Colour) : Tracer.Basics.Material =
                        let c = Colour(clamp01 c.R, clamp01 c.G, clamp01 c.B)
                        if emission.R + emission.G + emission.B > 0. then
                            let peak = max emission.R (max emission.G emission.B)
                            EmissiveMaterial(emission / peak, peak) :> _
                        elif transmission > 0.5 then TransparentMaterial(c, Colour.White, ior, 1.) :> _
                        elif metallic >= 0.5 then
                            if roughness < 0.3 then
                                PhongReflectiveMaterial(c, 0.1, c, 0.2, c, 0.6, c, 0.9 * (1. - roughness), exponentOfRoughness roughness) :> _
                            else PhongMaterial(c, 0.15, c, 0.5, c, 0.7 * (1. - roughness) + 0.1, exponentOfRoughness roughness) :> _
                        elif roughness < 0.6 then
                            PhongMaterial(c, 0.2, c, 0.8, Colour.White, 0.4 * (1. - roughness), exponentOfRoughness roughness) :> _
                        else MatteMaterial(c, 0.2, c, 0.8) :> _
                    if metallic >= 0.5 && roughness >= 0.3 then warn "Rough metals are approximated with Phong highlights."
                    if transmission > 0. && transmission <= 0.5 then warn "Partial transmission is ignored."
                    let texture =
                        match baseImage with
                        | Some sample -> mkTexture (fun u v -> make (sample u v * baseFactor)) |> markOpaque
                        | None -> mkMatTexture (make baseFactor)
                    textures.[material.LogicalIndex] <- texture
                    texture

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
                                let smooth = options.SmoothShading && normals.Length > 0
                                let baseShape = TriangleMesh.fromArrays positions normals uvs triangles smooth
                                yield baseShape.toShape (convertMaterial primitive.Material)
                        | other -> warn $"Primitives of type {other} are skipped; only triangles render." ]
                meshes.[mesh.LogicalIndex] <- shapes
                shapes

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
            if not (isNull node.Skin) then warn "Skinned meshes are not supported; they render in their bind pose."
            let content =
                [ if not (isNull node.Mesh) then
                      for shape in convertMesh node.Mesh do yield Geometry shape
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
                { Name = sceneName; Roots = roots; Clips = clips; ActiveCamera = activeCamera; StaticShapes = []
                  StaticLights = lights; Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 4
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
