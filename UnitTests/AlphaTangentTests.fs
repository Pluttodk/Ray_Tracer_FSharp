module AlphaTangentTests

open System
open System.IO
open Assert
open Tracer.Basics
open Tracer.Basics.PathTracing
open Tracer.Basics.TriangleMesh
open Tracer.Animation

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance

// Unit quad in the z = 0 plane facing +z, with u = (x + 1) / 2 and v = (y + 1) / 2 (v up, as meshes store it).
let private quadPositions = [| Point(-1., -1., 0.); Point(1., -1., 0.); Point(1., 1., 0.); Point(-1., 1., 0.) |]
let private quadNormals = Array.create 4 (Vector(0., 0., 1.))
let private quadUvs = [| 0., 0.; 1., 0.; 1., 1.; 0., 1. |]
let private quadTriangles = [| 0; 1; 2; 0; 2; 3 |]
let private plain = Textures.mkMatTexture (PbrMaterial PbrSample.defaults)

/// Left half (u < 0.5) transparent, right half opaque.
let private halfMask = AlphaMask.Cutout((fun u _ -> if u < 0.5 then 0. else 1.), 0.5)

let private quad tangents mask =
    (fromArraysWith tangents mask quadPositions quadNormals quadUvs quadTriangles true).toShape plain

let private down x y = Ray(Point(x, y, 5.), Vector(0., 0., -1.))

/// Masked quad in front of an opaque backstop at z = -1: rays through the transparent half reach the backstop,
/// for closest-hit (camera and bounce rays) and any-hit (shadow rays) queries alike, also when instanced.
let private maskTests () =
    let masked = quad NoTangents halfMask
    let backstop = Transform.transform (quad NoTangents null) (Transformation.translate 0. 0. -1.)
    for shape, label in [ masked, "direct"; Transform.transform masked (Transformation.scale 2. 2. 2.), "instanced" ] do
        let scene = Acceleration.buildWith Acceleration.FlatBVH [| shape; backstop |]
        let through = Acceleration.traverseClosest scene (down -0.4 0.3) 0. infinity
        let blocked = Acceleration.traverseClosest scene (down 0.4 0.3) 0. infinity
        Assert.True (through.DidHit && near 1e-9 through.Time 6., $"alpha-mask-camera-ray-passes-transparent-half-{label}")
        Assert.True (blocked.DidHit && near 1e-9 blocked.Time 5., $"alpha-mask-camera-ray-stops-on-opaque-half-{label}")
        // Shadow rays from below the quad up toward a light above it.
        let alone = Acceleration.buildWith Acceleration.FlatBVH [| shape |]
        let up x = Ray(Point(x, 0.3, -0.5), Vector(0., 0., 1.))
        Assert.True (not (Acceleration.anyHit alone (up -0.4) 0. 10.), $"alpha-mask-shadow-ray-passes-transparent-half-{label}")
        Assert.True (Acceleration.anyHit alone (up 0.4) 0. 10., $"alpha-mask-shadow-ray-blocked-by-opaque-half-{label}")
    // Stochastic coverage keeps the surface for about alpha of the rays, and the same ray always agrees.
    let film = quad NoTangents (AlphaMask.Stochastic(fun _ _ -> 0.35))
    let rays = [| for i in 0 .. 3999 -> Ray(Point(-0.9 + 1.8 * float (i % 63) / 63., -0.9 + 1.8 * float (i / 63) / 64., 5.), Vector(0.01 * float (i % 7), 0., -1.)) |]
    let hits = rays |> Array.filter (fun r -> (film.hitFunction r).DidHit) |> Array.length
    Assert.True (near 0.04 (float hits / float rays.Length) 0.35, "alpha-stochastic-coverage-matches-alpha")
    Assert.True (rays |> Array.forall (fun r -> (film.hitFunction r).DidHit = (film.hitFunction r).DidHit), "alpha-stochastic-coverage-is-deterministic-per-ray")

/// Without a mask (or with one that covers everything) hits are bit-identical to the plain mesh path.
let private opaqueIdentityTests () =
    let reference = (fromArrays quadPositions quadNormals quadUvs quadTriangles true).toShape plain
    let full = quad NoTangents (AlphaMask.Cutout((fun _ _ -> 1.), 0.5))
    let none = quad NoTangents null
    let same (a: HitPoint) (b: HitPoint) =
        a.DidHit = b.DidHit && a.Time = b.Time && a.U = b.U && a.V = b.V && a.Normal = b.Normal
        && a.Point = b.Point && a.ShadowPoint = b.ShadowPoint && a.BarycentricBeta = b.BarycentricBeta
    let rays = [ for x in -1.2 .. 0.17 .. 1.2 do for y in -1.2 .. 0.23 .. 1.2 -> Ray(Point(x, y, 3.), Vector(0.1 * x, -0.05, -1.)) ]
    Assert.True (rays |> List.forall (fun r -> same (reference.hitFunction r) (none.hitFunction r)), "alpha-opaque-mesh-hits-bit-identical")
    Assert.True (rays |> List.forall (fun r -> same (reference.hitFunction r) (full.hitFunction r)), "alpha-full-coverage-hits-bit-identical")
    Assert.True (rays |> List.forall (fun r -> not (none.hitFunction r).HasTangent), "alpha-no-tangent-source-carries-no-tangent")

/// A bumpy grid over the unit square whose u runs along a rotated, sheared direction.
let private generatedTangentTests () =
    let n = 6
    let positions =
        [| for j in 0 .. n do
             for i in 0 .. n ->
                let x, y = float i / float n, float j / float n
                Point(x, y, 0.05 * sin (3. * x) * cos (2. * y)) |]
    let uvs = [| for j in 0 .. n do for i in 0 .. n -> (0.8 * float i / float n + 0.3 * float j / float n, float j / float n) |]
    let triangles =
        [| for j in 0 .. n - 1 do
             for i in 0 .. n - 1 do
                let a = j * (n + 1) + i
                yield! [| a; a + 1; a + n + 2; a; a + n + 2; a + n + 1 |] |]
    let shape = (fromArraysWith GenerateTangents null positions [||] uvs triangles true).toShape plain
    let geometry = (shape :?> MeshShape).Geometry
    let tangents = geometry.Tangents.ToArray()
    let mutable orthogonal, unit, aligned = true, true, true
    // Vertex normals are generated by the mesh too; recover them through hits at the vertices' neighbourhood.
    for j in 1 .. n - 1 do
        for i in 1 .. n - 1 do
            let x, y = float i / float n + 0.013, float j / float n + 0.007
            let hit = shape.hitFunction (Ray(Point(x, y, 3.), Vector(0., 0., -1.)))
            let struct (t, b) = PbrShading.hitTangentFrame hit
            if not hit.HasTangent then aligned <- false
            if abs (t * hit.Normal) > 1e-9 || abs (b * hit.Normal) > 1e-9 || abs (t * b) > 1e-9 then orthogonal <- false
            if not (near 1e-9 t.Magnitude 1.) || not (near 1e-9 b.Magnitude 1.) then unit <- false
            // Positions as a function of (u, v): x = (u - 0.3 v) / 0.8, y = v, so dP/du is along +x and dP/dv along (-0.375, 1).
            if t.X < 0.95 || b * Vector(-0.375, 1., 0.).Normalise < 0.9 then aligned <- false
    Assert.True (orthogonal, "tangents-generated-orthogonal-to-normal")
    Assert.True (unit, "tangents-generated-frame-is-unit")
    Assert.True (aligned, "tangents-generated-follow-plus-u-and-v")
    Assert.True (tangents |> Array.forall (fun t -> near 1e-5 (float (t.Tx * t.Tx + t.Ty * t.Ty + t.Tz * t.Tz)) 1.), "tangents-generated-per-vertex-unit")
    // Mirrored UVs flip the handedness, so the bitangent still follows +v.
    let mirrored = quad GenerateTangents null
    let mirroredUvs = quadUvs |> Array.map (fun (u, v) -> u, 1. - v)
    let flipped = (fromArraysWith GenerateTangents null quadPositions quadNormals mirroredUvs quadTriangles true).toShape plain
    let struct (_, b0) = PbrShading.hitTangentFrame (mirrored.hitFunction (down 0.2 0.1))
    let struct (_, b1) = PbrShading.hitTangentFrame (flipped.hitFunction (down 0.2 0.1))
    Assert.True (b0.Y > 0.99 && b1.Y < -0.99, "tangents-generated-handedness-follows-v")

let private given (t: Vector) w = Array.create 4 { Tx = float32 t.X; Ty = float32 t.Y; Tz = float32 t.Z; W = w }

/// Tilted tangent-space normal (+x in tangent space) mapped to world space at a hit.
let private perturbed (hit: HitPoint) (n: Vector) =
    let sample = { PbrSample.defaults with Normal = n.Normalise }
    let surface = PbrShading.surfaceAt sample hit 1.
    (ShadingFrame.ofNormal hit.Normal).ToWorld surface.Normal

let private givenTangentTests () =
    // A tangent that is not the UV gradient: the given one wins.
    let rotated = quad (GivenTangents (given (Vector(0., 1., 0.)) 1.f)) null
    let hit = rotated.hitFunction (down 0.3 -0.2)
    let struct (t, b) = PbrShading.hitTangentFrame hit
    Assert.True (hit.HasTangent && near 1e-6 t.Y 1. && near 1e-6 b.X -1., "tangents-given-are-honoured")
    let mirrored = quad (GivenTangents (given (Vector(0., 1., 0.)) -1.f)) null
    let struct (_, b) = PbrShading.hitTangentFrame (mirrored.hitFunction (down 0.3 -0.2))
    Assert.True (near 1e-6 b.X 1., "tangents-given-handedness-is-honoured")
    // Instancing rotates the tangent with the surface.
    let turned = Transform.transform (quad (GivenTangents (given (Vector(1., 0., 0.)) 1.f)) null) (Transformation.rotateZ (Math.PI / 2.))
    let struct (t, _) = PbrShading.hitTangentFrame (turned.hitFunction (down 0.3 -0.2))
    Assert.True (near 1e-6 t.Y 1., "tangents-transform-with-instance")
    // Normal maps follow the tangent: tilting toward +x in tangent space tilts toward the tangent in world space.
    let tilt = Vector(0.4, 0., 1.)
    let alongX = perturbed ((quad (GivenTangents (given (Vector(1., 0., 0.)) 1.f)) null).hitFunction (down 0.3 -0.2)) tilt
    let alongY = perturbed (hit) tilt
    Assert.True (alongX.X > 0.3 && near 1e-9 alongX.Y 0., "tangents-normal-map-tilts-along-tangent")
    Assert.True (alongY.Y > 0.3 && near 1e-9 alongY.X 0., "tangents-normal-map-tilts-along-given-tangent")
    // Seen from behind (double-sided), the perturbed normal mirrors the front one and faces the viewer.
    let back = (quad (GivenTangents (given (Vector(1., 0., 0.)) 1.f)) null).hitFunction (Ray(Point(0.3, -0.2, -5.), Vector(0., 0., 1.)))
    let behind = perturbed back tilt
    Assert.True (not back.FrontFace && behind.Z < 0. && near 1e-9 behind.X -alongX.X, "tangents-back-face-normal-map-mirrors")

/// A flat normal map is the identity whether or not the hit carries tangents, including tangents that are
/// neither unit nor orthogonal to the shading normal.
let private flatMapIdentityTests () =
    let skewed = quad (GivenTangents (given (Vector(2., 0.5, 0.7)) 1.f)) null
    let curvedNormals = quadPositions |> Array.map (fun p -> Vector(0.3 * p.X, 0.2 * p.Y, 1.).Normalise)
    let curved = (fromArraysWith GenerateTangents null quadPositions curvedNormals quadUvs quadTriangles true).toShape plain
    let flat = PbrParams.sampleAt { PbrParams.defaults with NormalMap = Some (fun _ _ -> Vector(0., 0., 1.)) } 0.3 0.4
    for shape, label in [ skewed, "skewed"; curved, "curved" ] do
        for x, y in [ 0.3, -0.2; -0.7, 0.6; 0.05, 0.9 ] do
            let hit = shape.hitFunction (down x y)
            let surface = PbrShading.surfaceAt flat hit 1.
            Assert.True (hit.HasTangent && surface.Normal.X = 0. && surface.Normal.Y = 0. && surface.Normal.Z = 1., $"tangents-flat-normal-map-is-identity-{label}")
            // A nearly flat map (8-bit 128/255 encodes 0.0039) stays within a hair of the shading normal.
            let almost = perturbed hit (Vector(1. / 255., 1. / 255., 1.))
            Assert.True (almost * hit.Normal > 0.9999, $"tangents-near-flat-normal-map-is-near-identity-{label}")

// ------------------------------------------------------------- glTF import

let private writeFixture (directory: string) =
    let png (width: int) (height: int) (rgba: byte[]) =
        use stream = new MemoryStream()
        StbImageWriteSharp.ImageWriter().WritePng(rgba, width, height, StbImageWriteSharp.ColorComponents.RedGreenBlueAlpha, stream)
        "data:image/png;base64," + Convert.ToBase64String(stream.ToArray())
    // Left texel fully transparent, right texel opaque: with bilinear filtering, alpha is 0 at u = 0.25 and 1 at u = 0.75.
    let baseColour = png 2 1 [| 200uy; 50uy; 50uy; 0uy; 50uy; 200uy; 50uy; 255uy |]
    let flatNormal = png 1 1 [| 128uy; 128uy; 255uy; 255uy |]
    let bytes = ResizeArray<byte>()
    let f32 (v: float32) = bytes.AddRange(BitConverter.GetBytes v)
    for p in quadPositions do f32 (float32 p.X); f32 (float32 p.Y); f32 (float32 p.Z)
    for _ in quadPositions do f32 0.f; f32 0.f; f32 1.f
    // glTF texture space has t = 0 at the top.
    for p in quadPositions do f32 (float32 ((p.X + 1.) / 2.)); f32 (float32 ((1. - p.Y) / 2.))
    for _ in quadPositions do f32 0.f; f32 1.f; f32 0.f; f32 1.f
    for i in quadTriangles do bytes.AddRange(BitConverter.GetBytes(uint16 i))
    let buffer = "data:application/octet-stream;base64," + Convert.ToBase64String(bytes.ToArray())
    let json = $$"""
{ "asset": { "version": "2.0" },
  "scene": 0, "scenes": [ { "nodes": [ 0 ] } ],
  "nodes": [ { "mesh": 0, "name": "leaf" } ],
  "meshes": [ { "primitives": [ { "attributes": { "POSITION": 0, "NORMAL": 1, "TEXCOORD_0": 2, "TANGENT": 3 }, "indices": 4, "material": 0 } ] } ],
  "materials": [ { "name": "leaf", "alphaMode": "MASK", "alphaCutoff": 0.5, "doubleSided": true,
                   "pbrMetallicRoughness": { "baseColorTexture": { "index": 0 }, "metallicFactor": 0.0 },
                   "normalTexture": { "index": 1 } } ],
  "textures": [ { "source": 0 }, { "source": 1 } ],
  "images": [ { "uri": "{{baseColour}}" }, { "uri": "{{flatNormal}}" } ],
  "buffers": [ { "byteLength": {{bytes.Count}}, "uri": "{{buffer}}" } ],
  "bufferViews": [
    { "buffer": 0, "byteOffset": 0, "byteLength": 48 },
    { "buffer": 0, "byteOffset": 48, "byteLength": 48 },
    { "buffer": 0, "byteOffset": 96, "byteLength": 32 },
    { "buffer": 0, "byteOffset": 128, "byteLength": 64 },
    { "buffer": 0, "byteOffset": 192, "byteLength": 12 } ],
  "accessors": [
    { "bufferView": 0, "componentType": 5126, "count": 4, "type": "VEC3", "min": [ -1, -1, 0 ], "max": [ 1, 1, 0 ] },
    { "bufferView": 1, "componentType": 5126, "count": 4, "type": "VEC3" },
    { "bufferView": 2, "componentType": 5126, "count": 4, "type": "VEC2" },
    { "bufferView": 3, "componentType": 5126, "count": 4, "type": "VEC4" },
    { "bufferView": 4, "componentType": 5123, "count": 6, "type": "SCALAR" } ] }
"""
    let path = Path.Combine(directory, "masked_leaf.gltf")
    File.WriteAllText(path, json)
    path

let private importTests () =
    let directory = Path.Combine(Path.GetTempPath(), "raytracer-alpha-tests-" + string Environment.ProcessId)
    Directory.CreateDirectory directory |> ignore
    try
        let result = Gltf.load (writeFixture directory) Gltf.ImportOptions.Default
        Assert.True (result.Warnings |> List.forall (fun w -> not (w.Contains "lpha")), "gltf-mask-imports-without-alpha-warning")
        let shapes =
            [| for node in AnimatedScene.nodes result.Scene do
                 for content in node.Content do
                     match content with
                     | Geometry shape -> yield shape
                     | _ -> () |]
        let scene = Acceleration.buildWith Acceleration.FlatBVH shapes
        let transparent = Acceleration.traverseClosest scene (down -0.5 0.2) 0. infinity
        let opaque = Acceleration.traverseClosest scene (down 0.5 0.2) 0. infinity
        Assert.True (not transparent.DidHit, "gltf-mask-camera-ray-passes-transparent-texels")
        Assert.True (opaque.DidHit && near 1e-6 opaque.Time 5., "gltf-mask-camera-ray-hits-opaque-texels")
        Assert.True (not (Acceleration.anyHit scene (Ray(Point(-0.5, 0.2, -1.), Vector(0., 0., 1.))) 0. 10.), "gltf-mask-shadow-ray-passes-transparent-texels")
        Assert.True (Acceleration.anyHit scene (Ray(Point(0.5, 0.2, -1.), Vector(0., 0., 1.))) 0. 10., "gltf-mask-shadow-ray-blocked-by-opaque-texels")
        let struct (t, b) = PbrShading.hitTangentFrame opaque
        Assert.True (opaque.HasTangent && near 1e-6 t.Y 1. && near 1e-6 b.X -1., "gltf-tangent-attribute-is-honoured")
        // Back faces of a double-sided leaf shade with a normal toward the viewer.
        let behind = Acceleration.traverseClosest scene (Ray(Point(0.5, 0.2, -5.), Vector(0., 0., 1.))) 0. infinity
        Assert.True (behind.DidHit && not behind.FrontFace && behind.Normal.Z < -0.99, "gltf-double-sided-back-face-normal-faces-viewer")
    finally
        try Directory.Delete(directory, true) with _ -> ()

let private imageTests () =
    use stream = new MemoryStream()
    StbImageWriteSharp.ImageWriter().WritePng([| 1uy; 2uy; 3uy; 40uy; 5uy; 6uy; 7uy; 250uy |], 2, 1, StbImageWriteSharp.ColorComponents.RedGreenBlueAlpha, stream)
    stream.Position <- 0L
    let struct (rgb, alpha) = Tracer.Imaging.RgbImage.LoadWithAlpha stream
    use rgb = rgb
    Assert.True (rgb.Pixels = [| 1uy; 2uy; 3uy; 5uy; 6uy; 7uy |] && alpha = [| 40uy; 250uy |], "image-rgba-load-splits-alpha")
    use opaque = new MemoryStream()
    StbImageWriteSharp.ImageWriter().WritePng([| 1uy; 2uy; 3uy |], 1, 1, StbImageWriteSharp.ColorComponents.RedGreenBlue, opaque)
    opaque.Position <- 0L
    let struct (rgb, alpha) = Tracer.Imaging.RgbImage.LoadWithAlpha opaque
    use rgb = rgb
    Assert.True (rgb.Pixels = [| 1uy; 2uy; 3uy |] && alpha.Length = 0, "image-rgb-load-has-no-alpha-plane")

let allTest () =
    maskTests ()
    opaqueIdentityTests ()
    generatedTangentTests ()
    givenTangentTests ()
    flatMapIdentityTests ()
    imageTests ()
    importTests ()
