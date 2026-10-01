namespace Tracer.Animation

open System
open System.IO
open System.Text.Json
open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Basics.Transformation
open Tracer.Imaging

/// Intel's animated Sponza knight (CC-BY 4.0, see assets/sponza/SOURCES.md), played from the vertex cache
/// that scripts/sponza/convert-knight.py bakes from the authored USD: 135 armour pieces whose every point is
/// keyed per frame (300 frames at 24 fps, 12.46 s), in metres with +Y up. The knight starts in a guarded
/// stance at the origin facing -X, raises the sword, swings, then turns and walks about 2.9 m towards -X.
///
/// Poses between cache frames interpolate linearly; across a frame's shutter the pose is keyed at every
/// motion step, so the deformation motion-blurs (via `DeformingMeshShape`). Normals are recomputed per
/// pose, smooth within each piece. Materials follow the authored UsdPreviewSurface parameters (metal armour
/// with roughness maps, textured leather, cloth, shield and sword).
module Knight =
    /// One authored material, as the converter recorded it. Map paths are relative to the cache file.
    type MaterialDescription =
        { Name: string
          BaseColour: Colour
          BaseColourMap: string option
          Metallic: float
          Roughness: float
          RoughnessMap: string option
          NormalMap: string option
          Ior: float }

    /// Triangles sharing one material. Render vertex i is point Source.[i] with texture coordinate Uvs.[i].
    type Group = { Material: int; Source: int[]; Uvs: (float * float)[]; Triangles: int[] }

    [<NoEquality; NoComparison>]
    type Cache =
        { Path: string
          Fps: float
          FrameCount: int
          PointCount: int
          /// Frame-major point positions: frame f, point p at 3 * (f * PointCount + p).
          Positions: float32[]
          Groups: Group[]
          Materials: MaterialDescription[]
          /// Each piece's first point and point count.
          Pieces: Map<string, int * int>
          MarkerNames: string[]
          /// Frame-major marker positions, like Positions.
          Markers: float32[] }
        /// Length of the animation in seconds (first to last frame).
        member c.Duration = float (c.FrameCount - 1) / c.Fps

    let private magic = "KNCACHE1"B

    /// Loads a cache written by convert-knight.py.
    let load (path: string) : Cache =
        use stream = File.OpenRead path
        use reader = new BinaryReader(stream)
        if reader.ReadBytes 8 <> magic then raise (InvalidDataException $"{path} is not a knight vertex cache.")
        let header = reader.ReadBytes(reader.ReadInt32())
        use json = JsonDocument.Parse(ReadOnlyMemory header)
        let root = json.RootElement
        let int (name: string) = root.GetProperty(name).GetInt32()
        let frames, points = int "frames", int "points"
        let optString (e: JsonElement) (name: string) =
            match e.GetProperty(name) with
            | v when v.ValueKind = JsonValueKind.String -> Some (v.GetString())
            | _ -> None
        let materials =
            [| for m in root.GetProperty("materials").EnumerateArray() ->
                let c = m.GetProperty("baseColour").EnumerateArray() |> Seq.map (fun v -> v.GetDouble()) |> Array.ofSeq
                { Name = m.GetProperty("name").GetString()
                  BaseColour = Colour(c.[0], c.[1], c.[2])
                  BaseColourMap = optString m "baseColourMap"
                  Metallic = m.GetProperty("metallic").GetDouble()
                  Roughness = m.GetProperty("roughness").GetDouble()
                  RoughnessMap = optString m "roughnessMap"
                  NormalMap = optString m "normalMap"
                  Ior = m.GetProperty("ior").GetDouble() } |]
        let readInts n = Array.init n (fun _ -> reader.ReadInt32())
        let readFloats n =
            let bytes = reader.ReadBytes(4 * n)
            if bytes.Length <> 4 * n then raise (EndOfStreamException $"{path} is truncated.")
            let values = Array.zeroCreate<float32> n
            Buffer.BlockCopy(bytes, 0, values, 0, bytes.Length)
            values
        let groups =
            [| for g in root.GetProperty("groups").EnumerateArray() ->
                let vertices, triangles = g.GetProperty("vertices").GetInt32(), g.GetProperty("triangles").GetInt32()
                let source = readInts vertices
                let uv = readFloats (2 * vertices)
                // Meshes store v up, as USD's st does.
                { Material = g.GetProperty("material").GetInt32(); Source = source
                  Uvs = Array.init vertices (fun i -> float uv.[2 * i], float uv.[2 * i + 1])
                  Triangles = readInts (3 * triangles) } |]
        let positions = readFloats (3 * frames * points)
        let markerNames = root.GetProperty("markers").EnumerateArray() |> Seq.map (fun v -> v.GetString()) |> Array.ofSeq
        let markers = readFloats (3 * frames * markerNames.Length)
        let pieces =
            root.GetProperty("pieces").EnumerateObject()
            |> Seq.map (fun p -> p.Name, (p.Value.[0].GetInt32(), p.Value.[1].GetInt32())) |> Map.ofSeq
        { Path = Path.GetFullPath path; Fps = root.GetProperty("fps").GetDouble(); FrameCount = frames; PointCount = points
          Positions = positions; Groups = groups; Materials = materials; Pieces = pieces
          MarkerNames = markerNames; Markers = markers }

    /// The cache frame segment (index, fraction) at animation time t, clamped to the first and last frame.
    let frameAt (cache: Cache) (t: float) =
        let f = if Double.IsFinite t then max 0. (min (float (cache.FrameCount - 1)) (t * cache.Fps)) else 0.
        let i = min (cache.FrameCount - 2) (int (floor f))
        struct (i, f - float i)

    let private lerpArray (data: float32[]) (stride: int) (struct (i, f): struct (int * float)) =
        let a, b = i * stride, (i + 1) * stride
        let w = float32 f
        Array.init stride (fun k -> data.[a + k] + w * (data.[b + k] - data.[a + k]))

    /// Every point's position at animation time t (x, y, z per point), linearly interpolated between frames.
    let pointsAt (cache: Cache) (t: float) = lerpArray cache.Positions (3 * cache.PointCount) (frameAt cache t)

    /// Smooth per-point normals for a pose: area-weighted face normals summed over every triangle using
    /// the point (points belong to one piece, so pieces never smooth into each other).
    let normalsOf (cache: Cache) (points: float32[]) =
        let n = Array.zeroCreate<float> (3 * cache.PointCount)
        for g in cache.Groups do
            let tri = g.Triangles
            for t in 0 .. tri.Length / 3 - 1 do
                let a, b, c = 3 * g.Source.[tri.[3 * t]], 3 * g.Source.[tri.[3 * t + 1]], 3 * g.Source.[tri.[3 * t + 2]]
                let ux, uy, uz = float (points.[b] - points.[a]), float (points.[b + 1] - points.[a + 1]), float (points.[b + 2] - points.[a + 2])
                let vx, vy, vz = float (points.[c] - points.[a]), float (points.[c + 1] - points.[a + 1]), float (points.[c + 2] - points.[a + 2])
                let nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
                for p in [| a; b; c |] do
                    n.[p] <- n.[p] + nx; n.[p + 1] <- n.[p + 1] + ny; n.[p + 2] <- n.[p + 2] + nz
        n

    /// A named marker (pelvis, head, chestFront, chestBack, swordTip) at animation time t, in the knight's
    /// own frame.
    let markerAt (cache: Cache) (name: string) (t: float) =
        match Array.IndexOf(cache.MarkerNames, name) with
        | -1 -> invalidArg (nameof name) $"""No marker {name}; the cache has {String.Join(", ", cache.MarkerNames)}."""
        | m ->
            let all = lerpArray cache.Markers (3 * cache.MarkerNames.Length) (frameAt cache t)
            Point(float all.[3 * m], float all.[3 * m + 1], float all.[3 * m + 2])

    // ------------------------------------------------------------------ materials

    /// Decodes an image into linear floats, halving it with a box filter until neither side exceeds maxSize.
    let private loadImage (path: string) (srgb: bool) (maxSize: int) =
        use rgb = RgbImage.Load path
        let lut = Array.init 256 (fun b -> let v = float b / 255. in float32 (if srgb then TextureFilter.srgbToLinear v else v))
        let mutable w, h = rgb.Width, rgb.Height
        let mutable data = rgb.Pixels |> Array.map (fun b -> lut.[int b])
        while (w > maxSize || h > maxSize) && w % 2 = 0 && h % 2 = 0 do
            let w2, h2 = w / 2, h / 2
            let src, sw = data, w
            data <- Array.init (w2 * h2 * 3) (fun i ->
                let c, p = i % 3, i / 3
                let x, y = 2 * (p % w2), 2 * (p / w2)
                let at xx yy = src.[3 * (yy * sw + xx) + c]
                0.25f * (at x y + at (x + 1) y + at x (y + 1) + at (x + 1) (y + 1)))
            w <- w2; h <- h2
        TextureFilter.create w h 3 data

    /// The renderer material description of an authored material. Maps load from beside the cache and are
    /// reduced to at most maxSize texels a side (the 4K sources cost 200 MB each as floats).
    let describe (cache: Cache) (maxSize: int) (m: MaterialDescription) : PbrParams =
        let dir = Path.GetDirectoryName cache.Path
        let sampler (relative: string) srgb =
            let image = loadImage (Path.GetFullPath(Path.Combine(dir, relative))) srgb maxSize
            // Texture space has t = 0 at the top of the image; meshes store v up.
            fun (u: float) (v: float) -> TextureFilter.bilinear image Repeat Repeat u (1. - v)
        { PbrParams.defaults with
            BaseColour = m.BaseColour
            BaseColourMap = m.BaseColourMap |> Option.map (fun p ->
                let look = sampler p true
                fun u v -> let struct (r, g, b, _) = look u v in Colour(r, g, b))
            Metallic = m.Metallic
            Roughness = (if m.RoughnessMap.IsSome then 1. else m.Roughness)
            MetallicRoughnessMap = m.RoughnessMap |> Option.map (fun p ->
                let look = sampler p false
                fun u v -> let struct (r, _, _, _) = look u v in struct (1., r))
            NormalMap = m.NormalMap |> Option.map (fun p ->
                let look = sampler p false
                fun u v -> let struct (r, g, b, _) = look u v in Vector(2. * r - 1., 2. * g - 1., 2. * b - 1.))
            Ior = m.Ior }

    let private textureOf (p: PbrParams) =
        if PbrParams.isUniform p then mkMatTexture (PbrMaterial(PbrParams.sampleAt p 0. 0.))
        else mkTexture (fun u v -> PbrMaterial(PbrParams.sampleAt p u v) :> Tracer.Basics.Material) |> markOpaque

    /// One texture per material. `edit` adjusts each material (by name) before it is built, e.g. to tune
    /// the armour's roughness; pass `fun _ p -> p` to keep the authored look.
    let textures (cache: Cache) (maxSize: int) (edit: string -> PbrParams -> PbrParams) =
        cache.Materials |> Array.map (fun m -> describe cache maxSize m |> edit m.Name |> textureOf)

    // ------------------------------------------------------------------ shapes

    /// The knight's shapes, in its own frame, posed at animation times `times` (one mesh per material).
    /// Several distinct times give deforming meshes whose rays see the pose at their shutter time, which
    /// must then equal the scene times passed as `keys` (the frame's motion-step times).
    let shapesAt (cache: Cache) (textures: Texture[]) (keys: float[]) (times: float[]) : Shape list =
        let poses = times |> Array.map (pointsAt cache)
        let normals = poses |> Array.map (normalsOf cache)
        let moving = poses.Length > 1 && poses |> Array.exists (fun p -> p <> poses.[0])
        [ for g in cache.Groups do
            let pick (pose: float32[]) = g.Source |> Array.map (fun s -> Point(float pose.[3 * s], float pose.[3 * s + 1], float pose.[3 * s + 2]))
            let pickN (n: float[]) = g.Source |> Array.map (fun s -> Vector(n.[3 * s], n.[3 * s + 1], n.[3 * s + 2]).Normalise)
            if moving then
                yield DeformingMesh.DeformingMeshShape.Create(keys, Array.map pick poses, Array.map pickN normals, g.Uvs, g.Triangles, true, textures.[g.Material])
            else
                let mesh = TriangleMesh.fromArrays (pick poses.[0]) (pickN normals.[0]) g.Uvs g.Triangles true
                yield mesh.toShape textures.[g.Material] ]

    /// Maps scene time to animation time: the animation starts at scene time `start`, plays at `speed` and
    /// holds its first and last poses outside its range.
    let clock (start: float) (speed: float) = fun (t: float) -> (t - start) * speed

    /// A node carrying the animated knight at `placement` (see `placeAt`), driven by `time` (scene time to
    /// animation time, e.g. `clock 0. 1.`).
    let node (name: string) (cache: Cache) (textures: Texture[]) (placement: Trs) (time: float -> float) : Node =
        Node.create name
        |> Node.withRest placement
        |> Node.withContent [ Deforming (fun keys -> shapesAt cache textures keys (Array.map time keys)) ]

    /// A placement putting the knight's start point (its origin, between the feet at frame 1) at world
    /// (x, y, z), turned by `yaw` radians about +Y. At yaw 0 the knight faces -X and walks off towards -X.
    let placeAt (x: float) (y: float) (z: float) (yaw: float) =
        { Trs.identity with Translation = Vector(x, y, z); Rotation = Quaternion.ofAxisAngle (Vector(0., 1., 0.)) yaw }

    /// A marker in world space for a knight at `placement` at scene time t.
    let worldMarker (cache: Cache) (placement: Trs) (time: float -> float) (name: string) (t: float) =
        transformPoint (markerAt cache name (time t), Trs.toMatrix placement)

    /// Root: the pelvis dropped to the floor, in world space.
    let rootAt cache placement time t =
        let p = worldMarker cache placement time "pelvis" t
        Point(p.X, placement.Translation.Y, p.Z)

    /// The head (helmet centre), in world space: a natural camera target.
    let headAt cache placement time t = worldMarker cache placement time "head" t

    /// The horizontal unit direction the chest faces, in world space.
    let headingAt cache placement time t =
        let front, back = worldMarker cache placement time "chestFront" t, worldMarker cache placement time "chestBack" t
        let d = front - back
        let flat = Vector(d.X, 0., d.Z)
        if flat.Magnitude > 1e-9 then flat.Normalise else Vector(-1., 0., 0.)

    /// Bounds of the pose at animation time t, in the knight's own frame.
    let boundsAt (cache: Cache) (t: float) =
        let p = pointsAt cache t
        let mutable lo = Point(infinity, infinity, infinity)
        let mutable hi = Point(-infinity, -infinity, -infinity)
        for i in 0 .. cache.PointCount - 1 do
            let q = Point(float p.[3 * i], float p.[3 * i + 1], float p.[3 * i + 2])
            lo <- lo.Lowest q
            hi <- hi.Highest q
        lo, hi
