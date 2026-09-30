// Converts a Wavefront OBJ into one binary PLY per material group, plus a
// SceneFormat scene that references them.
//
// The renderer reads PLY only, while the standard research scenes (San Miguel,
// Sponza, Rungholt) ship as OBJ. Splitting by material is what lets the scene
// format address them: it has one material per object, so each `usemtl` group
// becomes its own mesh and object.
//
// Usage: dotnet fsi scripts/obj-to-ply.fsx -- <model.obj> <outputDir> <sceneId>

#r "../RayTracer/bin/Release/net10.0/Basics.dll"
#load "../SceneFormat/SceneFormat.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open Tracer.SceneFormat

// Matches the convention in scripts/benchmark.fsx.
let args = fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--")
if args.Length < 3 then
    eprintfn "Usage: dotnet fsi scripts/obj-to-ply.fsx -- <model.obj> <outputDir> <sceneId>"
    exit 2

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let objPath = Path.GetFullPath args.[0]
let outputDir = Path.GetFullPath args.[1]
let sceneId = args.[2]
Directory.CreateDirectory outputDir |> ignore

let inline parseFloat (s: ReadOnlySpan<char>) =
    Double.Parse(s, NumberStyles.Float, CultureInfo.InvariantCulture)

/// OBJ indices are 1-based and may be negative (relative to the end).
let inline resolve (index: int) (count: int) = if index < 0 then count + index else index - 1

// ---------------------------------------------------------------- MTL

type MaterialInfo =
    { mutable Diffuse: float array
      mutable Specular: float array
      mutable Shininess: float
      mutable Dissolve: float
      mutable HasAlphaMap: bool
      /// map_Kd, as an OBJ-relative path. The scene format carries one baked
      /// RGB texture per material, which is exactly what this maps onto.
      mutable DiffuseMap: string }

let materials = Dictionary<string, MaterialInfo>(StringComparer.Ordinal)

let readMtl (path: string) =
    if File.Exists path then
        let mutable current = Unchecked.defaultof<MaterialInfo>
        for raw in File.ReadLines path do
            let line = raw.Trim()
            if line.Length > 0 && line.[0] <> '#' then
                let parts = line.Split((null: char array), StringSplitOptions.RemoveEmptyEntries)
                match parts.[0] with
                | "newmtl" when parts.Length > 1 ->
                    current <- { Diffuse = [| 0.7; 0.7; 0.7 |]; Specular = [| 0.; 0.; 0. |]
                                 Shininess = 0.; Dissolve = 1.; HasAlphaMap = false; DiffuseMap = "" }
                    materials.[parts.[1]] <- current
                | "Kd" when parts.Length > 3 && not (obj.ReferenceEquals(current, null)) ->
                    current.Diffuse <- [| parseFloat (parts.[1].AsSpan()); parseFloat (parts.[2].AsSpan()); parseFloat (parts.[3].AsSpan()) |]
                | "Ks" when parts.Length > 3 && not (obj.ReferenceEquals(current, null)) ->
                    current.Specular <- [| parseFloat (parts.[1].AsSpan()); parseFloat (parts.[2].AsSpan()); parseFloat (parts.[3].AsSpan()) |]
                | "Ns" when parts.Length > 1 && not (obj.ReferenceEquals(current, null)) ->
                    current.Shininess <- parseFloat (parts.[1].AsSpan())
                | "d" when parts.Length > 1 && not (obj.ReferenceEquals(current, null)) ->
                    current.Dissolve <- parseFloat (parts.[1].AsSpan())
                | ("map_Kd" | "map_kd") when parts.Length > 1 && not (obj.ReferenceEquals(current, null)) ->
                    // MTL files commonly use Windows separators.
                    current.DiffuseMap <- parts.[parts.Length - 1].Replace('\\', '/')
                | ("map_d" | "map_D" | "map_bump") when not (obj.ReferenceEquals(current, null)) ->
                    // Alpha-mapped materials are cut-outs (foliage, grilles). The
                    // renderer has no alpha test, so these would render as solid
                    // quads. Flagged so the caller can decide.
                    current.HasAlphaMap <- true
                | _ -> ()

// ---------------------------------------------------------------- OBJ

let positions = List<float32>()
let normals = List<float32>()
let texcoords = List<float32>()

/// Face corners for one material group, as (position, normal, texcoord) indices.
type Group() =
    member val Corners = List<struct (int * int * int)>() with get
    member val Counts = List<int>() with get

let groups = Dictionary<string, Group>(StringComparer.Ordinal)
let mutable currentName = "default"
let mutable current = Group()
groups.["default"] <- current

let clock = Stopwatch.StartNew()
let mutable faceCount = 0

for raw in File.ReadLines objPath do
    let span = raw.AsSpan().Trim()
    if span.Length > 0 && span.[0] <> '#' then
        if span.StartsWith "v " then
            let mutable rest = span.Slice 2
            let mutable n = 0
            while n < 3 && rest.Length > 0 do
                rest <- rest.TrimStart()
                let stop = rest.IndexOf ' '
                let token = if stop < 0 then rest else rest.Slice(0, stop)
                positions.Add(float32 (parseFloat token))
                rest <- if stop < 0 then ReadOnlySpan<char>.Empty else rest.Slice stop
                n <- n + 1
        elif span.StartsWith "vn " then
            let mutable rest = span.Slice 3
            let mutable n = 0
            while n < 3 && rest.Length > 0 do
                rest <- rest.TrimStart()
                let stop = rest.IndexOf ' '
                let token = if stop < 0 then rest else rest.Slice(0, stop)
                normals.Add(float32 (parseFloat token))
                rest <- if stop < 0 then ReadOnlySpan<char>.Empty else rest.Slice stop
                n <- n + 1
        elif span.StartsWith "vt " then
            let mutable rest = span.Slice 3
            let mutable n = 0
            while n < 2 && rest.Length > 0 do
                rest <- rest.TrimStart()
                let stop = rest.IndexOf ' '
                let token = if stop < 0 then rest else rest.Slice(0, stop)
                texcoords.Add(float32 (parseFloat token))
                rest <- if stop < 0 then ReadOnlySpan<char>.Empty else rest.Slice stop
                n <- n + 1
        elif span.StartsWith "usemtl " then
            currentName <- span.Slice(7).Trim().ToString()
            match groups.TryGetValue currentName with
            | true, g -> current <- g
            | _ ->
                let g = Group()
                groups.[currentName] <- g
                current <- g
        elif span.StartsWith "mtllib " then
            let name = span.Slice(7).Trim().ToString()
            readMtl (Path.Combine(Path.GetDirectoryName objPath, name))
        elif span.StartsWith "f " then
            let text = span.Slice(2).ToString()
            let corners = text.Split((null: char array), StringSplitOptions.RemoveEmptyEntries)
            let vertexCount = positions.Count / 3
            let normalCount = normals.Count / 3
            let texCount = texcoords.Count / 2
            let mutable added = 0
            for corner in corners do
                let fields = corner.Split '/'
                let p = resolve (Int32.Parse(fields.[0], CultureInfo.InvariantCulture)) vertexCount
                let t =
                    if fields.Length > 1 && fields.[1] <> "" then resolve (Int32.Parse(fields.[1], CultureInfo.InvariantCulture)) texCount
                    else -1
                let n =
                    if fields.Length > 2 && fields.[2] <> "" then resolve (Int32.Parse(fields.[2], CultureInfo.InvariantCulture)) normalCount
                    else -1
                current.Corners.Add(struct (p, n, t))
                added <- added + 1
            current.Counts.Add added
            faceCount <- faceCount + 1

printfn "parsed %s in %.1fs: %d positions, %d normals, %d texcoords, %d faces, %d material groups"
    (Path.GetFileName objPath) clock.Elapsed.TotalSeconds (positions.Count / 3) (normals.Count / 3)
    (texcoords.Count / 2) faceCount groups.Count

// ---------------------------------------------------------------- PLY out

/// Writes one group as a binary little-endian PLY, re-indexing to only the
/// vertices that group actually uses. Polygons are fan-triangulated.
let writeGroup (name: string) (group: Group) =
    if group.Counts.Count = 0 then None
    else
        let hasNormals = group.Corners |> Seq.forall (fun (struct (_, n, _)) -> n >= 0)
        let hasUv = group.Corners |> Seq.forall (fun (struct (_, _, t)) -> t >= 0)
        let remap = Dictionary<struct (int * int * int), int>()
        let ordered = List<struct (int * int * int)>()
        let index corner =
            match remap.TryGetValue corner with
            | true, i -> i
            | _ ->
                let i = ordered.Count
                remap.[corner] <- i
                ordered.Add corner
                i
        let triangles = List<int>()
        let mutable offset = 0
        for count in group.Counts do
            let first = index group.Corners.[offset]
            for k in 1 .. count - 2 do
                triangles.Add first
                triangles.Add(index group.Corners.[offset + k])
                triangles.Add(index group.Corners.[offset + k + 1])
            offset <- offset + count
        let safe = String(name.ToCharArray() |> Array.map (fun c -> if Char.IsLetterOrDigit c || c = '-' || c = '_' then c else '-'))
        let file = Path.Combine(outputDir, safe + ".ply")
        use stream = new FileStream(file, FileMode.Create, FileAccess.Write, FileShare.None, 1 <<< 20)
        use writer = new BinaryWriter(stream)
        let header =
            let sb = Text.StringBuilder()
            sb.Append("ply\nformat binary_little_endian 1.0\n") |> ignore
            sb.Append($"element vertex {ordered.Count}\n") |> ignore
            sb.Append("property float x\nproperty float y\nproperty float z\n") |> ignore
            if hasNormals then sb.Append("property float nx\nproperty float ny\nproperty float nz\n") |> ignore
            if hasUv then sb.Append("property float u\nproperty float v\n") |> ignore
            sb.Append($"element face {triangles.Count / 3}\n") |> ignore
            sb.Append("property list uchar int vertex_indices\nend_header\n") |> ignore
            sb.ToString()
        writer.Write(Text.Encoding.ASCII.GetBytes header)
        for (struct (p, n, t)) in ordered do
            writer.Write positions.[p * 3]
            writer.Write positions.[p * 3 + 1]
            writer.Write positions.[p * 3 + 2]
            if hasNormals then
                writer.Write normals.[n * 3]
                writer.Write normals.[n * 3 + 1]
                writer.Write normals.[n * 3 + 2]
            if hasUv then
                writer.Write texcoords.[t * 2]
                writer.Write texcoords.[t * 2 + 1]
        for i in 0 .. 3 .. triangles.Count - 3 do
            writer.Write 3uy
            writer.Write triangles.[i]
            writer.Write triangles.[i + 1]
            writer.Write triangles.[i + 2]
        Some(safe, triangles.Count / 3, hasNormals)

let written = ResizeArray<string * int * bool * string>()
for KeyValue(name, group) in groups do
    match writeGroup name group with
    | Some(file, tris, smooth) when tris > 0 -> written.Add(file, tris, smooth, name)
    | _ -> ()

let total = written |> Seq.sumBy (fun (_, t, _, _) -> t)
printfn "wrote %d meshes, %s triangles, to %s" written.Count (total.ToString("N0")) outputDir

// ---------------------------------------------------------------- scene

let colour (v: float array) : float array = [| max 0. (min 1. v.[0]); max 0. (min 1. v.[1]); max 0. (min 1. v.[2]) |]

let sceneMaterials =
    written
    |> Seq.map (fun (file, _, _, name) ->
        let info =
            match materials.TryGetValue name with
            | true, m -> m
            | _ -> { Diffuse = [| 0.7; 0.7; 0.7 |]; Specular = [| 0.; 0.; 0. |]; Shininess = 0.; Dissolve = 1.; HasAlphaMap = false; DiffuseMap = "" }
        let texture =
            if info.DiffuseMap = "" then ""
            else
                let full = Path.Combine(Path.GetDirectoryName objPath, info.DiffuseMap)
                if File.Exists full then
                    Path.GetRelativePath(Path.Combine(root, "benchmarks/scenes"), full).Replace('\\', '/')
                else ""
        let specularStrength = (info.Specular.[0] + info.Specular.[1] + info.Specular.[2]) / 3.
        let kind = if specularStrength > 0.05 then MaterialKind.Phong else MaterialKind.Matte
        { Id = file; Kind = kind; Colour = colour info.Diffuse
          AmbientColour = null; SpecularColour = null; ReflectionColour = null
          // Ambient is a per-material coefficient multiplied by the scene
          // ambient. The Whitted GPU path has no indirect bounce, so this term
          // is the only fill a shaded interior receives; 0.04 leaves arcades
          // black.
          Ambient = 0.25; Diffuse = 0.85; Specular = min 0.6 specularStrength
          Exponent = max 1 (int info.Shininess); Reflectivity = 0.; GlossExponent = 0
          Ior = 1.5; Filter = [| 1.; 1.; 1. |]; Emission = 0.; Texture = texture })
    |> Seq.toArray

let identity : float array = [| 1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.; 0.;0.;0.;1. |]
let relative = Path.GetRelativePath(Path.Combine(root, "benchmarks/scenes"), outputDir).Replace('\\', '/')

let sceneMeshes =
    written |> Seq.map (fun (file, _, smooth, _) ->
        { Id = file; Path = relative + "/" + file + ".ply"; Smooth = smooth; Closed = false }) |> Seq.toArray

let sceneObjects =
    written |> Seq.map (fun (file, _, _, _) ->
        { Id = file; Mesh = file; Material = file; Transform = identity }) |> Seq.toArray

let alphaMapped =
    written |> Seq.filter (fun (_, _, _, name) ->
        match materials.TryGetValue name with true, m -> m.HasAlphaMap | _ -> false) |> Seq.length

// Frame the model automatically.
//
// Without this, converting a downloaded model gives a camera at a fixed
// position that has nothing to do with the model's scale or location - a
// Stanford scan is 0.2 units across, San Miguel is 69 - so the first render is
// almost always black or empty, and finding a viewpoint means several rounds of
// guessing. Deriving it from the actual bounds makes the conversion produce
// something you can render immediately.
let sceneBounds =
    let mutable lo = (infinity, infinity, infinity)
    let mutable hi = (-infinity, -infinity, -infinity)
    for i in 0 .. positions.Count / 3 - 1 do
        let x, y, z = float positions.[i * 3], float positions.[i * 3 + 1], float positions.[i * 3 + 2]
        let (lx, ly, lz) = lo
        let (hx, hy, hz) = hi
        lo <- (min lx x, min ly y, min lz z)
        hi <- (max hx x, max hy y, max hz z)
    lo, hi

let autoCamera =
    let (lx, ly, lz), (hx, hy, hz) = sceneBounds
    let cx, cy, cz = (lx + hx) / 2., (ly + hy) / 2., (lz + hz) / 2.
    let extent = max (hx - lx) (max (hy - ly) (hz - lz))
    let radius = max 1e-6 (extent / 2.)
    let halfFov = 0.42
    // Back off far enough for the bounding sphere to fit the frame, with margin.
    let distance = radius / tan halfFov * 1.6
    // Three-quarter view from slightly above, which reads better than head-on.
    let dirX, dirY, dirZ = 0.62, 0.42, 1.0
    let length = sqrt (dirX * dirX + dirY * dirY + dirZ * dirZ)
    { Position = [| cx + dirX / length * distance; cy + dirY / length * distance; cz + dirZ / length * distance |]
      Target = [| cx; cy; cz |]
      Up = [| 0.; 1.; 0. |]
      ViewDistance = 1.; ViewWidth = 2. * tan halfFov; ViewHeight = 2. * tan halfFov * 0.5625
      LensRadius = 0.; FocusDistance = distance }

let scene =
    { SchemaVersion = 1; Id = sceneId
      Title = sceneId + " - converted from OBJ"
      Description =
        sprintf "Converted from %s by scripts/obj-to-ply.fsx: %s triangles across %d material groups. \
                 Materials carry OBJ diffuse and specular; textures are not transferred. %d groups use an \
                 alpha map (cut-out foliage or grilles) which this renderer has no alpha test for, so those \
                 surfaces render as solid geometry."
                (Path.GetFileName objPath) (total.ToString("N0")) written.Count alphaMapped
      Camera = autoCamera
      AmbientColour = [| 0.5; 0.55; 0.65 |]; AmbientIntensity = 0.05; MaxBounces = 4
      Materials = sceneMaterials; Meshes = sceneMeshes; Objects = sceneObjects
      Lights =
        [| { Id = "sun"; Kind = LightKind.Directional; Position = [| 0.; 0.; 0. |]
             // Points AT the sun: GetDirectionFromPoint returns the direction
             // from the surface toward the light, so a downward vector puts the
             // sun underground and renders everything black.
             Direction = [| 0.35; 1.0; 0.28 |]; Colour = [| 1.; 0.95; 0.85 |]; Intensity = 3.0; Size = [| 1.; 1. |] }
           { Id = "sky"; Kind = LightKind.Environment; Position = [| 0.; 0.; 0. |]
             Direction = [| 0.; 1.; 0. |]; Colour = [| 0.45; 0.55; 0.75 |]; Intensity = 0.5; Size = [| 1000000.; 1000000. |] } |]
      Subjects = written |> Seq.map (fun (f, _, _, _) -> f) |> Seq.toArray }

let scenePath = Path.Combine(root, "benchmarks/scenes", sceneId + ".json")
SceneFiles.save scenePath scene
let textured = sceneMaterials |> Array.filter (fun m -> m.Texture <> "") |> Array.length
printfn "wrote %s (%d textured materials, %d alpha-mapped groups render as solid)" scenePath textured alphaMapped
let (blx, bly, blz), (bhx, bhy, bhz) = sceneBounds
printfn "model bounds: (%.2f %.2f %.2f) to (%.2f %.2f %.2f)" blx bly blz bhx bhy bhz
printfn "camera framed automatically at (%.2f %.2f %.2f) looking at the centre"
    autoCamera.Position.[0] autoCamera.Position.[1] autoCamera.Position.[2]
printfn ""
printfn "render it with:"
printfn "  dotnet BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll \\"
printfn "    --scene benchmarks/scenes/%s.json --material authored --settings <settings.json> \\" sceneId
printfn "    --output out.png --linear out.pfm --metrics out.json \\"
printfn "    --acceleration bvh --integrator path --denoise on --exposure 4" 
