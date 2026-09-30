// Mesh authoring toolkit.
//
// Turns a described scene into geometry the renderer can load: parametric
// primitives, transforms, and a binary PLY writer with normals and UVs.
// Self-contained so it can be `#load`ed from a throwaway script.
//
//   #load "meshkit.fsx"
//   open Meshkit
//   Mesh.sphere 48 24 |> Mesh.scaleUniform 2.0 |> Mesh.translate 0. 2. 0.
//   |> Mesh.writePly "artifacts/scene-assets/mine/ball.ply"

module Meshkit

open System
open System.IO

type V3 = { X: float; Y: float; Z: float }

module V3 =
    let create x y z = { X = x; Y = y; Z = z }
    let add a b = create (a.X + b.X) (a.Y + b.Y) (a.Z + b.Z)
    let sub a b = create (a.X - b.X) (a.Y - b.Y) (a.Z - b.Z)
    let scale k a = create (k * a.X) (k * a.Y) (k * a.Z)
    let cross a b = create (a.Y * b.Z - a.Z * b.Y) (a.Z * b.X - a.X * b.Z) (a.X * b.Y - a.Y * b.X)
    let dot a b = a.X * b.X + a.Y * b.Y + a.Z * b.Z
    let length a = sqrt (dot a a)
    let unit a = let l = length a in if l > 1e-12 then scale (1. / l) a else create 0. 1. 0.

type Mesh =
    { Positions: V3 array
      Normals: V3 array
      Uvs: (float * float) array
      Triangles: (int * int * int) array }

let private tau = 2. * Math.PI

module Mesh =

    /// Area-weighted vertex normals. Used when a primitive has no natural
    /// analytic normal, and to repair imported geometry that ships without any -
    /// without them curved surfaces render visibly faceted.
    let smoothNormals (mesh: Mesh) =
        let accumulated = Array.create mesh.Positions.Length (V3.create 0. 0. 0.)
        for (a, b, c) in mesh.Triangles do
            let pa, pb, pc = mesh.Positions.[a], mesh.Positions.[b], mesh.Positions.[c]
            // Un-normalized cross product is proportional to triangle area, so
            // large triangles contribute proportionally more.
            let weighted = V3.cross (V3.sub pb pa) (V3.sub pc pa)
            accumulated.[a] <- V3.add accumulated.[a] weighted
            accumulated.[b] <- V3.add accumulated.[b] weighted
            accumulated.[c] <- V3.add accumulated.[c] weighted
        { mesh with Normals = accumulated |> Array.map V3.unit }

    let private build positions normals uvs triangles =
        let mesh = { Positions = positions; Normals = normals; Uvs = uvs; Triangles = triangles }
        if Array.isEmpty normals then smoothNormals mesh else mesh

    // ---------------------------------------------------------- primitives

    /// UV sphere of radius 1 centred on the origin. Poles are shared vertices
    /// and the seam is duplicated so UVs stay continuous.
    let sphere sectors stacks =
        let positions = ResizeArray<V3>()
        let normals = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        for stack in 0 .. stacks do
            let v = float stack / float stacks
            let phi = v * Math.PI
            for sector in 0 .. sectors do
                let u = float sector / float sectors
                let theta = u * tau
                let p = V3.create (sin phi * cos theta) (cos phi) (sin phi * sin theta)
                positions.Add p
                normals.Add p
                uvs.Add(u, 1. - v)
        let triangles = ResizeArray<int * int * int>()
        for stack in 0 .. stacks - 1 do
            for sector in 0 .. sectors - 1 do
                let a = stack * (sectors + 1) + sector
                let b = a + sectors + 1
                if stack <> 0 then triangles.Add(a, b, a + 1)
                if stack <> stacks - 1 then triangles.Add(a + 1, b, b + 1)
        build (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) (triangles.ToArray())

    /// Axis-aligned unit box centred on the origin, with per-face normals so the
    /// edges stay sharp.
    let box () =
        let corners =
            [| -0.5, -0.5, -0.5; 0.5, -0.5, -0.5; 0.5, 0.5, -0.5; -0.5, 0.5, -0.5
               -0.5, -0.5, 0.5; 0.5, -0.5, 0.5; 0.5, 0.5, 0.5; -0.5, 0.5, 0.5 |]
            |> Array.map (fun (x, y, z) -> V3.create x y z)
        let faces =
            [| [| 0; 3; 2; 1 |], V3.create 0. 0. -1.
               [| 4; 5; 6; 7 |], V3.create 0. 0. 1.
               [| 0; 4; 7; 3 |], V3.create -1. 0. 0.
               [| 1; 2; 6; 5 |], V3.create 1. 0. 0.
               [| 0; 1; 5; 4 |], V3.create 0. -1. 0.
               [| 3; 7; 6; 2 |], V3.create 0. 1. 0. |]
        let positions = ResizeArray<V3>()
        let normals = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        let triangles = ResizeArray<int * int * int>()
        for (indices, normal) in faces do
            let start = positions.Count
            let uvCorners = [| 0., 0.; 1., 0.; 1., 1.; 0., 1. |]
            for k in 0 .. 3 do
                positions.Add corners.[indices.[k]]
                normals.Add normal
                uvs.Add uvCorners.[k]
            triangles.Add(start, start + 1, start + 2)
            triangles.Add(start, start + 2, start + 3)
        build (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) (triangles.ToArray())

    /// Closed cylinder, radius 1, height 1, centred on the origin, axis +Y.
    let cylinder sectors =
        let positions = ResizeArray<V3>()
        let normals = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        let triangles = ResizeArray<int * int * int>()
        // Side wall
        for sector in 0 .. sectors do
            let u = float sector / float sectors
            let theta = u * tau
            let nx, nz = cos theta, sin theta
            for (y, v) in [ -0.5, 0.; 0.5, 1. ] do
                positions.Add(V3.create nx y nz)
                normals.Add(V3.create nx 0. nz)
                uvs.Add(u, v)
        for sector in 0 .. sectors - 1 do
            let a = sector * 2
            triangles.Add(a, a + 1, a + 2)
            triangles.Add(a + 1, a + 3, a + 2)
        // Caps
        for (y, ny) in [ 0.5, 1.; -0.5, -1. ] do
            let centre = positions.Count
            positions.Add(V3.create 0. y 0.)
            normals.Add(V3.create 0. ny 0.)
            uvs.Add(0.5, 0.5)
            for sector in 0 .. sectors do
                let theta = float sector / float sectors * tau
                positions.Add(V3.create (cos theta) y (sin theta))
                normals.Add(V3.create 0. ny 0.)
                uvs.Add(0.5 + 0.5 * cos theta, 0.5 + 0.5 * sin theta)
            for sector in 0 .. sectors - 1 do
                if ny > 0. then triangles.Add(centre, centre + 1 + sector, centre + 2 + sector)
                else triangles.Add(centre, centre + 2 + sector, centre + 1 + sector)
        build (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) (triangles.ToArray())

    /// Torus in the XZ plane: ring radius 1, tube radius `tube`.
    let torus sectors rings (tube: float) =
        let positions = ResizeArray<V3>()
        let normals = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        for ring in 0 .. rings do
            let u = float ring / float rings
            let theta = u * tau
            let centre = V3.create (cos theta) 0. (sin theta)
            for sector in 0 .. sectors do
                let v = float sector / float sectors
                let phi = v * tau
                let radial = V3.create (cos theta * cos phi) (sin phi) (sin theta * cos phi)
                positions.Add(V3.add centre (V3.scale tube radial))
                normals.Add radial
                uvs.Add(u, v)
        let triangles = ResizeArray<int * int * int>()
        for ring in 0 .. rings - 1 do
            for sector in 0 .. sectors - 1 do
                let a = ring * (sectors + 1) + sector
                let b = a + sectors + 1
                triangles.Add(a, b, a + 1)
                triangles.Add(a + 1, b, b + 1)
        build (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) (triangles.ToArray())

    /// Flat quad in the XZ plane, one unit across, facing +Y.
    let plane subdivisions =
        let positions = ResizeArray<V3>()
        let normals = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        for row in 0 .. subdivisions do
            for column in 0 .. subdivisions do
                let u = float column / float subdivisions
                let v = float row / float subdivisions
                positions.Add(V3.create (u - 0.5) 0. (v - 0.5))
                normals.Add(V3.create 0. 1. 0.)
                uvs.Add(u, v)
        let triangles = ResizeArray<int * int * int>()
        let stride = subdivisions + 1
        for row in 0 .. subdivisions - 1 do
            for column in 0 .. subdivisions - 1 do
                let a = row * stride + column
                triangles.Add(a, a + stride, a + 1)
                triangles.Add(a + 1, a + stride, a + stride + 1)
        build (positions.ToArray()) (normals.ToArray()) (uvs.ToArray()) (triangles.ToArray())

    /// Surface of revolution about +Y from a profile of (radius, height) pairs.
    /// Covers vases, goblets, bowls, columns, bottles - most turned shapes.
    let lathe sectors (profile: (float * float) array) =
        let positions = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        for index in 0 .. profile.Length - 1 do
            let radius, height = profile.[index]
            let v = float index / float (max 1 (profile.Length - 1))
            for sector in 0 .. sectors do
                let u = float sector / float sectors
                let theta = u * tau
                positions.Add(V3.create (radius * cos theta) height (radius * sin theta))
                uvs.Add(u, v)
        let triangles = ResizeArray<int * int * int>()
        for index in 0 .. profile.Length - 2 do
            for sector in 0 .. sectors - 1 do
                let a = index * (sectors + 1) + sector
                let b = a + sectors + 1
                triangles.Add(a, b, a + 1)
                triangles.Add(a + 1, b, b + 1)
        build (positions.ToArray()) [||] (uvs.ToArray()) (triangles.ToArray())

    // ---------------------------------------------------------- transforms

    let map f (mesh: Mesh) = { mesh with Positions = Array.map f mesh.Positions }

    let translate x y z = map (fun p -> V3.add p (V3.create x y z))
    let scaleUniform k = map (V3.scale k)
    let scaleXyz x y z = map (fun p -> V3.create (p.X * x) (p.Y * y) (p.Z * z))

    let rotateY radians (mesh: Mesh) =
        let c, s = cos radians, sin radians
        let rotate (p: V3) = V3.create (p.X * c + p.Z * s) p.Y (-p.X * s + p.Z * c)
        { mesh with Positions = Array.map rotate mesh.Positions
                    Normals = Array.map rotate mesh.Normals }

    let rotateX radians (mesh: Mesh) =
        let c, s = cos radians, sin radians
        let rotate (p: V3) = V3.create p.X (p.Y * c - p.Z * s) (p.Y * s + p.Z * c)
        { mesh with Positions = Array.map rotate mesh.Positions
                    Normals = Array.map rotate mesh.Normals }

    /// Concatenate meshes, re-indexing triangles.
    let merge (meshes: Mesh seq) =
        let positions = ResizeArray<V3>()
        let normals = ResizeArray<V3>()
        let uvs = ResizeArray<float * float>()
        let triangles = ResizeArray<int * int * int>()
        for mesh in meshes do
            let offset = positions.Count
            positions.AddRange mesh.Positions
            normals.AddRange mesh.Normals
            uvs.AddRange mesh.Uvs
            for (a, b, c) in mesh.Triangles do triangles.Add(a + offset, b + offset, c + offset)
        { Positions = positions.ToArray(); Normals = normals.ToArray()
          Uvs = uvs.ToArray(); Triangles = triangles.ToArray() }

    // ---------------------------------------------------------- output

    let bounds (mesh: Mesh) =
        let mutable lo = V3.create infinity infinity infinity
        let mutable hi = V3.create -infinity -infinity -infinity
        for p in mesh.Positions do
            lo <- V3.create (min lo.X p.X) (min lo.Y p.Y) (min lo.Z p.Z)
            hi <- V3.create (max hi.X p.X) (max hi.Y p.Y) (max hi.Z p.Z)
        lo, hi

    /// Binary little-endian PLY with positions, normals and UVs - exactly the
    /// layout the project's parser expects.
    let writePly (path: string) (mesh: Mesh) =
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path)) |> ignore
        if mesh.Positions.Length = 0 || mesh.Triangles.Length = 0 then
            failwithf "Mesh for %s is empty." path
        if mesh.Normals.Length <> mesh.Positions.Length || mesh.Uvs.Length <> mesh.Positions.Length then
            failwithf "Mesh for %s has mismatched vertex attribute counts." path
        for p in mesh.Positions do
            if not (Double.IsFinite p.X && Double.IsFinite p.Y && Double.IsFinite p.Z) then
                failwithf "Mesh for %s has a non-finite vertex." path
        use stream = new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None, 1 <<< 20)
        use writer = new BinaryWriter(stream)
        let header =
            "ply\nformat binary_little_endian 1.0\n"
            + $"element vertex {mesh.Positions.Length}\n"
            + "property float x\nproperty float y\nproperty float z\n"
            + "property float nx\nproperty float ny\nproperty float nz\n"
            + "property float u\nproperty float v\n"
            + $"element face {mesh.Triangles.Length}\n"
            + "property list uchar int vertex_indices\nend_header\n"
        writer.Write(Text.Encoding.ASCII.GetBytes header)
        for i in 0 .. mesh.Positions.Length - 1 do
            let p, n = mesh.Positions.[i], mesh.Normals.[i]
            let u, v = mesh.Uvs.[i]
            for value in [| p.X; p.Y; p.Z; n.X; n.Y; n.Z; u; v |] do writer.Write(float32 value)
        for (a, b, c) in mesh.Triangles do
            writer.Write 3uy
            writer.Write a
            writer.Write b
            writer.Write c
        let lo, hi = bounds mesh
        printfn "  %-46s %7d tris  bounds (%.2f %.2f %.2f)-(%.2f %.2f %.2f)"
            (Path.GetFileName path) mesh.Triangles.Length lo.X lo.Y lo.Z hi.X hi.Y hi.Z
