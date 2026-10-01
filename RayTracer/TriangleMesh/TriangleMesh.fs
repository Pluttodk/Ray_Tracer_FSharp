module Tracer.Basics.TriangleMesh

open System
open System.IO
open Tracer.Basics
open Tracer.BaseShape
open Tracer.Basics.Textures
open PLYParser

// Anchoring preserves constant UVs exactly, including at nearest-texel boundaries.
let inline private interpolateUv (a: float) b c beta gamma =
    a + beta * (b - a) + gamma * (c - a)

type TriPoint(vertex: Vertex) =
    inherit Point(vertex.x, vertex.y, vertex.z)
    member _.v = vertex

type PLYTriangle(a: Point, b: Point, c: Point, texture: Texture, smoothen: bool,
                 hasNormalWithin: bool, hasTextureCoords: bool) =
    inherit Triangle(a, b, c, Material.None)
    override _.IsOpaque = Textures.isOpaque texture
    override this.hitFunction ray =
        let hit = base.hitFunction ray
        if not hit.DidHit then hit
        else
            let alpha, beta, gamma = hit.BarycentricAlpha, hit.BarycentricBeta, hit.BarycentricGamma
            let normal (point: Point) =
                let vertex = (point :?> TriPoint).v
                if hasNormalWithin then
                    match vertex.nx, vertex.ny, vertex.nz with
                    | Some x, Some y, Some z -> Vector(x,y,z).Normalise
                    | _ -> invalidOp "PLY triangle is missing a declared vertex normal."
                else vertex.normal
            let shading =
                if smoothen then alpha * normal a + beta * normal b + gamma * normal c
                else hit.GeometricNormal
            let u, v =
                if hasTextureCoords then
                    let a, b, c = (a :?> TriPoint).v, (b :?> TriPoint).v, (c :?> TriPoint).v
                    interpolateUv a.u.Value b.u.Value c.u.Value beta gamma,
                    interpolateUv a.v.Value b.v.Value c.v.Value beta gamma
                else 0., 0.
            HitPoint(ray, hit.Time, hit.GeometricNormal, shading, getFunc texture u v,
                     this, u, v, beta, gamma, true)

type BasePLYTriangle(a: Point, b: Point, c: Point, smoothen: bool,
                     hasNormalWithin: bool, hasTextureCoords: bool) =
    inherit BaseTriangle(a,b,c)
    let bounds = Geometry.triangleBounds a b c
    member _.BBox = bounds
    override _.toShape texture = PLYTriangle(a,b,c,texture,smoothen,hasNormalWithin,hasTextureCoords) :> Shape

[<Struct; System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>]
type TriangleIndices = { A: int; B: int; C: int }

let private invalidFace index message =
    raise (InvalidDataException(sprintf "PLY face %d: %s" index message))

let private triangulate (vertices: Vertex array) (faces: int array array) =
    let triangles = ResizeArray<TriangleIndices>()
    let polygon faceIndex (face: int array) =
        if (Array.distinct face).Length <> face.Length then
            invalidFace faceIndex "repeated vertex indices are not supported."
        let points = face |> Array.map (fun index -> let v = vertices.[index] in Point(v.x,v.y,v.z))
        let mutable areaNormal = Vector.Zero
        for i = 1 to points.Length-2 do
            areaNormal <- areaNormal + ((points.[i] - points.[0]) % (points.[i+1] - points.[0]))
        if not areaNormal.IsFinite || areaNormal = Vector.Zero then
            invalidFace faceIndex "a degenerate polygon has no well-defined surface."
        let normal = areaNormal.Normalise
        let extent = points |> Array.fold (fun extent point -> max extent (point - points.[0]).Magnitude) 0.
        let planeTolerance = 1e-8 * extent
        if points |> Array.exists (fun point -> abs ((point - points.[0]) * normal) > planeTolerance) then
            invalidFace faceIndex "non-planar polygons are not supported; triangulate the source explicitly."
        let project (p: Point) =
            if abs normal.X >= abs normal.Y && abs normal.X >= abs normal.Z then struct (p.Y, p.Z)
            elif abs normal.Y >= abs normal.Z then struct (p.X, p.Z)
            else struct (p.X, p.Y)
        let projected = points |> Array.map project
        let cross (struct (ax,ay)) (struct (bx,by)) (struct (cx,cy)) =
            (bx-ax)*(cy-ay) - (by-ay)*(cx-ax)
        let tolerance = 1e-12 * extent * extent
        let mutable winding = 0.
        for i = 0 to face.Length-1 do
            let turn = cross projected.[i] projected.[(i+1) % face.Length] projected.[(i+2) % face.Length]
            if abs turn > tolerance then
                if winding = 0. then winding <- turn
                elif winding * turn < 0. then
                    invalidFace faceIndex "concave polygons are not supported; triangulate the source explicitly."
        let onSegment (struct (ax,ay)) (struct (bx,by)) (struct (px,py)) =
            px >= min ax bx && px <= max ax bx && py >= min ay by && py <= max ay by
        let intersects a b c d =
            let abC, abD, cdA, cdB = cross a b c, cross a b d, cross c d a, cross c d b
            (abC*abD < 0. && cdA*cdB < 0.)
            || (abs abC <= tolerance && onSegment a b c)
            || (abs abD <= tolerance && onSegment a b d)
            || (abs cdA <= tolerance && onSegment c d a)
            || (abs cdB <= tolerance && onSegment c d b)
        for i = 0 to face.Length-1 do
            for j = i+1 to face.Length-1 do
                if j <> i+1 && not (i = 0 && j = face.Length-1)
                   && intersects projected.[i] projected.[(i+1) % face.Length]
                                 projected.[j] projected.[(j+1) % face.Length] then
                    invalidFace faceIndex "self-intersecting polygons are not supported."
        for i = 1 to face.Length-2 do
            if cross projected.[0] projected.[i] projected.[i+1] <> 0. then
                triangles.Add { A = face.[0]; B = face.[i]; C = face.[i+1] }
    for faceIndex = 0 to faces.Length-1 do
        let face = faces.[faceIndex]
        if face.Length < 3 then invalidFace faceIndex "at least three vertices are required."
        if face |> Array.exists (fun index -> index < 0 || index >= vertices.Length) then
            invalidFace faceIndex "a vertex index is out of range."
        if face.Length = 3 then
            // Keep zero-area source triangles as nonintersecting primitives, as older PLY models expect.
            triangles.Add { A = face.[0]; B = face.[1]; C = face.[2] }
        else polygon faceIndex face
    triangles.ToArray()

let private vertexNormals (vertices: Vertex array) (triangles: TriangleIndices array) =
    let normals = Array.create vertices.Length Vector.Zero
    for triangle in triangles do
        let a, b, c = vertices.[triangle.A], vertices.[triangle.B], vertices.[triangle.C]
        let ab, ac = Vector(b.x-a.x, b.y-a.y, b.z-a.z), Vector(c.x-a.x, c.y-a.y, c.z-a.z)
        let normal = ab % ac
        normals.[triangle.A] <- normals.[triangle.A] + normal
        normals.[triangle.B] <- normals.[triangle.B] + normal
        normals.[triangle.C] <- normals.[triangle.C] + normal
    normals |> Array.map (fun n -> n.Normalise)

let createTriangles (vertices: Vertex array) (faces: int list array)
                    (smooth: bool) (hasNormalWithin: bool) (hasTextureCoords: bool) =
    let indices =
        faces |> Array.mapi (fun index face ->
            match face with
            | count::indices when count = indices.Length -> Array.ofList indices
            | _ -> invalidFace index "the declared list length does not match the face.")
        |> triangulate vertices
    let vertices =
        if smooth && not hasNormalWithin then
            let normals = vertexNormals vertices indices
            vertices |> Array.mapi (fun i v -> Vertex(v.x,v.y,v.z,v.nx,v.ny,v.nz,v.u,v.v,normals.[i]))
        else Array.copy vertices
    let triangles =
        indices |> Array.map (fun triangle ->
            BasePLYTriangle(TriPoint(vertices.[triangle.A]), TriPoint(vertices.[triangle.B]),
                            TriPoint(vertices.[triangle.C]), smooth, hasNormalWithin, hasTextureCoords))
    let low, high =
        triangles |> Array.fold (fun ((low: Point), (high: Point)) triangle ->
            low.Lowest triangle.BBox.lowPoint, high.Highest triangle.BBox.highPoint)
            (Point(infinity,infinity,infinity), Point(-infinity,-infinity,-infinity))
    triangles, BBox(low, high)

[<Struct; System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>]
type MeshVertex =
    { X: float; Y: float; Z: float
      Nx: float; Ny: float; Nz: float
      U: float; V: float }

/// Detached object-space FP64 mesh data. BLAS primitive indices address the Triangles array.
[<Sealed>]
type MeshExportData internal (vertices: MeshVertex array, triangles: TriangleIndices array,
                              smooth: bool, bounds: BBox, degenerateCount: int, blas: FlatBVH.ExportData) =
    member _.Vertices = ReadOnlyMemory<MeshVertex>(vertices)
    member _.Triangles = ReadOnlyMemory<TriangleIndices>(triangles)
    member _.VertexCount = vertices.Length
    member _.TriangleCount = triangles.Length
    member _.SmoothShading = smooth
    member _.Bounds = bounds
    member _.DegenerateTriangleCount = degenerateCount
    member _.Blas = blas

type private TrianglePrimitive(vertices: MeshVertex array, indices: TriangleIndices, index: int) =
    inherit Shape()
    let normal, bounds =
        let a, b, c = vertices.[indices.A], vertices.[indices.B], vertices.[indices.C]
        (Vector(b.X-a.X,b.Y-a.Y,b.Z-a.Z) % Vector(c.X-a.X,c.Y-a.Y,c.Z-a.Z)).Normalise,
        Geometry.paddedBounds (Point(min a.X (min b.X c.X), min a.Y (min b.Y c.Y), min a.Z (min b.Z c.Z)))
                              (Point(max a.X (max b.X c.X), max a.Y (max b.Y c.Y), max a.Z (max b.Z c.Z)))
    let material = Material.None
    let nondegenerate = normal.IsFinite && (normal.X <> 0. || normal.Y <> 0. || normal.Z <> 0.)
    member _.Index = index
    member _.IsDegenerate = not nondegenerate
    // This BLAS has no materials; MeshShape supplies the opacity of each materialized instance.
    override _.IsOpaque = true
    override _.getBoundingBox() = bounds
    override _.isInside _ = false
    override this.hitFunction ray =
        let a, b, c = vertices.[indices.A], vertices.[indices.B], vertices.[indices.C]
        match (if nondegenerate then Geometry.intersectTriangleCoordinates ray a.X a.Y a.Z b.X b.Y b.Z c.X c.Y c.Z else ValueNone) with
        | ValueNone -> HitPoint(ray)
        | ValueSome(struct (time, beta, gamma)) ->
            HitPoint(ray, time, normal, normal, material, this, 0., 0., beta, gamma, true)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.hitFunction ray |> Geometry.within minimum maximum

type MeshGeometry internal (inputVertices: Vertex array, inputFaces: int array array, smooth: bool) =
    let indices = triangulate inputVertices inputFaces
    let normals = if smooth then vertexNormals inputVertices indices else [||]
    let vertices =
        inputVertices |> Array.mapi (fun i vertex ->
            let normal =
                if not smooth then Vector.Zero
                elif vertex.normal <> Vector.Zero then vertex.normal.Normalise
                else normals.[i]
            { X = vertex.x; Y = vertex.y; Z = vertex.z
              Nx = normal.X; Ny = normal.Y; Nz = normal.Z
              U = defaultArg vertex.u 0.; V = defaultArg vertex.v 0. })
    let primitives =
        indices |> Array.mapi (fun index triangle -> TrianglePrimitive(vertices, triangle, index) :> Shape)
    let degenerateCount =
        primitives |> Array.sumBy (fun primitive -> if (primitive :?> TrianglePrimitive).IsDegenerate then 1 else 0)
    let accelerator = Acceleration.buildWith Acceleration.FlatBVH primitives
    let bounds =
        let low, high =
            primitives |> Array.fold (fun ((low: Point), (high: Point)) shape ->
                let box = shape.getBoundingBox()
                low.Lowest box.lowPoint, high.Highest box.highPoint)
                (Point(infinity,infinity,infinity), Point(-infinity,-infinity,-infinity))
        BBox(low, high)
    member _.VertexCount = vertices.Length
    member _.TriangleCount = indices.Length
    member _.DegenerateTriangleCount = degenerateCount
    member _.Bounds = bounds
    member _.AccelerationKind = accelerator.Kind
    /// Copies prepared geometry and the existing BLAS without rebuilding or exposing CPU-owned arrays.
    member _.Export() =
        match Acceleration.tryExportFlat accelerator with
        | Some blas -> MeshExportData(Array.copy vertices, Array.copy indices, smooth, bounds, degenerateCount, blas)
        | None -> invalidOp "Prepared mesh geometry requires a flat BLAS for export."
    member internal _.Intersect(ray: Ray, minimum: float, maximum: float) =
        Acceleration.traverseClosest accelerator ray minimum maximum
    member internal this.Intersect(ray: Ray) = this.Intersect(ray, 0., infinity)
    member internal _.Shade(hit: HitPoint, texture: Texture, shape: Shape) =
        let triangle = indices.[(hit.Shape :?> TrianglePrimitive).Index]
        let a, b, c = vertices.[triangle.A], vertices.[triangle.B], vertices.[triangle.C]
        let alpha, beta, gamma = hit.BarycentricAlpha, hit.BarycentricBeta, hit.BarycentricGamma
        let shading =
            if smooth then
                Vector(alpha*a.Nx + beta*b.Nx + gamma*c.Nx,
                       alpha*a.Ny + beta*b.Ny + gamma*c.Ny,
                       alpha*a.Nz + beta*b.Nz + gamma*c.Nz)
            else hit.GeometricNormal
        let u, v = interpolateUv a.U b.U c.U beta gamma, interpolateUv a.V b.V c.V beta gamma
        let shaded =
            HitPoint(hit.Ray, hit.Time, hit.GeometricNormal, shading, getFunc texture u v,
                     shape, u, v, beta, gamma, true)
        if smooth then shaded.WithShadowPoint(MeshGeometry.TerminatorPoint(hit.Point, hit.GeometricNormal, shading, a, b, c, alpha, beta, gamma))
        else shaded

    /// Hanika 2021 shadow-terminator point: project p onto each vertex's tangent plane
    /// when it lies below it, then blend with the barycentrics.
    static member TerminatorPoint(p: Point, geometric: Vector, shading: Vector, a: MeshVertex, b: MeshVertex, c: MeshVertex,
                                  alpha: float, beta: float, gamma: float) : Point =
        // Vertex normals follow the same flip HitPoint applies to the interpolated normal.
        let flip = if shading * geometric < 0. then -1. else 1.
        let corner (v: MeshVertex) =
            let nx, ny, nz = flip * v.Nx, flip * v.Ny, flip * v.Nz
            let d = (p.X - v.X) * nx + (p.Y - v.Y) * ny + (p.Z - v.Z) * nz
            if d >= 0. then struct (p.X, p.Y, p.Z)
            else struct (p.X - d * nx, p.Y - d * ny, p.Z - d * nz)
        let struct (ax, ay, az) = corner a
        let struct (bx, by, bz) = corner b
        let struct (cx, cy, cz) = corner c
        Point(alpha*ax + beta*bx + gamma*cx, alpha*ay + beta*by + gamma*cy, alpha*az + beta*bz + gamma*cz)

type MeshShape internal (geometry: MeshGeometry, texture: Texture) =
    inherit Shape()
    member _.Geometry = geometry
    member _.Texture = texture
    override _.IsOpaque = Textures.isOpaque texture
    override _.getBoundingBox() = geometry.Bounds
    override _.isInside point =
        if not (geometry.Bounds.isInside point) then false
        else
            let hit = geometry.Intersect(Ray(point, Vector(0.754877666,0.569840291,0.323606798)))
            hit.DidHit && not hit.FrontFace
    member private this.Intersect(ray: Ray, minimum: float, maximum: float) =
        let hit = geometry.Intersect(ray, minimum, maximum)
        if hit.DidHit then geometry.Shade(hit, texture, this) else hit
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)

type BaseMeshShape internal (geometry: MeshGeometry) =
    inherit BaseShape()
    member _.Geometry = geometry
    override _.toShape texture = MeshShape(geometry, texture) :> Shape

let drawTriangles (filepath: string) (smoothen: bool) =
    let vertices, faces = parseIndexedPLY filepath
    BaseMeshShape(MeshGeometry(vertices, faces, smoothen)) :> BaseShape

/// Builds a mesh from in-memory arrays, e.g. a glTF primitive. `normals` and `uvs` are either empty or hold one
/// entry per position; `triangles` holds three position indices per triangle.
let fromArrays (positions: Point[]) (normals: Vector[]) (uvs: (float * float)[]) (triangles: int[]) (smooth: bool) =
    if isNull positions || positions.Length = 0 then invalidArg (nameof positions) "A mesh needs at least one vertex."
    if not (isNull normals || normals.Length = 0 || normals.Length = positions.Length) then
        invalidArg (nameof normals) "Give one normal per vertex, or none."
    if not (isNull uvs || uvs.Length = 0 || uvs.Length = positions.Length) then
        invalidArg (nameof uvs) "Give one UV per vertex, or none."
    if isNull triangles || triangles.Length = 0 || triangles.Length % 3 <> 0 then
        invalidArg (nameof triangles) "Triangle indices must come in threes."
    if triangles |> Array.exists (fun i -> i < 0 || i >= positions.Length) then
        invalidArg (nameof triangles) "A triangle index is out of range."
    let hasNormals = not (isNull normals) && normals.Length > 0
    let hasUvs = not (isNull uvs) && uvs.Length > 0
    let vertices =
        positions |> Array.mapi (fun i p ->
            let nx, ny, nz =
                if hasNormals && normals.[i].IsFinite && not normals.[i].IsZero then Some normals.[i].X, Some normals.[i].Y, Some normals.[i].Z
                else None, None, None
            let u, v = if hasUvs then Some (fst uvs.[i]), Some (snd uvs.[i]) else None, None
            Vertex(p.X, p.Y, p.Z, nx, ny, nz, u, v))
    let faces = Array.init (triangles.Length / 3) (fun t -> [| triangles.[3 * t]; triangles.[3 * t + 1]; triangles.[3 * t + 2] |])
    BaseMeshShape(MeshGeometry(vertices, faces, smooth)) :> BaseShape
