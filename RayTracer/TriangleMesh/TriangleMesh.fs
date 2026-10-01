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

/// Coverage of a cut-out surface (glTF alphaMode MASK or BLEND) over the mesh's texture coordinates (u, v as
/// stored on the mesh, v up). Where the surface is not covered, rays pass through it as if it were not
/// there: camera, bounce and shadow rays alike, since the test happens inside mesh traversal.
[<Sealed; AllowNullLiteral>]
type AlphaMask(coverage: float -> float -> float, cutoff: float, stochastic: bool) =
    /// Alpha in [0,1] at (u, v).
    member _.Coverage(u: float, v: float) = coverage u v
    member _.Cutoff = cutoff
    /// Stochastic coverage keeps a surface with probability alpha per ray, so partial transparency averages
    /// out over samples; otherwise the surface is kept where alpha >= Cutoff.
    member _.IsStochastic = stochastic
    /// True where a ray hits the surface. `xi` in [0,1) is a per-ray random number (stochastic coverage only).
    member _.Covers(u: float, v: float, xi: float) =
        let alpha = coverage u v
        if stochastic then alpha > xi else alpha >= cutoff
    /// A hard cut-out: covered where alpha >= cutoff (glTF MASK).
    static member Cutout(coverage: float -> float -> float, cutoff: float) = AlphaMask(coverage, cutoff, false)
    /// Fractional coverage, resolved per ray (glTF BLEND as stochastic transparency).
    static member Stochastic(coverage: float -> float -> float) = AlphaMask(coverage, 0.5, true)

module internal AlphaHash =
    /// Uniform [0,1) number from the ray and the primitive: the same ray always gets the same answer.
    let sample (ray: Ray) (index: int) =
        let o, d = ray.GetOrigin, ray.GetDirection
        let inline bits (x: float) = uint64 (BitConverter.DoubleToInt64Bits x)
        let mutable h = Sampling.mixKey (uint64 (uint32 index) + 0x9E3779B97F4A7C15UL)
        h <- Sampling.mixKey (h ^^^ bits o.X)
        h <- Sampling.mixKey (h ^^^ bits o.Y)
        h <- Sampling.mixKey (h ^^^ bits o.Z)
        h <- Sampling.mixKey (h ^^^ bits d.X)
        h <- Sampling.mixKey (h ^^^ bits d.Y)
        h <- Sampling.mixKey (h ^^^ bits d.Z)
        float (h >>> 11) * (1. / 9007199254740992.)

/// Per-vertex tangent: the direction of increasing u, and in W the handedness (+1 or -1) of the bitangent
/// W * (normal x tangent), which points along increasing v (as stored on the mesh, v up). This is glTF's
/// TANGENT attribute once its texture coordinates are flipped to v up.
[<Struct; System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>]
type MeshTangent = { Tx: float32; Ty: float32; Tz: float32; W: float32 }

/// Where a mesh's tangent frame comes from.
type TangentSource =
    /// No tangents: normal maps fall back to the renderer-chosen frame.
    | NoTangents
    /// One tangent per vertex (e.g. a glTF TANGENT attribute).
    | GivenTangents of MeshTangent[]
    /// Generated from positions, normals and texture coordinates.
    | GenerateTangents

type private TrianglePrimitive(vertices: MeshVertex array, indices: TriangleIndices, index: int, mask: AlphaMask) =
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
    /// Intersection within (minimum, maximum). The alpha test runs only on hits inside the interval, so a
    /// traversal that has already found a closer hit does not pay for texture lookups behind it.
    member private this.Hit(ray: Ray, minimum: float, maximum: float) =
        let a, b, c = vertices.[indices.A], vertices.[indices.B], vertices.[indices.C]
        match (if nondegenerate then Geometry.intersectTriangleCoordinates ray a.X a.Y a.Z b.X b.Y b.Z c.X c.Y c.Z else ValueNone) with
        | ValueNone -> HitPoint(ray)
        | ValueSome(struct (time, beta, gamma)) ->
            if isNull mask then HitPoint(ray, time, normal, normal, material, this, 0., 0., beta, gamma, true)
            elif not (time > minimum && time < maximum) then HitPoint(ray)
            elif mask.Covers(interpolateUv a.U b.U c.U beta gamma, interpolateUv a.V b.V c.V beta gamma,
                             (if mask.IsStochastic then AlphaHash.sample ray index else 0.)) then
                HitPoint(ray, time, normal, normal, material, this, 0., 0., beta, gamma, true)
            else HitPoint(ray)
    override this.hitFunction ray = this.Hit(ray, -infinity, infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Hit(ray, minimum, maximum) |> Geometry.within minimum maximum

module private Tangents =
    /// Per-vertex tangents from texture coordinates (Lengyel's accumulation, then Gram-Schmidt against the
    /// vertex normal). Vertices without a usable UV gradient get a zero tangent, which shading ignores.
    let generate (vertices: MeshVertex array) (triangles: TriangleIndices array) =
        let n = vertices.Length
        let tan1 = Array.zeroCreate<float> (3 * n)
        let tan2 = Array.zeroCreate<float> (3 * n)
        let faceNormals = Array.zeroCreate<float> (3 * n)
        let add (target: float[]) v x y z =
            target.[3 * v] <- target.[3 * v] + x
            target.[3 * v + 1] <- target.[3 * v + 1] + y
            target.[3 * v + 2] <- target.[3 * v + 2] + z
        for triangle in triangles do
            let a, b, c = vertices.[triangle.A], vertices.[triangle.B], vertices.[triangle.C]
            let e1x, e1y, e1z = b.X - a.X, b.Y - a.Y, b.Z - a.Z
            let e2x, e2y, e2z = c.X - a.X, c.Y - a.Y, c.Z - a.Z
            let fnx, fny, fnz = e1y * e2z - e1z * e2y, e1z * e2x - e1x * e2z, e1x * e2y - e1y * e2x
            let du1, dv1 = b.U - a.U, b.V - a.V
            let du2, dv2 = c.U - a.U, c.V - a.V
            let r = du1 * dv2 - du2 * dv1
            let ok = Double.IsFinite r && abs r > 1e-20
            let f = if ok then 1. / r else 0.
            let sx, sy, sz = (e1x * dv2 - e2x * dv1) * f, (e1y * dv2 - e2y * dv1) * f, (e1z * dv2 - e2z * dv1) * f
            let tx, ty, tz = (e2x * du1 - e1x * du2) * f, (e2y * du1 - e1y * du2) * f, (e2z * du1 - e1z * du2) * f
            for v in [| triangle.A; triangle.B; triangle.C |] do
                if ok then
                    add tan1 v sx sy sz
                    add tan2 v tx ty tz
                add faceNormals v fnx fny fnz
        let none = { Tx = 0.f; Ty = 0.f; Tz = 0.f; W = 1.f }
        Array.init n (fun i ->
            let vertex = vertices.[i]
            let nx, ny, nz =
                if vertex.Nx <> 0. || vertex.Ny <> 0. || vertex.Nz <> 0. then vertex.Nx, vertex.Ny, vertex.Nz
                else faceNormals.[3 * i], faceNormals.[3 * i + 1], faceNormals.[3 * i + 2]
            let nl = sqrt (nx * nx + ny * ny + nz * nz)
            let tx, ty, tz = tan1.[3 * i], tan1.[3 * i + 1], tan1.[3 * i + 2]
            if not (nl > 0. && Double.IsFinite nl) then none
            else
                let nx, ny, nz = nx / nl, ny / nl, nz / nl
                let d = nx * tx + ny * ty + nz * tz
                let ox, oy, oz = tx - d * nx, ty - d * ny, tz - d * nz
                let ol = sqrt (ox * ox + oy * oy + oz * oz)
                if not (ol > 1e-30 && Double.IsFinite ol) then none
                else
                    let ox, oy, oz = ox / ol, oy / ol, oz / ol
                    // Handedness: does normal x tangent point along increasing v?
                    let bx, by, bz = ny * oz - nz * oy, nz * ox - nx * oz, nx * oy - ny * ox
                    let w = if bx * tan2.[3 * i] + by * tan2.[3 * i + 1] + bz * tan2.[3 * i + 2] < 0. then -1.f else 1.f
                    { Tx = float32 ox; Ty = float32 oy; Tz = float32 oz; W = w })

type MeshGeometry internal (inputVertices: Vertex array, inputFaces: int array array, smooth: bool,
                            tangentSource: TangentSource, mask: AlphaMask) =
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
    let tangents =
        match tangentSource with
        | NoTangents -> Array.empty
        | GivenTangents given ->
            if given.Length <> vertices.Length then invalidArg (nameof tangentSource) "Give one tangent per vertex."
            Array.copy given
        | GenerateTangents -> Tangents.generate vertices indices
    let primitives =
        indices |> Array.mapi (fun index triangle -> TrianglePrimitive(vertices, triangle, index, mask) :> Shape)
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
    /// Per-vertex tangents (empty when the mesh has none).
    member _.Tangents = ReadOnlyMemory<MeshTangent>(tangents)
    member _.AlphaMask = mask
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
        let shaded =
            if smooth then shaded.WithShadowPoint(MeshGeometry.TerminatorPoint(hit.Point, hit.GeometricNormal, shading, a, b, c, alpha, beta, gamma))
            else shaded
        if tangents.Length = 0 then shaded
        else
            let ta, tb, tc = tangents.[triangle.A], tangents.[triangle.B], tangents.[triangle.C]
            let t = Vector(alpha * float ta.Tx + beta * float tb.Tx + gamma * float tc.Tx,
                           alpha * float ta.Ty + beta * float tb.Ty + gamma * float tc.Ty,
                           alpha * float ta.Tz + beta * float tb.Tz + gamma * float tc.Tz)
            let w = alpha * float ta.W + beta * float tb.W + gamma * float tc.W
            // The bitangent follows the interpolated vertex normal, before HitPoint orients it.
            let bitangent = (if w < 0. then -1. else 1.) * (shading % t)
            if t.IsFinite && not t.IsZero && bitangent.IsFinite && not bitangent.IsZero then shaded.WithTangent(t, bitangent)
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
    BaseMeshShape(MeshGeometry(vertices, faces, smoothen, NoTangents, null)) :> BaseShape

/// Builds a mesh from in-memory arrays, e.g. a glTF primitive, with a tangent frame for normal maps and an
/// optional alpha mask (null for an opaque surface). `normals` and `uvs` are either empty or hold one
/// entry per position; `triangles` holds three position indices per triangle.
let fromArraysWith (tangents: TangentSource) (mask: AlphaMask) (positions: Point[]) (normals: Vector[])
                   (uvs: (float * float)[]) (triangles: int[]) (smooth: bool) =
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
    BaseMeshShape(MeshGeometry(vertices, faces, smooth, tangents, mask)) :> BaseShape

/// Builds a mesh from in-memory arrays, e.g. a glTF primitive. `normals` and `uvs` are either empty or hold one
/// entry per position; `triangles` holds three position indices per triangle.
let fromArrays (positions: Point[]) (normals: Vector[]) (uvs: (float * float)[]) (triangles: int[]) (smooth: bool) =
    fromArraysWith NoTangents null positions normals uvs triangles smooth
