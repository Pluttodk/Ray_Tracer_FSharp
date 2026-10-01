module Tracer.Basics.DeformingMesh

open System
open Tracer.Basics
open Tracer.Basics.TriangleMesh
open Tracer.Basics.Textures

// A triangle mesh whose vertices move linearly between keyed poses (e.g. a skin posed at several instants of
// the shutter). Each ray is intersected against the vertices interpolated at its own ShutterTime, so the
// deformation itself blurs. One BVH is built over the union of every triangle's bounds across all keys; a
// triangle whose vertices move linearly stays inside the box of its keyed vertices, so no extra sampling is needed.

let inline private lerp (a: float) (b: float) (f: float) = a + f * (b - a)

/// Key segment (index, fraction) for a time; times outside the keyed range clamp to the end poses.
let inline private segment (times: float[]) (t: float) =
    let last = times.Length - 1
    if t <= times.[0] then struct (0, 0.)
    elif t >= times.[last] then struct (last - 1, 1.)
    else
        let mutable i = 0
        while times.[i + 1] < t do i <- i + 1
        struct (i, (t - times.[i]) / (times.[i + 1] - times.[i]))

let inline private blend (a: MeshVertex) (b: MeshVertex) (f: float) : MeshVertex =
    { X = lerp a.X b.X f; Y = lerp a.Y b.Y f; Z = lerp a.Z b.Z f
      Nx = lerp a.Nx b.Nx f; Ny = lerp a.Ny b.Ny f; Nz = lerp a.Nz b.Nz f
      U = a.U; V = a.V }

type private Keys = { Times: float[]; Vertices: MeshVertex[][] }

type private DeformingTriangle(keys: Keys, indices: TriangleIndices, index: int, mask: AlphaMask) =
    inherit Shape()
    let bounds =
        let mutable lx, ly, lz = infinity, infinity, infinity
        let mutable hx, hy, hz = -infinity, -infinity, -infinity
        for vertices in keys.Vertices do
            for i in [| indices.A; indices.B; indices.C |] do
                let v = vertices.[i]
                lx <- min lx v.X; ly <- min ly v.Y; lz <- min lz v.Z
                hx <- max hx v.X; hy <- max hy v.Y; hz <- max hz v.Z
        Geometry.paddedBounds (Point(lx, ly, lz)) (Point(hx, hy, hz))
    member _.Index = index
    override _.IsOpaque = true
    override _.getBoundingBox() = bounds
    override _.isInside _ = false
    override this.hitFunction ray =
        let struct (k, f) = segment keys.Times ray.ShutterTime
        let lo, hi = keys.Vertices.[k], keys.Vertices.[k + 1]
        let a, b, c = blend lo.[indices.A] hi.[indices.A] f, blend lo.[indices.B] hi.[indices.B] f, blend lo.[indices.C] hi.[indices.C] f
        let normal = (Vector(b.X-a.X,b.Y-a.Y,b.Z-a.Z) % Vector(c.X-a.X,c.Y-a.Y,c.Z-a.Z)).Normalise
        if not (normal.IsFinite && (normal.X <> 0. || normal.Y <> 0. || normal.Z <> 0.)) then HitPoint(ray)
        else
            match Geometry.intersectTriangleCoordinates ray a.X a.Y a.Z b.X b.Y b.Z c.X c.Y c.Z with
            | ValueNone -> HitPoint(ray)
            | ValueSome(struct (time, beta, gamma)) ->
                let covered =
                    isNull mask
                    || mask.Covers(a.U + beta * (b.U - a.U) + gamma * (c.U - a.U), a.V + beta * (b.V - a.V) + gamma * (c.V - a.V),
                                   (if mask.IsStochastic then AlphaHash.sample ray index else 0.))
                if covered then HitPoint(ray, time, normal, normal, Material.None, this, 0., 0., beta, gamma, true)
                else HitPoint(ray)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.hitFunction ray |> Geometry.within minimum maximum

type DeformingMeshShape private (keys: Keys, indices: TriangleIndices[], smooth: bool, texture: Texture, mask: AlphaMask) =
    inherit Shape()
    let primitives = indices |> Array.mapi (fun i t -> DeformingTriangle(keys, t, i, mask) :> Shape)
    let accelerator = Acceleration.buildWith Acceleration.FlatBVH primitives
    let bounds =
        let low, high =
            primitives |> Array.fold (fun ((low: Point), (high: Point)) shape ->
                let box = shape.getBoundingBox()
                low.Lowest box.lowPoint, high.Highest box.highPoint)
                (Point(infinity,infinity,infinity), Point(-infinity,-infinity,-infinity))
        BBox(low, high)
    let middle = 0.5 * (keys.Times.[0] + keys.Times.[keys.Times.Length - 1])

    /// Builds the mesh; every array under `positions`/`normals` holds one entry per vertex for the key of the
    /// same index in `times` (strictly increasing, at least two). `normals` may be empty (flat shading).
    /// `alphaMask` cuts the surface out where it is not covered (none: opaque).
    static member Create(times: float[], positions: Point[][], normals: Vector[][], uvs: (float * float)[],
                         triangles: int[], smooth: bool, texture: Texture, ?alphaMask: AlphaMask) : Shape =
        if times.Length < 2 then invalidArg (nameof times) "A deforming mesh needs at least two keys."
        if positions.Length <> times.Length then invalidArg (nameof positions) "Give one position array per key."
        if times |> Array.pairwise |> Array.exists (fun (a, b) -> b <= a) then invalidArg (nameof times) "Key times must increase."
        if triangles.Length = 0 || triangles.Length % 3 <> 0 then invalidArg (nameof triangles) "Triangle indices must come in threes."
        let hasNormals = smooth && normals.Length = times.Length
        let vertices =
            positions |> Array.mapi (fun key keyed ->
                keyed |> Array.mapi (fun i p ->
                    let n = if hasNormals then normals.[key].[i].Normalise else Vector.Zero
                    let n = if n.IsFinite then n else Vector.Zero
                    let u, v = if uvs.Length > 0 then uvs.[i] else 0., 0.
                    { X = p.X; Y = p.Y; Z = p.Z; Nx = n.X; Ny = n.Y; Nz = n.Z; U = u; V = v }))
        let indices = Array.init (triangles.Length / 3) (fun t -> { A = triangles.[3*t]; B = triangles.[3*t+1]; C = triangles.[3*t+2] })
        DeformingMeshShape({ Times = times; Vertices = vertices }, indices, hasNormals, texture, defaultArg alphaMask null) :> Shape

    override _.IsOpaque = Textures.isOpaque texture
    override _.getBoundingBox() = bounds
    override this.isInside point =
        if not (bounds.isInside point) then false
        else
            let ray = Ray(point, Vector(0.754877666,0.569840291,0.323606798), middle)
            let hit = Acceleration.traverseClosest accelerator ray 0. infinity
            hit.DidHit && not hit.FrontFace
    member private this.Intersect(ray: Ray, minimum: float, maximum: float) =
        let hit = Acceleration.traverseClosest accelerator ray minimum maximum
        if not hit.DidHit then hit
        else
            let triangle = indices.[(hit.Shape :?> DeformingTriangle).Index]
            let struct (k, f) = segment keys.Times ray.ShutterTime
            let lo, hi = keys.Vertices.[k], keys.Vertices.[k + 1]
            let a, b, c = blend lo.[triangle.A] hi.[triangle.A] f, blend lo.[triangle.B] hi.[triangle.B] f, blend lo.[triangle.C] hi.[triangle.C] f
            let alpha, beta, gamma = hit.BarycentricAlpha, hit.BarycentricBeta, hit.BarycentricGamma
            let shading =
                if smooth then
                    Vector(alpha*a.Nx + beta*b.Nx + gamma*c.Nx, alpha*a.Ny + beta*b.Ny + gamma*c.Ny, alpha*a.Nz + beta*b.Nz + gamma*c.Nz)
                else hit.GeometricNormal
            let u = a.U + beta * (b.U - a.U) + gamma * (c.U - a.U)
            let v = a.V + beta * (b.V - a.V) + gamma * (c.V - a.V)
            let shaded =
                HitPoint(hit.Ray, hit.Time, hit.GeometricNormal, shading, getFunc texture u v, this, u, v, beta, gamma, true)
            if smooth then shaded.WithShadowPoint(MeshGeometry.TerminatorPoint(hit.Point, hit.GeometricNormal, shading, a, b, c, alpha, beta, gamma))
            else shaded
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)
