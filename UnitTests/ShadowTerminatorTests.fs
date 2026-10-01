module ShadowTerminatorTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.TriangleMesh

// Quad in the y=0 plane split along the x=z diagonal; geometric normal is +y.
let private positions = [| Point(-1.,0.,-1.); Point(1.,0.,-1.); Point(1.,0.,1.); Point(-1.,0.,1.) |]
let private triangles = [| 0; 2; 1;  0; 3; 2 |]
let private texture = Textures.mkMatTexture (BlankMaterial())

let private hitAt (normals: Vector[]) (x: float) (z: float) =
    let shape = (fromArrays positions normals [||] triangles true).toShape texture
    shape.hitFunction (Ray(Point(x, 5., z), Vector(0., -1., 0.)))

let private flatNormals = Array.create 4 (Vector(0., 1., 0.))
// Vertex normals tilted outward from the quad centre (a convex patch).
let private curvedNormals =
    positions |> Array.map (fun p -> Vector(p.X * 0.5, 1., p.Z * 0.5).Normalise)

let private distance (a: Point) (b: Point) = (a - b).Magnitude

let flatTests () =
    for (x, z) in [ (0.3, -0.5); (-0.4, 0.2); (0.6, 0.7) ] do
        let hit = hitAt flatNormals x z
        Assert.True (hit.DidHit, "terminator-flat-hit")
        Assert.True (distance hit.ShadowPoint hit.Point < 1e-12, "terminator-flat-shadow-point-equals-hit-point")

let curvedTests () =
    for (x, z) in [ (0.3, -0.5); (-0.4, 0.2); (0.6, 0.7); (-0.8, -0.1) ] do
        let hit = hitAt curvedNormals x z
        Assert.True (hit.DidHit, "terminator-curved-hit")
        let lift = (hit.ShadowPoint - hit.Point) * hit.GeometricNormal
        Assert.True (lift >= -1e-12, "terminator-curved-shadow-point-on-or-above-plane")
    // Outward-tilted (convex) normals put the point below the vertex tangent planes, so it is lifted.
    let hit = hitAt curvedNormals 0.3 -0.5
    Assert.True ((hit.ShadowPoint - hit.Point) * hit.GeometricNormal > 1e-6, "terminator-convex-shadow-point-is-lifted")
    Assert.True (distance hit.ShadowPoint hit.Point < 0.5, "terminator-convex-shadow-point-stays-near-hit")
    // Inward-tilted (concave) normals leave the point above every tangent plane: unchanged.
    let inward = positions |> Array.map (fun p -> Vector(-p.X * 0.5, 1., -p.Z * 0.5).Normalise)
    let same = hitAt inward 0.3 -0.5
    Assert.True (distance same.ShadowPoint same.Point < 1e-12, "terminator-concave-shadow-point-unchanged")

let continuityTests () =
    // Straddle the shared diagonal x=z: the shadow point must be continuous across it.
    let a = hitAt curvedNormals 0.5 0.4999999
    let b = hitAt curvedNormals 0.5 0.5000001
    Assert.True (a.DidHit && b.DidHit, "terminator-edge-hits")
    Assert.True (distance a.ShadowPoint b.ShadowPoint < 1e-5, "terminator-shadow-point-continuous-at-shared-edge")
    let c = hitAt (positions |> Array.map (fun p -> Vector(-p.X * 0.5, 1., -p.Z * 0.5).Normalise)) 0.5 0.4999999
    let d = hitAt (positions |> Array.map (fun p -> Vector(-p.X * 0.5, 1., -p.Z * 0.5).Normalise)) 0.5 0.5000001
    Assert.True (distance c.ShadowPoint d.ShadowPoint < 1e-5, "terminator-concave-shadow-point-continuous-at-shared-edge")

let originTests () =
    let hit = hitAt curvedNormals 0.3 -0.5
    let up = Vector(0., 1., 0.)
    Assert.True ((hit.ShadowOrigin up).Y > (hit.OffsetPoint up).Y, "terminator-shadow-origin-lifted-when-light-on-shading-side")
    let flat = hitAt flatNormals 0.3 -0.5
    Assert.True (distance (flat.ShadowOrigin up) (flat.OffsetPoint up) < 1e-9, "terminator-flat-shadow-origin-is-plain-offset")

let allTest () =
    flatTests ()
    curvedTests ()
    continuityTests ()
    originTests ()
