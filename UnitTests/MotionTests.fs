module MotionTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Transformation
open Tracer.Basics.Render

// Rotation maths, keyed transforms and ray shutter time: the library half of motion blur.

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance
let private nearVector tolerance (a: Vector) (b: Vector) =
    near tolerance a.X b.X && near tolerance a.Y b.Y && near tolerance a.Z b.Z
let private nearPoint tolerance (a: Point) (b: Point) =
    near tolerance a.X b.X && near tolerance a.Y b.Y && near tolerance a.Z b.Z
let private matrixValues (m: QuickMatrix) =
    [| m.Pos1x1; m.Pos1x2; m.Pos1x3; m.Pos1x4; m.Pos2x1; m.Pos2x2; m.Pos2x3; m.Pos2x4
       m.Pos3x1; m.Pos3x2; m.Pos3x3; m.Pos3x4; m.Pos4x1; m.Pos4x2; m.Pos4x3; m.Pos4x4 |]
let private nearMatrix tolerance a b =
    Array.forall2 (near tolerance) (matrixValues a) (matrixValues b)

let private red = Textures.mkMatTexture (MatteMaterial(Colour.Black, 0., Colour.Red, 1.))

let quaternionTests () =
    let q = Quaternion.ofAxisAngle (Vector(0., 0., 1.)) (Math.PI / 2.)
    Assert.True(nearVector 1e-12 (Quaternion.rotate q (Vector(1., 0., 0.))) (Vector(0., 1., 0.)), "quaternion-rotates-x-to-y")
    Assert.True(nearMatrix 1e-12 (Quaternion.toMatrix q) (getMatrix (rotateZ (Math.PI / 2.))), "quaternion-matrix-matches-rotateZ")
    let euler = Quaternion.ofEuler 0.3 -0.7 1.1
    let merged = mergeTransformations [ rotateX 0.3; rotateY -0.7; rotateZ 1.1 ]
    Assert.True(nearMatrix 1e-12 (Quaternion.toMatrix euler) (getMatrix merged), "quaternion-euler-order-matches-merge")
    Assert.True(Quaternion.dot (Quaternion.ofRotationMatrix (Quaternion.toMatrix euler)) euler |> abs |> near 1e-12 1., "quaternion-matrix-round-trip")
    let a = Quaternion.identity
    let b = Quaternion.ofAxisAngle (Vector(0., 1., 0.)) (Math.PI / 2.)
    let middle = Quaternion.slerp a b 0.5
    Assert.True(abs (Quaternion.dot middle (Quaternion.ofAxisAngle (Vector(0., 1., 0.)) (Math.PI / 4.))) |> near 1e-12 1., "slerp-midpoint-is-half-angle")
    Assert.True(Quaternion.dot (Quaternion.slerp a b 0.) a |> near 1e-12 1. && abs (Quaternion.dot (Quaternion.slerp a b 1.) b) |> near 1e-12 1., "slerp-endpoints")
    // b and -b are the same rotation; interpolation must still take the short 90-degree arc.
    let viaNegated = Quaternion.slerp a (Quaternion.negate b) 0.5
    Assert.True(abs (Quaternion.dot viaNegated middle) |> near 1e-12 1., "slerp-shortest-path")
    Assert.True(Quaternion.length (Quaternion.slerp a b 0.37) |> near 1e-12 1., "slerp-unit-length")
    let tiny = Quaternion.ofAxisAngle (Vector(1., 0., 0.)) 1e-9
    Assert.True(Quaternion.length (Quaternion.slerp a tiny 0.5) |> near 1e-12 1., "slerp-near-parallel-stable")
    let look = Quaternion.lookRotation (Vector(1., 0., 0.)) (Vector(0., 1., 0.))
    Assert.True(nearVector 1e-12 (Quaternion.rotate look (Vector(0., 0., -1.))) (Vector(1., 0., 0.)), "look-rotation-forward")
    Assert.True(nearVector 1e-12 (Quaternion.rotate look (Vector(0., 1., 0.))) (Vector(0., 1., 0.)), "look-rotation-up")

let trsTests () =
    let trs =
        { Translation = Vector(1., -2., 3.)
          Rotation = Quaternion.ofEuler 0.2 0.4 -0.9
          Scale = Vector(2., 0.5, 3.) }
    let expected =
        mergeTransformations [ scale 2. 0.5 3.; rotateX 0.2; rotateY 0.4; rotateZ -0.9; translate 1. -2. 3. ]
    let actual = Trs.toTransformation trs
    Assert.True(nearMatrix 1e-12 (getMatrix actual) (getMatrix expected), "trs-matches-merged-transformations")
    Assert.True(nearMatrix 1e-12 (QuickMatrix.multi (getMatrix actual, getInvMatrix actual)) identityMatrix, "trs-inverse")
    let general = getMatrix (mergeTransformations [ sheare (0.3, 0., 0.1, 0., 0., 0.2); rotateY 1.; translate 4. 5. 6. ])
    let t = ofAffine general
    Assert.True(nearMatrix 1e-12 (QuickMatrix.multi (general, getInvMatrix t)) identityMatrix, "of-affine-inverse")
    Assert.Throws<ArgumentException>((fun () -> ofAffine { identityMatrix with Pos2x2 = 0. } |> ignore), "of-affine-singular-rejected")
    let mid = Trs.lerp Trs.identity trs 0.5
    Assert.True(nearVector 1e-12 mid.Translation (Vector(0.5, -1., 1.5)) && nearVector 1e-12 mid.Scale (Vector(1.5, 0.75, 2.)), "trs-lerp")

let animatedTransformTests () =
    let m0 = getMatrix (translate 0. 0. 0.)
    let m1 = getMatrix (mergeTransformations [ rotateY (Math.PI / 2.); translate 2. 0. 0. ])
    let motion = AnimatedTransform([| 0., m0; 1., m1 |])
    let struct (a, ai) = motion.At 0.
    let struct (b, _) = motion.At 1.
    Assert.True(nearMatrix 1e-12 a m0 && nearMatrix 1e-12 b m1, "animated-transform-hits-keys")
    Assert.True(nearMatrix 1e-12 (QuickMatrix.multi (a, ai)) identityMatrix, "animated-transform-inverse")
    let struct (half, halfInverse) = motion.At 0.5
    let expectedHalf = getMatrix (mergeTransformations [ rotateY (Math.PI / 4.); translate 1. 0. 0. ])
    Assert.True(nearMatrix 1e-12 half expectedHalf, "animated-transform-slerps-rotation")
    Assert.True(nearMatrix 1e-12 (QuickMatrix.multi (half, halfInverse)) identityMatrix, "animated-transform-interpolated-inverse")
    let struct (before, _) = motion.At -5.
    let struct (after, _) = motion.At 5.
    Assert.True(nearMatrix 0. before m0 && nearMatrix 0. after m1, "animated-transform-clamps")
    // A world matrix under a non-uniformly scaled, rotated parent is not a TRS; polar decomposition must still
    // reproduce it exactly at the keys.
    let skewed = getMatrix (mergeTransformations [ rotateZ 0.7; scale 3. 1. 1.; rotateY 0.4; translate 1. 2. 3. ])
    let mirrored = getMatrix (mergeTransformations [ scale -1. 2. 1.; rotateX 0.3 ])
    let general = AnimatedTransform([| 0., skewed; 0.5, mirrored; 1., m0 |])
    let struct (k0, _) = general.At 0.
    let struct (k1, _) = general.At 0.5
    Assert.True(nearMatrix 1e-10 k0 skewed && nearMatrix 1e-10 k1 mirrored, "animated-transform-general-affine-keys")
    Assert.Throws<ArgumentException>((fun () -> AnimatedTransform([| 1., m0; 0., m1 |]) |> ignore), "animated-transform-rejects-unsorted")

let rayTimeTests () =
    let ray = Ray(Point(0., 0., 5.), Vector(0., 0., -1.), 0.25)
    Assert.True(ray.ShutterTime = 0.25 && ray.Invert.ShutterTime = 0.25, "ray-carries-shutter-time")
    Assert.True(Ray(Point.Zero, Vector(0., 0., 1.)).ShutterTime = 0., "ray-default-time-zero")
    let sphere = SphereShape(Point.Zero, 1., red) :> Shape
    let hit = sphere.hitFunction ray
    Assert.True(hit.DidHit && (hit.SpawnRay (Vector(0., 1., 1.))).ShutterTime = 0.25, "spawn-ray-keeps-shutter-time")

let motionTransformTests () =
    // A unit sphere moving from x = -2 to x = 2 during the shutter [0, 1].
    let motion =
        AnimatedTransform([| 0., getMatrix (translate -2. 0. 0.); 1., getMatrix (translate 2. 0. 0.) |])
    let moving = MotionTransform.transform (SphereShape(Point.Zero, 1., red)) motion
    let probe x time = moving.hitFunction (Ray(Point(x, 0., 5.), Vector(0., 0., -1.), time))
    Assert.True((probe -2. 0.).DidHit && not (probe 2. 0.).DidHit, "motion-transform-pose-at-open")
    Assert.True((probe 2. 1.).DidHit && not (probe -2. 1.).DidHit, "motion-transform-pose-at-close")
    Assert.True((probe 0. 0.5).DidHit && near 1e-12 (probe 0. 0.5).Time 4., "motion-transform-pose-mid-shutter")
    let bounds = moving.Bounds.Value
    Assert.True(bounds.lowPoint.X <= -3. && bounds.highPoint.X >= 3., "motion-transform-bounds-cover-sweep")
    // Rotation sweeps an arc: the box around an off-centre sphere must contain every intermediate pose.
    let spin =
        AnimatedTransform([| 0., identityMatrix; 1., getMatrix (rotateY Math.PI) |])
    let orbiting = MotionTransform.transform (SphereShape(Point(3., 0., 0.), 0.5, red)) spin
    let orbitBounds = orbiting.Bounds.Value
    let rng = Random(11)
    let contained =
        Seq.init 64 (fun _ -> rng.NextDouble()) |> Seq.forall (fun time ->
            let struct (m, _) = spin.At time
            let centre = transformPoint (Point(3., 0., 0.), m)
            orbitBounds.lowPoint.X <= centre.X - 0.5 && orbitBounds.highPoint.X >= centre.X + 0.5
            && orbitBounds.lowPoint.Z <= centre.Z - 0.5 && orbitBounds.highPoint.Z >= centre.Z + 0.5)
    Assert.True(contained, "motion-transform-bounds-contain-rotation-arc")
    // Nested instancing: an outer static Transform must forward the ray's time to the moving child.
    let nested = Transform.transform moving (translate 0. 10. 0.)
    let nestedHit = nested.hitFunction (Ray(Point(2., 10., 5.), Vector(0., 0., -1.), 1.))
    Assert.True(nestedHit.DidHit, "transform-forwards-shutter-time")
    let still = MotionTransform.transform (SphereShape(Point.Zero, 1., red)) (AnimatedTransform([| 0., identityMatrix; 1., identityMatrix |]))
    Assert.True((still.hitFunction (Ray(Point(0., 0., 5.), Vector(0., 0., -1.), 0.7))).DidHit, "motion-transform-static-keys")

let shutterTests () =
    let camera shutterOpen shutterClose =
        PinholeCamera(Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.), 1., 2., 2., 4, 4, regular 4,
                      shutterOpen, shutterClose)
    let closed = PinholeCamera(Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.), 1., 2., 2., 4, 4, regular 4)
    Assert.True(closed.ShutterOpen = 0. && closed.ShutterClose = 0. && (closed.CreateRay(1, 2, 3)).ShutterTime = 0., "shutter-default-closed")
    let open' = camera 2. 2.5
    let times = [| for s in 0 .. 15 -> (open'.CreateRay(1, 2, s)).ShutterTime |]
    Assert.True(times |> Array.forall (fun t -> t >= 2. && t < 2.5), "shutter-times-within-interval")
    // Stratified: each sixteenth of the shutter receives exactly one sample.
    let strata = times |> Array.map (fun t -> int ((t - 2.) / 0.5 * 16.)) |> Array.sort
    Assert.Equal([| 0 .. 15 |], strata, "shutter-times-stratified")
    Assert.Throws<ArgumentException>((fun () -> camera 1. 0. |> ignore), "shutter-reversed-rejected")
    let lens = ThinLensCamera(Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.), 1., 2., 2., 4, 4, 0.1, 5., regular 2, regular 2, 0., 1.)
    Assert.True((lens.CreateRay(0, 0, 3)).ShutterTime > 0., "thin-lens-samples-shutter")

/// A sphere moving horizontally during the shutter smears across the image; closed, it leaves a sharp disc.
let blurRenderTests () =
    let white = Textures.mkMatTexture (EmissiveMaterial(Colour.White, 1.))
    let motion =
        AnimatedTransform([| 0., getMatrix (translate -1.5 0. 0.); 1., getMatrix (translate 1.5 0. 0.) |])
    let scene = Scene([ MotionTransform.transform (SphereShape(Point.Zero, 0.5, white)) motion ], [], AmbientLight(Colour.Black, 0.), 1)
    let render shutterOpen shutterClose =
        let camera =
            PinholeCamera(Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.), 5., 4., 2., 32, 16, regular 8,
                          shutterOpen, shutterClose)
        (Render(scene, camera, { RenderOptions.Default with Seed = 3 })).RenderLinear
    let pixel (film: RenderFilm) x = film.Pixels.[((film.Height / 2) * film.Width + x) * 3]
    let still = render 0.5 0.5
    let blurred = render 0. 1.
    Assert.True(pixel still 16 > 0.99 && pixel still 4 = 0., "closed-shutter-sharp")
    let centre = pixel blurred 16
    Assert.True(centre > 0.1 && centre < 0.6, "open-shutter-partial-coverage")
    Assert.True(pixel blurred 4 > 0.05 && pixel blurred 27 > 0.05, "open-shutter-smears-along-path")

let allTest () =
    quaternionTests ()
    trsTests ()
    animatedTransformTests ()
    rayTimeTests ()
    motionTransformTests ()
    shutterTests ()
    blurRenderTests ()
