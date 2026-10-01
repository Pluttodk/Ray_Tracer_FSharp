module DeformingMeshTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Transformation
open Tracer.Animation

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance
let private white = Textures.mkMatTexture (MatteMaterial(Colour.White, 0., Colour.White, 1.))

/// One triangle whose vertices slide from z = 0 at t = 0 to z = 1 at t = 1 (and x by +2).
let private sliding () =
    let tri dx dz = [| Point(dx, 0., dz); Point(dx + 1., 0., dz); Point(dx, 1., dz) |]
    let normals = Array.create 3 (Vector(0., 0., 1.))
    DeformingMesh.DeformingMeshShape.Create([| 0.; 1. |], [| tri 0. 0.; tri 2. 1. |], [| normals; normals |], [||], [| 0; 1; 2 |], true, white)

let slidingTests () =
    let shape = sliding ()
    let hitAt time x = shape.hitFunction (Ray(Point(x, 0.2, 5.), Vector(0., 0., -1.), time))
    // At t = 0.25 the triangle sits at x in [0.5, 1.5], z = 0.25.
    let h = hitAt 0.25 0.7
    Assert.True(h.DidHit && near 1e-9 h.Point.Z 0.25, "deform-hit-at-interpolated-position")
    Assert.True(not (hitAt 0.25 0.3).DidHit && not (hitAt 0.25 1.7).DidHit, "deform-misses-other-keys-position")
    Assert.True((hitAt 0. 0.3).DidHit && near 1e-9 (hitAt 0. 0.3).Point.Z 0., "deform-first-key-exact")
    Assert.True((hitAt 1. 2.3).DidHit && near 1e-9 (hitAt 1. 2.3).Point.Z 1., "deform-last-key-exact")
    Assert.True(near 1e-9 h.ShadingNormal.Z 1., "deform-shading-normal-interpolated")
    let box = shape.getBoundingBox()
    Assert.True(box.lowPoint.X <= 0. && box.highPoint.X >= 3. && box.lowPoint.Z <= 0. && box.highPoint.Z >= 1.
                && box.highPoint.Y >= 1., "deform-bounds-contain-all-keys")

/// The bent quad of FilmTests, with a configurable bend clip.
let private skinScene (bend: Clip) =
    let mesh =
        { Positions = [| Point(0., 0., 0.); Point(0., 0.1, 0.); Point(2., 0., 0.); Point(2., 0.1, 0.); Point(1., 0., 0.); Point(1., 0.1, 0.) |]
          Normals = [||]; Uvs = [||]; Triangles = [| 0; 4; 1; 1; 4; 5; 4; 2; 5; 5; 2; 3 |]
          Joints = [| 0;0;0;0; 0;0;0;0; 1;0;0;0; 1;0;0;0; 0;1;0;0; 0;1;0;0 |]
          Weights = [| 1.;0.;0.;0.; 1.;0.;0.;0.; 1.;0.;0.;0.; 1.;0.;0.;0.; 0.5;0.5;0.;0.; 0.5;0.5;0.;0. |]
          JointNodes = [| "base"; "tip" |]
          InverseBind = [| identityMatrix; getInvMatrix (translate 1. 0. 0.) |]
          Texture = white }
    let skeleton = Node.create "base" |> Node.withChildren [ Node.create "tip" |> Node.at 1. 0. 0. ]
    let holder = Node.create "skin" |> Node.at 5. 0. 0. |> Node.withContent [ Skinned mesh ]
    let camera = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
    mesh,
    ({ Name = "t"; Roots = [ skeleton; holder; camera ]; Clips = [ bend ]; ActiveCamera = "camera"; Cuts = []; StaticShapes = []; StaticLights = []
       Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Atmosphere = None; Duration = 2. }
     |> AnimatedScene.validate)

let private hitsAny (shapes: Shape list) time (x, y) =
    shapes |> List.exists (fun s -> (s.hitFunction (Ray(Point(x, y, 5.), Vector(0., 0., -1.), time))).DidHit)

let sceneTests () =
    let quarter = Quaternion.ofAxisAngle (Vector(0., 0., 1.)) (Math.PI / 2.)
    let bend = Clip.create "bend" [ Clip.rotate "tip" (Sampler.linear [ 0., Quaternion.identity; 1., quarter ]) ]
    let mesh, s = skinScene bend
    // Wing beating across the shutter: each ray sees the pose at its own time.
    let blurred = (Frame.sceneAt s 0. 1. 3).Shapes
    Assert.True(hitsAny blurred 0. (1.9, 0.05) && not (hitsAny blurred 0. (0.95, 0.9)), "mblur-ray-at-open-sees-bind-pose")
    Assert.True(hitsAny blurred 1. (0.95, 0.9) && not (hitsAny blurred 1. (1.9, 0.05)), "mblur-ray-at-close-sees-bent-pose")
    // Mid-shutter the tip is bent 45 degrees (a diagonal arm from (1, 0)).
    Assert.True(hitsAny blurred 0.5 (1.465, 0.535) && not (hitsAny blurred 0.5 (1.9, 0.05)), "mblur-ray-at-mid-sees-intermediate")
    // Shutter 0 reproduces the single pose at that instant.
    let closed = (Frame.sceneAt s 1. 1. 3).Shapes
    let reference = [ Skinning.pose mesh (AnimatedScene.worldMatrices s 1.) identityMatrix ]
    let grid = [ for i in 0 .. 17 do for j in 0 .. 22 -> -0.5 + 0.17 * float i, -0.5 + 0.13 * float j ]
    Assert.True(grid |> List.forall (fun p -> hitsAny closed 1. p = hitsAny reference 1. p), "mblur-zero-shutter-matches-single-pose")
    Assert.True(grid |> List.exists (hitsAny closed 1.), "mblur-zero-shutter-grid-hits")
    // One motion step (below two) also falls back to the mid-shutter pose.
    let single = (Frame.sceneAt s 0. 1. 1).Shapes
    let mid = [ Skinning.pose mesh (AnimatedScene.worldMatrices s 0.5) identityMatrix ]
    Assert.True(grid |> List.forall (fun p -> hitsAny single 0.5 p = hitsAny mid 0.5 p), "mblur-single-step-is-mid-pose")
    // A skin that does not move during the shutter is the mid-shutter pose, whatever the ray time.
    let hold = Clip.create "hold" [ Clip.rotate "tip" (Sampler.linear [ 0., quarter ]) ]
    let _, held = skinScene hold
    let still = (Frame.sceneAt held 0.2 0.8 3).Shapes
    let stillRef = [ Skinning.pose mesh (AnimatedScene.worldMatrices held 0.5) identityMatrix ]
    Assert.True(grid |> List.forall (fun p -> [ 0.2; 0.5; 0.8 ] |> List.forall (fun t -> hitsAny still t p = hitsAny stillRef t p)), "mblur-static-skin-matches-mid-pose")

let allTest () =
    slidingTests ()
    sceneTests ()
