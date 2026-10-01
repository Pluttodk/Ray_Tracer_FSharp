module AnimationTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Transformation
open Tracer.Animation

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance
let private nearVector tolerance (a: Vector) (b: Vector) =
    near tolerance a.X b.X && near tolerance a.Y b.Y && near tolerance a.Z b.Z
let private v x = Vector(x, 0., 0.)

let samplerTests () =
    let keys = [ 1., v 10.; 2., v 20.; 4., v 0. ]
    let linear = Sampler.linear keys
    let step = Sampler.step keys
    Assert.True(Sampler.evaluateVector linear 1.5 |> nearVector 1e-12 (v 15.), "linear-midpoint")
    Assert.True(Sampler.evaluateVector linear 3. |> nearVector 1e-12 (v 10.), "linear-uneven-spacing")
    Assert.True(Sampler.evaluateVector linear 0. |> nearVector 0. (v 10.) && Sampler.evaluateVector linear 9. |> nearVector 0. (v 0.), "linear-clamps")
    Assert.True(Sampler.evaluateVector step 1.99 |> nearVector 0. (v 10.) && Sampler.evaluateVector step 2. |> nearVector 0. (v 20.), "step-holds")
    // glTF 2.0 section 3.11 cubic spline: p(s) = h00 v_k + td h10 b_k + h01 v_k+1 + td h11 a_k+1,
    // with b_k the out-tangent of key k and a_k+1 the in-tangent of key k+1. Values by hand at s = 0.25, td = 2.
    let cubic = Sampler.cubic [ 0., v 99., v 1., v 3.; 2., v -5., v 4., v 99. ]
    let s = 0.25
    let h00, h10, h01, h11 = 2.*s*s*s - 3.*s*s + 1., s*s*s - 2.*s*s + s, -2.*s*s*s + 3.*s*s, s*s*s - s*s
    let expected = h00 * 1. + 2. * h10 * 3. + h01 * 4. + 2. * h11 * -5.
    Assert.True(Sampler.evaluateVector cubic 0.5 |> nearVector 1e-12 (v expected), "cubic-matches-gltf-formula")
    Assert.True(Sampler.evaluateVector cubic 0. |> nearVector 0. (v 1.) && Sampler.evaluateVector cubic 2. |> nearVector 0. (v 4.), "cubic-hits-keys")
    // Hermite tangents equal to the true derivative reproduce a parabola exactly.
    let g, launch = 9.81, 4.
    let parabola t = launch * t - 0.5 * g * t * t
    let apex = launch / g
    let ballistic = Sampler.cubic [ 0., v launch, v 0., v launch; apex, v 0., v (parabola apex), v 0.; 2. * apex, v -launch, v 0., v 0. ]
    let exact = [ 0.1; 0.3; 0.5; 0.7 ] |> List.forall (fun t -> near 1e-12 (Sampler.evaluateVector ballistic t).X (parabola t))
    Assert.True(exact, "cubic-reproduces-ballistic-arc")
    let rot = Sampler.linear [ 0., Quaternion.identity; 1., Quaternion.ofAxisAngle (Vector(0., 1., 0.)) (Math.PI / 2.) ]
    let half = Sampler.evaluateRotation rot 0.5
    Assert.True(abs (Quaternion.dot half (Quaternion.ofAxisAngle (Vector(0., 1., 0.)) (Math.PI / 4.))) |> near 1e-12 1., "rotation-track-slerps")
    let cubicRotation =
        Sampler.cubic [ 0., Quaternion.identity, Quaternion.identity, Quaternion.identity
                        1., Quaternion.identity, Quaternion.ofAxisAngle (Vector(1., 0., 0.)) 1., Quaternion.identity ]
    Assert.True(Quaternion.length (Sampler.evaluateRotation cubicRotation 0.4) |> near 1e-12 1., "cubic-rotation-normalised")
    let eased = Sampler.linear [ 0., v 0.; 1., v 1. ] |> Sampler.withEase Easing.quadIn
    Assert.True(Sampler.evaluateVector eased 0.5 |> nearVector 1e-12 (v 0.25), "ease-retimes-segment")
    Assert.Throws<ArgumentException>((fun () -> Sampler.linear [ 1., v 0.; 1., v 1. ] |> ignore), "sampler-rejects-duplicate-times")

let easingTests () =
    let curves =
        [ "linear", Easing.linear; "smoothstep", Easing.smoothstep; "quadInOut", Easing.quadInOut
          "cubicInOut", Easing.cubicInOut; "backIn", Easing.backIn; "backOut", Easing.backOut; "backInOut", Easing.backInOut
          "bezier", Easing.cubicBezier 0.25 0.1 0.25 1. ]
    for name, ease in curves do
        Assert.True(near 1e-9 (ease 0.) 0. && near 1e-9 (ease 1.) 1., $"ease-endpoints-{name}")
    let monotone ease = Seq.init 101 (fun i -> ease (float i / 100.)) |> Seq.pairwise |> Seq.forall (fun (a, b) -> b >= a - 1e-12)
    Assert.True([ Easing.smoothstep; Easing.quadInOut; Easing.cubicInOut; Easing.cubicBezier 0.42 0. 0.58 1. ] |> List.forall monotone, "ease-monotone")
    Assert.True(Easing.backIn 0.2 < 0. && Easing.backOut 0.8 > 1., "back-anticipates-and-overshoots")
    Assert.True(near 1e-9 (Easing.cubicBezier 0. 0. 1. 1. 0.5) 0.5, "bezier-symmetric-midpoint")

let private sphereNode name =
    Node.create name |> Node.withContent [ Geometry(SphereShape(Point.Zero, 0.5, Textures.mkMatTexture (MatteMaterial(Colour.White, 0., Colour.White, 1.)))) ]

let private testScene clips roots =
    { Name = "test"; Roots = roots; Clips = clips; ActiveCamera = "camera"; Cuts = []
      StaticShapes = []; StaticLights = []; Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Atmosphere = None; Duration = 1. }

let sceneGraphTests () =
    let child = sphereNode "child" |> Node.at 1. 0. 0.
    let parent = Node.create "parent" |> Node.withChildren [ child ]
    let camera = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig { CameraSpec.Default with Target = Some "child" } ]
    let spin = Clip.create "spin" [ Clip.rotate "parent" (Sampler.linear [ 0., Quaternion.identity; 1., Quaternion.ofAxisAngle (Vector(0., 1., 0.)) (Math.PI / 2.) ]) ]
    let scene = testScene [ spin ] [ parent; camera ] |> AnimatedScene.validate
    let world = AnimatedScene.worldMatrices scene 1.
    let childOrigin = transformPoint (Point.Zero, world.["child"])
    // Rotating the parent a quarter turn about +Y carries the child from +X to -Z.
    Assert.True(near 1e-12 childOrigin.X 0. && near 1e-12 childOrigin.Z -1., "hierarchy-composes-parent-rotation")
    let pose = AnimatedScene.cameraPose scene 1.
    Assert.True(near 1e-12 pose.LookAt.Z -1. && near 1e-12 pose.Position.Z 10., "camera-tracks-target")
    // Static frames get plain instancing; moving frames blur.
    let still = Frame.sceneAt scene 1. 1. 3
    let hitAt (s: Scene) x z time =
        s.Shapes |> List.exists (fun shape -> (shape.hitFunction (Ray(Point(x, 5., z), Vector(0., -1., 0.), time))).DidHit)
    Assert.True(hitAt still 0. -1. 1. && not (hitAt still 1. 0. 1.), "static-frame-pose")
    let moving = Frame.sceneAt scene 0. 1. 3
    Assert.True(hitAt moving 1. 0. 0. && hitAt moving 0. -1. 1. && not (hitAt moving 1. 0. 1.), "moving-frame-follows-ray-time")
    Assert.Throws<ArgumentException>((fun () -> testScene [] [ sphereNode "a"; sphereNode "a"; camera ] |> AnimatedScene.validate |> ignore), "duplicate-node-names-rejected")
    Assert.Throws<ArgumentException>((fun () -> testScene [ Clip.create "bad" [ Clip.translate "ghost" (Sampler.linear [ 0., Vector.Zero ]) ] ] [ camera ] |> AnimatedScene.validate |> ignore), "unknown-channel-node-rejected")
    let settings = { FrameSettings.Default with Fps = 24.; Shutter = 0.5 }
    let o, c = Frame.shutterInterval settings 12
    Assert.True(near 1e-12 o 0.5 && near 1e-12 c (0.5 + 0.5 / 24.), "shutter-interval-of-frame")

let demoTests () =
    for demo in Demos.all do
        let scene = demo.Build () |> AnimatedScene.validate
        let settings = { FrameSettings.Default with Width = 16; Height = 9; SamplesPerPixel = 1 }
        let film = Frame.render scene settings RenderOptions.Default (Frame.frameCount settings scene / 2)
        Assert.True(film.Pixels |> Array.forall Double.IsFinite && film.Pixels |> Array.exists (fun p -> p > 0.), $"demo-renders-{demo.Name}")

/// Static mesh instances baked into one world-space BVH hit and shade like the instances they replace.
let bakedStaticMeshTests () =
    let grid n =
        let positions = [| for y in 0 .. n do for x in 0 .. n -> Point(float x / float n - 0.5, 0.2 * sin (float (x + y)), float y / float n - 0.5) |]
        let uvs = [| for y in 0 .. n do for x in 0 .. n -> float x / float n, float y / float n |]
        let normals = positions |> Array.map (fun p -> Vector(0.1 * p.X, 1., 0.2 * p.Z).Normalise)
        let tris = [| for y in 0 .. n - 1 do
                        for x in 0 .. n - 1 do
                            let i = y * (n + 1) + x
                            yield! [| i; i + 1; i + n + 1; i + 1; i + n + 2; i + n + 1 |] |]
        TriangleMesh.fromArrays positions normals uvs tris true
    let texture = Textures.mkMatTexture (MatteMaterial(Colour.White, 1., Colour.White, 1.))
    let meshNode name (x: float) (y: float) (z: float) =
        let shape = (grid 6).toShape texture
        Node.create name |> Node.withContent [ Geometry shape ] |> Node.at x y z
    let camera = Node.create "camera" |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
    let roots = [ for i in 0 .. 3 -> meshNode $"m{i}" (float i * 0.7 - 1.) (float i * 0.3) (0.2 * float i) ] @ [ camera ]
    let make () = testScene [] roots |> AnimatedScene.validate
    let saved = Frame.flattenThreshold
    let instanced, baked =
        try
            Frame.flattenThreshold <- Int32.MaxValue
            let instanced = Frame.sceneAt (make ()) 0. 0. 2
            Frame.flattenThreshold <- 1
            instanced, Frame.sceneAt (make ()) 0. 0. 2
        finally Frame.flattenThreshold <- saved
    Assert.True(baked.Shapes.Length = 1 && instanced.Shapes.Length = 4, "baked-static-meshes-replace-instances")
    let closest (s: Scene) (ray: Ray) =
        s.Shapes |> List.map (fun shape -> shape.hitFunction ray) |> List.filter (fun h -> h.DidHit)
        |> List.sortBy (fun h -> h.Time) |> List.tryHead
    let rng = Random 7
    let mutable agree, hits = true, 0
    for _ in 1 .. 400 do
        let ray = Ray(Point(rng.NextDouble() * 3. - 2., 4., rng.NextDouble() * 2. - 1.), Vector(rng.NextDouble() * 0.4 - 0.2, -1., rng.NextDouble() * 0.4 - 0.2))
        match closest instanced ray, closest baked ray with
        | None, None -> ()
        | Some a, Some b ->
            hits <- hits + 1
            agree <- agree && near 1e-9 a.Time b.Time && nearVector 1e-9 a.Normal b.Normal
                     && nearVector 1e-9 a.ShadingNormal b.ShadingNormal && nearVector 1e-9 a.ShadowPoint.ToVector b.ShadowPoint.ToVector
                     && near 1e-9 a.U b.U && near 1e-9 a.V b.V
        | _ -> agree <- false
    Assert.True(agree && hits > 100, $"baked-static-meshes-hit-and-shade-like-instances ({hits} hits)")

let allTest () =
    samplerTests ()
    easingTests ()
    sceneGraphTests ()
    bakedStaticMeshTests ()
    demoTests ()
