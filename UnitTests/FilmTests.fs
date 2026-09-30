module FilmTests

open System
open System.IO
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Transformation
open Tracer.Animation

let private near tolerance (a: float) (b: float) = abs (a - b) <= tolerance
let private white = Textures.mkMatTexture (MatteMaterial(Colour.White, 0., Colour.White, 1.))
let private camera name = Node.create name |> Node.at 0. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
let private scene roots clips cuts =
    { Name = "t"; Roots = roots; Clips = clips; ActiveCamera = "camera"; Cuts = cuts; StaticShapes = []; StaticLights = []
      Ambient = AmbientLight(Colour.White, 0.); MaxBounces = 1; Duration = 2. }

/// A unit quad along +X bound to two joints: the root, and a joint at x = 1 that bends 90 degrees about Z.
let skinningTests () =
    let bend = Clip.create "bend" [ Clip.rotate "tip" (Sampler.linear [ 0., Quaternion.identity; 1., Quaternion.ofAxisAngle (Vector(0., 0., 1.)) (Math.PI / 2.) ]) ]
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
    let s = scene [ skeleton; holder; camera "camera" ] [ bend ] [] |> AnimatedScene.validate
    let posed t = Skinning.pose mesh (AnimatedScene.worldMatrices s t) identityMatrix
    let hits (shape: Shape) (x, y) = (shape.hitFunction (Ray(Point(x, y, 5.), Vector(0., 0., -1.)))).DidHit
    Assert.True(hits (posed 0.) (1.9, 0.05) && not (hits (posed 0.) (0.95, 0.9)), "skinning-bind-pose")
    // Bent 90 degrees: the far end (fully bound to the tip) now points up from x = 1.
    Assert.True(hits (posed 1.) (0.95, 0.9) && not (hits (posed 1.) (1.9, 0.05)), "skinning-follows-joint")
    // The skinned mesh ignores the carrying node's transform (glTF rule) when posed into world space.
    let frame = Frame.sceneAt s 1. 1. 2
    Assert.True(frame.Shapes |> List.exists (fun shape -> hits shape (0.95, 0.9)), "skinning-in-world-space")

let arrangeTests () =
    let ramp = Clip.create "ramp" [ Clip.translate "n" (Sampler.linear [ 0., Vector(0., 0., 0.); 1., Vector(1., 0., 0.) ]) ]
    let high = Clip.create "high" [ Clip.translate "n" (Sampler.linear [ 0., Vector(0., 10., 0.) ]) ]
    let rest _ = Trs.identity
    let looped = Clip.arrange "p" rest [ { Clip.segment ramp 0. with Loop = true; Blend = 0. } ] 3. 100.
    let x t = match looped.Channels.[0].Track with Translation s -> (Sampler.evaluateVector s t).X | _ -> nan
    Assert.True(near 1e-9 (x 0.5) 0.5 && near 1e-9 (x 1.5) 0.5 && near 1e-9 (x 2.25) 0.25, "arrange-loops")
    let fast = Clip.arrange "p" rest [ { Clip.segment ramp 0. with Speed = 2.; Blend = 0. } ] 1. 100.
    let fx t = match fast.Channels.[0].Track with Translation s -> (Sampler.evaluateVector s t).X | _ -> nan
    Assert.True(near 1e-9 (fx 0.25) 0.5 && near 1e-9 (fx 0.9) 1., "arrange-speed-and-hold")
    let switched = Clip.arrange "p" rest [ { Clip.segment ramp 0. with Blend = 0. }; { Clip.segment high 1. with Blend = 0.5 } ] 2. 100.
    let y t = match switched.Channels.[0].Track with Translation s -> (Sampler.evaluateVector s t).Y | _ -> nan
    Assert.True(near 1e-9 (y 0.9) 0. && near 1e-9 (y 1.25) 5. && near 1e-9 (y 1.8) 10., "arrange-crossfades")

let cutTests () =
    let a = camera "camera"
    let b = Node.create "other" |> Node.at 3. 0. 10. |> Node.withContent [ CameraRig CameraSpec.Default ]
    let s = scene [ a; b ] [] [ 1., "other" ] |> AnimatedScene.validate
    Assert.True(AnimatedScene.cameraAt s 0.99 = "camera" && AnimatedScene.cameraAt s 1. = "other", "camera-cut-switches")
    Assert.True(near 1e-12 (AnimatedScene.cameraPose s 1.5).Position.X 3., "camera-cut-pose")
    Assert.Throws<ArgumentException>((fun () -> scene [ a ] [] [ 1., "missing" ] |> AnimatedScene.validate |> ignore), "camera-cut-unknown-rejected")

let movingCameraTests () =
    let still = PinholeCamera(Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.), 1., 2., 2., 4, 4, regular 2, 0., 1.)
    let pose t = t, Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.)
    let moving = MovingPinholeCamera([| pose 0.; pose 1. |], 1., 2., 2., 4, 4, regular 2, 0., 1.)
    let same =
        [ for x in 0 .. 3 do for y in 0 .. 3 do for s in 0 .. 3 -> x, y, s ]
        |> List.forall (fun (x, y, s) ->
            let r1, r2 = still.CreateRay(x, y, s), (moving :> Camera).CreateRay(x, y, s)
            (r1.GetDirection - r2.GetDirection).Magnitude < 1e-12 && r1.ShutterTime = r2.ShutterTime)
    Assert.True(same, "moving-camera-matches-pinhole-when-still")
    let slide = MovingPinholeCamera([| 0., Point(0., 0., 5.), Point.Zero, Vector(0., 1., 0.); 1., Point(4., 0., 5.), Point(4., 0., 0.), Vector(0., 1., 0.) |], 1., 2., 2., 4, 4, regular 2, 0., 1.)
    let follows =
        [ 0 .. 3 ] |> List.forall (fun s -> let r = (slide :> Camera).CreateRay(1, 1, s) in near 1e-9 r.GetOrigin.X (4. * r.ShutterTime))
    Assert.True(follows, "moving-camera-origin-at-shutter-time")

let audioTests () =
    let l, r = Audio.panGains 0.
    Assert.True(near 1e-12 (l * l + r * r) 1. && near 1e-12 l r, "audio-equal-power-pan")
    let tone = Array.init 4800 (fun i -> sin (2. * Math.PI * 440. * float i / 48000.))
    Assert.True((Audio.repitch 0.5 tone).Length = 9600, "audio-repitch-length")
    let buffer = Audio.silence 1.
    Audio.mixMono buffer 0.5 tone (fun _ -> 1.) (fun _ -> -1.)
    Assert.True(buffer.Right |> Array.forall (fun x -> abs x < 1e-9) && buffer.Left.[24000 + 10] <> 0., "audio-mix-places-and-pans")
    let mastered = Audio.master { Left = Array.map ((*) 3.) buffer.Left; Right = buffer.Right } 0.89
    Assert.True(Seq.append mastered.Left mastered.Right |> Seq.forall (fun x -> abs x <= 0.89 + 1e-12), "audio-master-ceiling")
    let path = Path.Combine(Path.GetTempPath(), $"audio-test-{Environment.ProcessId}.wav")
    try
        Audio.writeWav path buffer
        let bytes = File.ReadAllBytes path
        Assert.True(bytes.Length = 44 + 48000 * 4 && Text.Encoding.ASCII.GetString(bytes, 0, 4) = "RIFF" && BitConverter.ToInt32(bytes, 24) = 48000, "audio-wav-header")
    finally File.Delete path

let allTest () =
    skinningTests ()
    arrangeTests ()
    cutTests ()
    movingCameraTests ()
    audioTests ()
