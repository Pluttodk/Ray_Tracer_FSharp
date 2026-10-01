namespace Tracer.Animation

open System
open System.Collections.Generic
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Transformation

/// A perspective camera in glTF terms: it looks down its node's local -Z with +Y up.
type CameraSpec =
    { /// Vertical field of view in radians.
      YFov: float
      /// Thin-lens aperture radius; 0 is a pinhole.
      ApertureRadius: float
      /// Distance to the plane in focus (thin lens only).
      FocusDistance: float
      /// Optional node the camera keeps aimed at, overriding the node's own rotation.
      Target: string option
      /// Node to autofocus on: the focus distance becomes the distance from the camera to that node at
      /// mid-shutter, overriding FocusDistance. None keeps the fixed FocusDistance.
      FocusTarget: string option }
    static member Default = { YFov = 0.7; ApertureRadius = 0.; FocusDistance = 10.; Target = None; FocusTarget = None }

/// A mesh deformed by a skeleton (glTF skinning). Each vertex follows up to four joints; as in glTF, the
/// vertices end up in world space and the transform of the node carrying the mesh is ignored.
type SkinnedMesh =
    { Positions: Point[]
      Normals: Vector[]
      Uvs: (float * float)[]
      Triangles: int[]
      /// Four joint slots per vertex, indexing JointNodes.
      Joints: int[]
      /// Four weights per vertex, matching Joints.
      Weights: float[]
      /// Names of the nodes acting as joints.
      JointNodes: string[]
      /// Per joint: the inverse of its world matrix in the bind pose.
      InverseBind: QuickMatrix[]
      Texture: Textures.Texture }

module Skinning =
    /// Poses the mesh with the given world matrices and returns it in the frame whose world matrix is `frame`
    /// (so it can be instanced, and motion-blurred, like rigid geometry of that node).
    let pose (mesh: SkinnedMesh) (world: IDictionary<string, QuickMatrix>) (frame: QuickMatrix) : Shape =
        let joints = Array.init mesh.JointNodes.Length (fun i -> QuickMatrix.multi (world.[mesh.JointNodes.[i]], mesh.InverseBind.[i]))
        let toFrame = getInvMatrix (ofAffine frame)
        let n = mesh.Positions.Length
        let positions = Array.zeroCreate<Point> n
        let normals = Array.zeroCreate<Vector> n
        for v in 0 .. n - 1 do
            // Weighted sum of the joint matrices' 3x4 parts (linear blend skinning).
            let m = Array.zeroCreate<float> 12
            let mutable total = 0.
            for k in 0 .. 3 do
                let w = mesh.Weights.[4 * v + k]
                if w > 0. then
                    total <- total + w
                    let j = joints.[mesh.Joints.[4 * v + k]]
                    m.[0] <- m.[0] + w * j.Pos1x1; m.[1] <- m.[1] + w * j.Pos1x2; m.[2] <- m.[2] + w * j.Pos1x3; m.[3] <- m.[3] + w * j.Pos1x4
                    m.[4] <- m.[4] + w * j.Pos2x1; m.[5] <- m.[5] + w * j.Pos2x2; m.[6] <- m.[6] + w * j.Pos2x3; m.[7] <- m.[7] + w * j.Pos2x4
                    m.[8] <- m.[8] + w * j.Pos3x1; m.[9] <- m.[9] + w * j.Pos3x2; m.[10] <- m.[10] + w * j.Pos3x3; m.[11] <- m.[11] + w * j.Pos3x4
            let scale = if total > 0. then 1. / total else 1.
            let p = mesh.Positions.[v]
            let world =
                if total = 0. then p
                else Point(scale * (m.[0] * p.X + m.[1] * p.Y + m.[2] * p.Z + m.[3]),
                           scale * (m.[4] * p.X + m.[5] * p.Y + m.[6] * p.Z + m.[7]),
                           scale * (m.[8] * p.X + m.[9] * p.Y + m.[10] * p.Z + m.[11]))
            positions.[v] <- transformPoint (world, toFrame)
            if mesh.Normals.Length > 0 then
                let q = mesh.Normals.[v]
                let worldNormal =
                    if total = 0. then q
                    else Vector(m.[0] * q.X + m.[1] * q.Y + m.[2] * q.Z, m.[4] * q.X + m.[5] * q.Y + m.[6] * q.Z, m.[8] * q.X + m.[9] * q.Y + m.[10] * q.Z)
                // Normals go into the frame by the inverse transpose of its inverse, i.e. the frame's transpose.
                normals.[v] <- transformVector (worldNormal, (getMatrix (ofAffine frame)).transpose)
        let hasNormals = mesh.Normals.Length > 0
        (TriangleMesh.fromArrays positions (if hasNormals then normals else [||]) mesh.Uvs mesh.Triangles hasNormals).toShape mesh.Texture

/// What a node carries. Geometry and lights are authored in the node's local space.
type Content =
    /// Built once and re-instanced every frame, so meshes keep their internal acceleration structure.
    | Geometry of Shape
    /// A skeleton-deformed mesh, re-posed every frame (at mid-shutter).
    | Skinned of SkinnedMesh
    | LightSource of Light
    | CameraRig of CameraSpec

type Node =
    { Name: string
      Rest: Trs
      Content: Content list
      Children: Node list }

module Node =
    let create name = { Name = name; Rest = Trs.identity; Content = []; Children = [] }
    let withRest rest node = { node with Rest = rest }
    let at x y z node = { node with Rest = { node.Rest with Translation = Vector(x, y, z) } }
    let withContent content node = { node with Content = node.Content @ content }
    let withChildren children node = { node with Children = node.Children @ children }

    let rec descendants (node: Node) = seq { yield node; for child in node.Children do yield! descendants child }

/// An animated scene: a node hierarchy, the clips that drive it, and the environment around it.
type AnimatedScene =
    { Name: string
      Roots: Node list
      Clips: Clip list
      /// Name of the node carrying the active camera.
      ActiveCamera: string
      /// Camera cuts: from each time on, the named camera node is active (before the first cut, ActiveCamera).
      Cuts: (float * string) list
      /// Shapes that never move; they are passed straight to every frame without instancing.
      StaticShapes: Shape list
      StaticLights: Light list
      Ambient: AmbientLight
      MaxBounces: int
      /// Height fog / aerial perspective applied to every frame; None for none.
      Atmosphere: Atmosphere option
      /// Natural length of the animation in seconds.
      Duration: float }

/// A camera placement resolved for one frame.
type CameraPose = { Position: Point; LookAt: Point; Up: Vector; Spec: CameraSpec }

module AnimatedScene =
    let nodes (scene: AnimatedScene) = scene.Roots |> Seq.collect Node.descendants

    let validate (scene: AnimatedScene) =
        let names = nodes scene |> Seq.map (fun node -> node.Name) |> Array.ofSeq
        match names |> Array.countBy id |> Array.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> invalidArg (nameof scene) $"Node name {name} is used more than once."
        | None -> ()
        let known = HashSet names
        for clip in scene.Clips do
            for channel in clip.Channels do
                if not (known.Contains channel.Node) then
                    invalidArg (nameof scene) $"Clip {clip.Name} animates unknown node {channel.Node}."
        for name in scene.ActiveCamera :: (scene.Cuts |> List.map snd) do
            match nodes scene |> Seq.tryFind (fun node -> node.Name = name) with
            | Some node when node.Content |> List.exists (function CameraRig _ -> true | _ -> false) -> ()
            | _ -> invalidArg (nameof scene) $"Camera node {name} does not exist or has no camera."
        if scene.Cuts |> List.pairwise |> List.exists (fun ((a, _), (b, _)) -> b <= a) then
            invalidArg (nameof scene) "Camera cuts must be in increasing time order."
        scene

    /// The camera node active at time t, following the cuts.
    let cameraAt (scene: AnimatedScene) (t: float) =
        scene.Cuts |> List.fold (fun active (time, name) -> if time <= t then name else active) scene.ActiveCamera

    /// Every node's world matrix at time t.
    let worldMatrices (scene: AnimatedScene) (t: float) =
        let pose = Dictionary<string, Trs>()
        for node in nodes scene do pose.[node.Name] <- node.Rest
        for clip in scene.Clips do Clip.apply clip t pose
        let world = Dictionary<string, QuickMatrix>()
        let rec walk (parent: QuickMatrix) (node: Node) =
            let matrix = QuickMatrix.multi (parent, Trs.toMatrix pose.[node.Name])
            world.[node.Name] <- matrix
            for child in node.Children do walk matrix child
        for root in scene.Roots do walk identityMatrix root
        world

    let private origin (m: QuickMatrix) = Point(m.Pos1x4, m.Pos2x4, m.Pos3x4)

    let cameraPose (scene: AnimatedScene) (t: float) =
        let world = worldMatrices scene t
        let active = cameraAt scene t
        let node = nodes scene |> Seq.find (fun node -> node.Name = active)
        let spec = node.Content |> List.pick (function CameraRig spec -> Some spec | _ -> None)
        let m = world.[node.Name]
        let position = origin m
        let lookAt =
            match spec.Target with
            | Some target ->
                match world.TryGetValue target with
                | true, targetMatrix -> origin targetMatrix
                | _ -> invalidArg (nameof scene) $"Camera target {target} does not exist."
            | None -> position + (transformVector (Vector(0., 0., -1.), m)).Normalise
        let spec =
            match spec.FocusTarget with
            | Some focus ->
                match world.TryGetValue focus with
                | true, focusMatrix -> { spec with FocusDistance = max 1e-6 (origin focusMatrix - position).Magnitude }
                | _ -> invalidArg (nameof scene) $"Camera focus target {focus} does not exist."
            | None -> spec
        { Position = position; LookAt = lookAt; Up = (transformVector (Vector(0., 1., 0.), m)).Normalise; Spec = spec }

/// Per-frame render parameters.
type FrameSettings =
    { Width: int
      Height: int
      /// Samples per pixel; rounded up to a square for the multi-jittered sampler.
      SamplesPerPixel: int
      /// Shutter fraction of the frame interval: 0 is no blur, 0.5 is the film-standard 180 degree shutter.
      Shutter: float
      /// Poses sampled across the shutter for moving objects (at least 2). More follows fast arcs better.
      MotionSteps: int
      Fps: float }
    static member Default = { Width = 640; Height = 360; SamplesPerPixel = 16; Shutter = 0.5; MotionSteps = 3; Fps = 24. }

module Frame =
    let private closeTo (a: QuickMatrix) (b: QuickMatrix) =
        let d (x: float) (y: float) = abs (x - y) <= 1e-12 * (1. + abs x)
        d a.Pos1x1 b.Pos1x1 && d a.Pos1x2 b.Pos1x2 && d a.Pos1x3 b.Pos1x3 && d a.Pos1x4 b.Pos1x4
        && d a.Pos2x1 b.Pos2x1 && d a.Pos2x2 b.Pos2x2 && d a.Pos2x3 b.Pos2x3 && d a.Pos2x4 b.Pos2x4
        && d a.Pos3x1 b.Pos3x1 && d a.Pos3x2 b.Pos3x2 && d a.Pos3x3 b.Pos3x3 && d a.Pos3x4 b.Pos3x4

    let private isIdentity m = closeTo m identityMatrix

    /// The static scene visible during the shutter interval [shutterOpen, shutterClose].
    let sceneAt (scene: AnimatedScene) (shutterOpen: float) (shutterClose: float) (motionSteps: int) =
        let times =
            if shutterClose <= shutterOpen then [| shutterOpen |]
            else
                let steps = max 2 motionSteps
                Array.init steps (fun i -> shutterOpen + (shutterClose - shutterOpen) * float i / float (steps - 1))
        let worlds = times |> Array.map (AnimatedScene.worldMatrices scene)
        let middle = AnimatedScene.worldMatrices scene (0.5 * (shutterOpen + shutterClose))
        let shapes = ResizeArray<Shape>(scene.StaticShapes)
        let lights = ResizeArray<Light>(scene.StaticLights)
        for node in AnimatedScene.nodes scene do
            for content in node.Content do
                match content with
                | Geometry shape ->
                    let matrices = worlds |> Array.map (fun world -> world.[node.Name])
                    if matrices |> Array.forall (closeTo matrices.[0]) then
                        if isIdentity matrices.[0] then shapes.Add shape
                        else shapes.Add(Transform.transform shape (ofAffine matrices.[0]))
                    else
                        shapes.Add(MotionTransform.transform shape (AnimatedTransform(Array.zip times matrices)))
                | Skinned mesh ->
                    // Pose the skin at mid-shutter in the node's own frame, then instance it like rigid geometry,
                    // so the node's (flight) motion still blurs; the deformation itself is not blurred.
                    let shape = Skinning.pose mesh middle middle.[node.Name]
                    let matrices = worlds |> Array.map (fun world -> world.[node.Name])
                    if matrices |> Array.forall (closeTo matrices.[0]) then
                        shapes.Add(Transform.transform shape (ofAffine matrices.[0]))
                    else
                        shapes.Add(MotionTransform.transform shape (AnimatedTransform(Array.zip times matrices)))
                | LightSource light ->
                    // Lights are placed at mid-shutter; light motion blur is not modelled.
                    let m = middle.[node.Name]
                    lights.Add(if isIdentity m then light else TransformLight.transformLight light (ofAffine m))
                | CameraRig _ -> ()
        Scene(List.ofSeq shapes, List.ofSeq lights, scene.Ambient, scene.MaxBounces, ?atmosphere = scene.Atmosphere)

    let camera (scene: AnimatedScene) (settings: FrameSettings) (shutterOpen: float) (shutterClose: float) =
        let pose = AnimatedScene.cameraPose scene (0.5 * (shutterOpen + shutterClose))
        let side = max 1 (int (ceil (sqrt (float settings.SamplesPerPixel))))
        let height = 2. * tan (pose.Spec.YFov / 2.)
        let width = height * float settings.Width / float settings.Height
        // A pinhole that moves during the shutter blurs the world relative to it, not just moving objects.
        let poses =
            if shutterClose <= shutterOpen then [||]
            else
                let steps = max 2 settings.MotionSteps
                Array.init steps (fun i ->
                    let t = shutterOpen + (shutterClose - shutterOpen) * float i / float (steps - 1)
                    let p = AnimatedScene.cameraPose scene t
                    t, p.Position, p.LookAt, p.Up)
        let moving =
            poses.Length > 1
            && poses |> Array.exists (fun (_, p, l, u) ->
                let _, p0, l0, u0 = poses.[0]
                (p - p0).Magnitude > 1e-9 || (l - l0).Magnitude > 1e-9 || (u - u0).Magnitude > 1e-9)
        if moving && pose.Spec.ApertureRadius = 0. then
            MovingPinholeCamera(poses, 1., width, height, settings.Width, settings.Height, multiJittered side 83, shutterOpen, shutterClose)
            :> Tracer.Basics.Camera
        elif moving then
            MovingThinLensCamera(poses, 1., width, height, settings.Width, settings.Height,
                                 pose.Spec.ApertureRadius, pose.Spec.FocusDistance,
                                 multiJittered side 83, multiJittered side 89, shutterOpen, shutterClose)
            :> Tracer.Basics.Camera
        elif pose.Spec.ApertureRadius > 0. then
            ThinLensCamera(pose.Position, pose.LookAt, pose.Up, 1., width, height, settings.Width, settings.Height,
                           pose.Spec.ApertureRadius, pose.Spec.FocusDistance,
                           multiJittered side 83, multiJittered side 89, shutterOpen, shutterClose) :> Tracer.Basics.Camera
        else
            PinholeCamera(pose.Position, pose.LookAt, pose.Up, 1., width, height, settings.Width, settings.Height,
                          multiJittered side 83, shutterOpen, shutterClose) :> Tracer.Basics.Camera

    /// Shutter interval of frame `index` (frame 0 opens at t = 0).
    let shutterInterval (settings: FrameSettings) (index: int) =
        let t = float index / settings.Fps
        t, t + settings.Shutter / settings.Fps

    let frameCount (settings: FrameSettings) (scene: AnimatedScene) =
        max 1 (int (ceil (scene.Duration * settings.Fps - 1e-9)))

    /// Renders frame `index` to a linear film.
    let render (scene: AnimatedScene) (settings: FrameSettings) (options: RenderOptions) (index: int) =
        let shutterOpen, shutterClose = shutterInterval settings index
        let frameScene = sceneAt scene shutterOpen shutterClose settings.MotionSteps
        let camera = camera scene settings shutterOpen shutterClose
        Render.Render(frameScene, camera, options).RenderLinear
