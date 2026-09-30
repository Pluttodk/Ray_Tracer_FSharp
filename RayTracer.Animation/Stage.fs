namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Textures
open Tracer.Basics.Transformation

/// Building blocks shared by the demo scenes.
module Stage =
    let rgb r g b = Colour(r, g, b)

    let matte (colour: Colour) = MatteMaterial(colour, 0.25, colour, 0.8) :> Material

    let plastic (colour: Colour) =
        PhongMaterial(colour, 0.2, colour, 0.8, Colour.White, 0.35, 40) :> Material

    let glossy (colour: Colour) =
        PhongReflectiveMaterial(colour, 0.15, colour, 0.7, Colour.White, 0.4, Colour.White, 0.15, 80) :> Material

    let solid material = mkMatTexture material

    /// Alternating materials on a u/v grid; `uCells` by `vCells` cells over the unit square.
    let checker uCells vCells (a: Material) (b: Material) =
        mkTexture (fun u v ->
            let cell = int (floor (u * float uCells)) + int (floor (v * float vCells))
            if cell % 2 = 0 then a else b)
        |> markOpaque

    /// Checker with square cells of `size` world units, for the unbounded ground (whose UVs are world X/Z).
    let floorChecker size (a: Material) (b: Material) =
        mkTexture (fun u v ->
            let cell = int (floor (u / size)) + int (floor (v / size))
            if cell % 2 = 0 then a else b)
        |> markOpaque

    /// A node holding the unbounded y = 0 ground plane, facing +Y. Its UVs are world X and -Z.
    let groundNode texture =
        Node.create "ground"
        |> Node.withRest { Trs.identity with Rotation = Quaternion.ofAxisAngle (Vector(1., 0., 0.)) (-Math.PI / 2.) }
        |> Node.withContent [ Geometry(InfinitePlane(texture)) ]

    /// An axis-aligned box from `low` to `high` with one texture on every face.
    let box (low: Point) (high: Point) texture =
        Box(low, high, texture, texture, texture, texture, texture, texture) :> Shape

    /// A node holding a box matching a physics box collider.
    let colliderNode name (centre: Point) (half: Vector) (rotation: Quaternion) texture =
        Node.create name
        |> Node.withRest { Translation = Vector(centre.X, centre.Y, centre.Z); Rotation = rotation; Scale = Vector(1., 1., 1.) }
        |> Node.withContent [ Geometry(box (Point(-half.X, -half.Y, -half.Z)) (Point(half.X, half.Y, half.Z)) texture) ]

    let sphere radius texture = SphereShape(Point.Zero, radius, texture) :> Shape

    let sun (direction: Vector) intensity = DirectionalLight(Colour.White, intensity, direction.Normalise) :> Light

    let lamp (position: Point) intensity = PointLight(Colour.White, intensity, position) :> Light

    let ambient intensity = AmbientLight(Colour.White, intensity)

    /// A gradient sky dome: horizon haze fading to `zenith` overhead, lighting the scene softly from above.
    /// `samples` is the per-axis count of sky directions sampled at each shading point.
    let sky (horizon: Colour) (zenith: Colour) intensity samples =
        let emissive (c: Colour) = EmissiveMaterial(c, intensity) :> Material
        let below = emissive (horizon * 0.35)
        let texture =
            mkTexture (fun _ v ->
                if v < 0.5 then below
                else
                    let h = min 1. ((v - 0.5) * 2.)
                    let t = 1. - (1. - h) * (1. - h) * (1. - h)
                    emissive (horizon * (1. - t) + zenith * t))
        EnvironmentLight(1e6, texture, multiJittered samples 17) :> Light

    /// A camera node at `position` aimed at `target` (a node name).
    let cameraNode name (position: Point) (spec: CameraSpec) =
        Node.create name
        |> Node.at position.X position.Y position.Z
        |> Node.withContent [ CameraRig spec ]
