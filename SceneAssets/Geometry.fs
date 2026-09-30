namespace Tracer.SceneAssets

open System

[<Struct>]
type V3 =
    { X: float
      Y: float
      Z: float }

module V3 =
    let create x y z = { X = x; Y = y; Z = z }
    let zero = create 0. 0. 0.
    let add a b = create (a.X + b.X) (a.Y + b.Y) (a.Z + b.Z)
    let sub a b = create (a.X - b.X) (a.Y - b.Y) (a.Z - b.Z)
    let scale k a = create (k * a.X) (k * a.Y) (k * a.Z)
    let dot a b = a.X * b.X + a.Y * b.Y + a.Z * b.Z
    let cross a b = create (a.Y * b.Z - a.Z * b.Y) (a.Z * b.X - a.X * b.Z) (a.X * b.Y - a.Y * b.X)
    let length a = sqrt (dot a a)
    let unit a =
        let magnitude = length a
        if not (Double.IsFinite magnitude) || magnitude < 1e-14 then
            invalidArg (nameof a) "Cannot normalize a zero or non-finite vector."
        scale (1. / magnitude) a
    let lerp a b t = add (scale (1. - t) a) (scale t b)
    let toArray a = [| a.X; a.Y; a.Z |]
    let finite a = Double.IsFinite a.X && Double.IsFinite a.Y && Double.IsFinite a.Z

module Matrix =
    let identity =
        [| 1.; 0.; 0.; 0.
           0.; 1.; 0.; 0.
           0.; 0.; 1.; 0.
           0.; 0.; 0.; 1. |]

    let multiply (a: float array) (b: float array) =
        Array.init 16 (fun i ->
            let row, column = i / 4, i % 4
            [ 0 .. 3 ] |> List.sumBy (fun k -> a.[row * 4 + k] * b.[k * 4 + column]))

    let translation x y z =
        [| 1.; 0.; 0.; x
           0.; 1.; 0.; y
           0.; 0.; 1.; z
           0.; 0.; 0.; 1. |]

    let scale x y z =
        [| x; 0.; 0.; 0.
           0.; y; 0.; 0.
           0.; 0.; z; 0.
           0.; 0.; 0.; 1. |]

    let private radians degrees = degrees * Math.PI / 180.

    let rotateX degrees =
        let c, s = cos (radians degrees), sin (radians degrees)
        [| 1.; 0.; 0.; 0.; 0.; c; -s; 0.; 0.; s; c; 0.; 0.; 0.; 0.; 1. |]

    let rotateY degrees =
        let c, s = cos (radians degrees), sin (radians degrees)
        [| c; 0.; s; 0.; 0.; 1.; 0.; 0.; -s; 0.; c; 0.; 0.; 0.; 0.; 1. |]

    let rotateZ degrees =
        let c, s = cos (radians degrees), sin (radians degrees)
        [| c; -s; 0.; 0.; s; c; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1. |]

    let compose (matrices: float array list) =
        List.fold multiply identity matrices

    let trs (x, y, z) (sx, sy, sz) (rx, ry, rz) =
        compose [ translation x y z; rotateZ rz; rotateY ry; rotateX rx; scale sx sy sz ]

    let place position size = trs position size (0., 0., 0.)

    let between (a: V3) (b: V3) radiusX radiusZ =
        let direction = V3.sub b a
        let distance = V3.length direction
        let y = V3.unit direction
        let reference = if abs y.Z < 0.9 then V3.create 0. 0. 1. else V3.create 1. 0. 0.
        let x = V3.cross y reference |> V3.unit
        let z = V3.cross x y |> V3.unit
        let center = V3.lerp a b 0.5
        [| x.X * radiusX; y.X * distance / 2.; z.X * radiusZ; center.X
           x.Y * radiusX; y.Y * distance / 2.; z.Y * radiusZ; center.Y
           x.Z * radiusX; y.Z * distance / 2.; z.Z * radiusZ; center.Z
           0.; 0.; 0.; 1. |]

    let point (matrix: float array) (p: V3) =
        V3.create
            (matrix.[0] * p.X + matrix.[1] * p.Y + matrix.[2] * p.Z + matrix.[3])
            (matrix.[4] * p.X + matrix.[5] * p.Y + matrix.[6] * p.Z + matrix.[7])
            (matrix.[8] * p.X + matrix.[9] * p.Y + matrix.[10] * p.Z + matrix.[11])
