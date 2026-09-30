namespace Tracer.Basics

open System
open Tracer.Basics.Transformation

/// Unit quaternion (X, Y, Z imaginary, W real) representing a rotation. Matches glTF's component order.
[<Struct>]
type Quaternion =
    { X: float; Y: float; Z: float; W: float }

module Quaternion =
    let identity = { X = 0.; Y = 0.; Z = 0.; W = 1. }

    let dot (a: Quaternion) (b: Quaternion) = a.X * b.X + a.Y * b.Y + a.Z * b.Z + a.W * b.W

    let length q = sqrt (dot q q)

    let normalise (q: Quaternion) =
        let l = length q
        if l = 0. || not (Double.IsFinite l) then invalidArg (nameof q) "A rotation quaternion must be finite and nonzero."
        { X = q.X / l; Y = q.Y / l; Z = q.Z / l; W = q.W / l }

    let conjugate (q: Quaternion) = { X = -q.X; Y = -q.Y; Z = -q.Z; W = q.W }

    let negate (q: Quaternion) = { X = -q.X; Y = -q.Y; Z = -q.Z; W = -q.W }

    /// Hamilton product: the result rotates by b first, then by a.
    let multiply (a: Quaternion) (b: Quaternion) =
        { X = a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y
          Y = a.W * b.Y - a.X * b.Z + a.Y * b.W + a.Z * b.X
          Z = a.W * b.Z + a.X * b.Y - a.Y * b.X + a.Z * b.W
          W = a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z }

    let ofAxisAngle (axis: Vector) (angle: float) =
        if not axis.IsFinite || axis.IsZero || not (Double.IsFinite angle) then
            invalidArg (nameof axis) "A rotation needs a finite, nonzero axis and a finite angle."
        let a = axis.Normalise
        let s = sin (angle / 2.)
        { X = a.X * s; Y = a.Y * s; Z = a.Z * s; W = cos (angle / 2.) }

    /// Rotation about X, then Y, then Z (radians), matching mergeTransformations [rotateX; rotateY; rotateZ].
    let ofEuler x y z =
        multiply (ofAxisAngle (Vector(0., 0., 1.)) z)
            (multiply (ofAxisAngle (Vector(0., 1., 0.)) y) (ofAxisAngle (Vector(1., 0., 0.)) x))

    let rotate (q: Quaternion) (v: Vector) =
        // v' = v + 2w(u x v) + 2u x (u x v), with u the vector part.
        let u = Vector(q.X, q.Y, q.Z)
        let t = 2. * (u % v)
        v + q.W * t + (u % t)

    /// The rotation as the upper 3x3 of an affine matrix.
    let toMatrix (q: Quaternion) =
        let q = normalise q
        let xx, yy, zz = q.X * q.X, q.Y * q.Y, q.Z * q.Z
        let xy, xz, yz = q.X * q.Y, q.X * q.Z, q.Y * q.Z
        let wx, wy, wz = q.W * q.X, q.W * q.Y, q.W * q.Z
        { identityMatrix with
            Pos1x1 = 1. - 2. * (yy + zz); Pos1x2 = 2. * (xy - wz); Pos1x3 = 2. * (xz + wy)
            Pos2x1 = 2. * (xy + wz); Pos2x2 = 1. - 2. * (xx + zz); Pos2x3 = 2. * (yz - wx)
            Pos3x1 = 2. * (xz - wy); Pos3x2 = 2. * (yz + wx); Pos3x3 = 1. - 2. * (xx + yy) }

    /// Rotation from the upper 3x3 of a matrix that is (numerically) a proper rotation.
    let ofRotationMatrix (m: QuickMatrix) =
        let trace = m.Pos1x1 + m.Pos2x2 + m.Pos3x3
        let q =
            if trace > 0. then
                let s = 2. * sqrt (trace + 1.)
                { W = 0.25 * s; X = (m.Pos3x2 - m.Pos2x3) / s; Y = (m.Pos1x3 - m.Pos3x1) / s; Z = (m.Pos2x1 - m.Pos1x2) / s }
            elif m.Pos1x1 > m.Pos2x2 && m.Pos1x1 > m.Pos3x3 then
                let s = 2. * sqrt (1. + m.Pos1x1 - m.Pos2x2 - m.Pos3x3)
                { W = (m.Pos3x2 - m.Pos2x3) / s; X = 0.25 * s; Y = (m.Pos1x2 + m.Pos2x1) / s; Z = (m.Pos1x3 + m.Pos3x1) / s }
            elif m.Pos2x2 > m.Pos3x3 then
                let s = 2. * sqrt (1. + m.Pos2x2 - m.Pos1x1 - m.Pos3x3)
                { W = (m.Pos1x3 - m.Pos3x1) / s; X = (m.Pos1x2 + m.Pos2x1) / s; Y = 0.25 * s; Z = (m.Pos2x3 + m.Pos3x2) / s }
            else
                let s = 2. * sqrt (1. + m.Pos3x3 - m.Pos1x1 - m.Pos2x2)
                { W = (m.Pos2x1 - m.Pos1x2) / s; X = (m.Pos1x3 + m.Pos3x1) / s; Y = (m.Pos2x3 + m.Pos3x2) / s; Z = 0.25 * s }
        normalise q

    /// Normalised linear interpolation along the shortest arc.
    let nlerp (a: Quaternion) (b: Quaternion) t =
        let b = if dot a b < 0. then negate b else b
        normalise
            { X = a.X + t * (b.X - a.X); Y = a.Y + t * (b.Y - a.Y)
              Z = a.Z + t * (b.Z - a.Z); W = a.W + t * (b.W - a.W) }

    /// Spherical linear interpolation along the shortest arc.
    let slerp (a: Quaternion) (b: Quaternion) t =
        let d = dot a b
        let b, d = if d < 0. then negate b, -d else b, d
        if d > 0.9995 then nlerp a b t
        else
            let theta = acos (min 1. d)
            let sinTheta = sin theta
            let wa = sin ((1. - t) * theta) / sinTheta
            let wb = sin (t * theta) / sinTheta
            normalise
                { X = wa * a.X + wb * b.X; Y = wa * a.Y + wb * b.Y
                  Z = wa * a.Z + wb * b.Z; W = wa * a.W + wb * b.W }

    /// Rotation that maps local -Z to `forward` and local +Y towards `up` (the glTF camera convention).
    let lookRotation (forward: Vector) (up: Vector) =
        if not forward.IsFinite || forward.IsZero then invalidArg (nameof forward) "A look direction must be finite and nonzero."
        let back = (-forward).Normalise
        let up =
            let up = if up.IsFinite && not up.IsZero then up.Normalise else Vector(0., 1., 0.)
            if abs (up * back) < 0.999999 then up
            elif abs back.Y < 0.9 then Vector(0., 1., 0.)
            else Vector(1., 0., 0.)
        let right = (up % back).Normalise
        let trueUp = back % right
        ofRotationMatrix
            { identityMatrix with
                Pos1x1 = right.X; Pos1x2 = trueUp.X; Pos1x3 = back.X
                Pos2x1 = right.Y; Pos2x2 = trueUp.Y; Pos2x3 = back.Y
                Pos3x1 = right.Z; Pos3x2 = trueUp.Z; Pos3x3 = back.Z }

/// Translation, rotation and scale, composed as T * R * S (the glTF node convention).
type Trs =
    { Translation: Vector; Rotation: Quaternion; Scale: Vector }

module Trs =
    let identity = { Translation = Vector.Zero; Rotation = Quaternion.identity; Scale = Vector(1., 1., 1.) }

    let ofTranslation x y z = { identity with Translation = Vector(x, y, z) }

    let toMatrix (trs: Trs) =
        let r = Quaternion.toMatrix trs.Rotation
        let s = trs.Scale
        { r with
            Pos1x1 = r.Pos1x1 * s.X; Pos1x2 = r.Pos1x2 * s.Y; Pos1x3 = r.Pos1x3 * s.Z; Pos1x4 = trs.Translation.X
            Pos2x1 = r.Pos2x1 * s.X; Pos2x2 = r.Pos2x2 * s.Y; Pos2x3 = r.Pos2x3 * s.Z; Pos2x4 = trs.Translation.Y
            Pos3x1 = r.Pos3x1 * s.X; Pos3x2 = r.Pos3x2 * s.Y; Pos3x3 = r.Pos3x3 * s.Z; Pos3x4 = trs.Translation.Z }

    let toTransformation (trs: Trs) = ofAffine (toMatrix trs)

    let lerp (a: Trs) (b: Trs) t =
        { Translation = a.Translation + t * (b.Translation - a.Translation)
          Rotation = Quaternion.slerp a.Rotation b.Rotation t
          Scale = a.Scale + t * (b.Scale - a.Scale) }

/// An affine matrix split as T * R * S, where S is a general (symmetric) stretch. Unlike a TRS triple this
/// represents any affine matrix, including world matrices under non-uniformly scaled, rotated parents.
type private Decomposed =
    { T: Vector; R: Quaternion; S: float[] }

/// A transform keyed at increasing times across a shutter interval. Between keys, translation and stretch
/// are interpolated linearly and rotation spherically, so rotating objects blur along arcs, not chords.
type AnimatedTransform(keys: (float * QuickMatrix)[]) =
    do
        if isNull keys || keys.Length = 0 then invalidArg (nameof keys) "An animated transform needs at least one key."
        for i in 1 .. keys.Length - 1 do
            if not (fst keys.[i] > fst keys.[i - 1]) then
                invalidArg (nameof keys) "Animated transform keys must have strictly increasing times."

    static let invert3 (a: float[]) =
        let det =
            a.[0] * (a.[4] * a.[8] - a.[5] * a.[7])
            - a.[1] * (a.[3] * a.[8] - a.[5] * a.[6])
            + a.[2] * (a.[3] * a.[7] - a.[4] * a.[6])
        [| (a.[4] * a.[8] - a.[5] * a.[7]) / det; (a.[2] * a.[7] - a.[1] * a.[8]) / det; (a.[1] * a.[5] - a.[2] * a.[4]) / det
           (a.[5] * a.[6] - a.[3] * a.[8]) / det; (a.[0] * a.[8] - a.[2] * a.[6]) / det; (a.[2] * a.[3] - a.[0] * a.[5]) / det
           (a.[3] * a.[7] - a.[4] * a.[6]) / det; (a.[1] * a.[6] - a.[0] * a.[7]) / det; (a.[0] * a.[4] - a.[1] * a.[3]) / det |]

    static let multiply3 (a: float[]) (b: float[]) =
        Array.init 9 (fun i ->
            let r, c = i / 3, i % 3
            a.[3 * r] * b.[c] + a.[3 * r + 1] * b.[3 + c] + a.[3 * r + 2] * b.[6 + c])

    static let decompose (m: QuickMatrix) =
        let a = [| m.Pos1x1; m.Pos1x2; m.Pos1x3; m.Pos2x1; m.Pos2x2; m.Pos2x3; m.Pos3x1; m.Pos3x2; m.Pos3x3 |]
        let det =
            a.[0] * (a.[4] * a.[8] - a.[5] * a.[7])
            - a.[1] * (a.[3] * a.[8] - a.[5] * a.[6])
            + a.[2] * (a.[3] * a.[7] - a.[4] * a.[6])
        if det = 0. || not (Double.IsFinite det) then invalidArg "keys" "Animated transform keys must be invertible."
        // A mirrored matrix has no rotation factor; decompose its negation and fold the sign into the stretch.
        let sign = if det < 0. then -1. else 1.
        let a' = Array.map (fun x -> sign * x) a
        // Polar decomposition by Newton iteration: R <- (R + R^-T) / 2.
        let mutable r = Array.copy a'
        let mutable iteration = 0
        let mutable converged = false
        while not converged && iteration < 100 do
            let inverse = invert3 r
            let next = Array.init 9 (fun i -> 0.5 * (r.[i] + inverse.[(i % 3) * 3 + i / 3]))
            let change = Array.fold2 (fun acc x y -> max acc (abs (x - y))) 0. r next
            r <- next
            iteration <- iteration + 1
            converged <- change < 1e-14
        let rT = Array.init 9 (fun i -> r.[(i % 3) * 3 + i / 3])
        let s = multiply3 rT a' |> Array.map (fun x -> sign * x)
        let rotation =
            Quaternion.ofRotationMatrix
                { identityMatrix with
                    Pos1x1 = r.[0]; Pos1x2 = r.[1]; Pos1x3 = r.[2]
                    Pos2x1 = r.[3]; Pos2x2 = r.[4]; Pos2x3 = r.[5]
                    Pos3x1 = r.[6]; Pos3x2 = r.[7]; Pos3x3 = r.[8] }
        { T = Vector(m.Pos1x4, m.Pos2x4, m.Pos3x4); R = rotation; S = s }

    static let compose (d: Decomposed) =
        let r = Quaternion.toMatrix d.R
        let r = [| r.Pos1x1; r.Pos1x2; r.Pos1x3; r.Pos2x1; r.Pos2x2; r.Pos2x3; r.Pos3x1; r.Pos3x2; r.Pos3x3 |]
        let a = multiply3 r d.S
        let i = invert3 a
        let t = d.T
        let forward =
            { identityMatrix with
                Pos1x1 = a.[0]; Pos1x2 = a.[1]; Pos1x3 = a.[2]; Pos1x4 = t.X
                Pos2x1 = a.[3]; Pos2x2 = a.[4]; Pos2x3 = a.[5]; Pos2x4 = t.Y
                Pos3x1 = a.[6]; Pos3x2 = a.[7]; Pos3x3 = a.[8]; Pos3x4 = t.Z }
        let inverse =
            { identityMatrix with
                Pos1x1 = i.[0]; Pos1x2 = i.[1]; Pos1x3 = i.[2]; Pos1x4 = -(i.[0] * t.X + i.[1] * t.Y + i.[2] * t.Z)
                Pos2x1 = i.[3]; Pos2x2 = i.[4]; Pos2x3 = i.[5]; Pos2x4 = -(i.[3] * t.X + i.[4] * t.Y + i.[5] * t.Z)
                Pos3x1 = i.[6]; Pos3x2 = i.[7]; Pos3x3 = i.[8]; Pos3x4 = -(i.[6] * t.X + i.[7] * t.Y + i.[8] * t.Z) }
        struct (forward, inverse)

    let times = keys |> Array.map fst
    let decomposed =
        let raw = keys |> Array.map (snd >> decompose)
        // Keep neighbouring rotations in the same hemisphere so interpolation takes the short way round.
        for i in 1 .. raw.Length - 1 do
            if Quaternion.dot raw.[i - 1].R raw.[i].R < 0. then
                let d = raw.[i]
                raw.[i] <- { d with R = Quaternion.negate d.R }
        raw
    let keyed = keys |> Array.map (fun (_, m) -> struct (m, (Transformation.ofAffine m |> getInvMatrix)))
    let isStatic = keys |> Array.forall (fun (_, m) -> m = snd keys.[0])

    member _.Times = times
    member _.Matrices = keys |> Array.map snd
    member _.IsStatic = isStatic
    member _.Start = times.[0]
    member _.End = times.[times.Length - 1]

    /// Forward and inverse matrices at `time`; times outside the keys clamp to the nearest key.
    member _.At(time: float) : struct (QuickMatrix * QuickMatrix) =
        if isStatic || time <= times.[0] || Double.IsNaN time then keyed.[0]
        elif time >= times.[times.Length - 1] then keyed.[keyed.Length - 1]
        else
            let mutable hi = 1
            while times.[hi] < time do hi <- hi + 1
            let lo = hi - 1
            if time = times.[hi] then keyed.[hi]
            else
                let u = (time - times.[lo]) / (times.[hi] - times.[lo])
                let a, b = decomposed.[lo], decomposed.[hi]
                compose
                    { T = a.T + u * (b.T - a.T)
                      R = Quaternion.slerp a.R b.R u
                      S = Array.init 9 (fun i -> a.S.[i] + u * (b.S.[i] - a.S.[i])) }
