namespace Tracer.Animation

open System
open Tracer.Basics

/// Keyframe interpolation, as defined for glTF 2.0 animation samplers.
type Interpolation =
    | Step
    | Linear
    /// Cubic Hermite spline; every key carries an in-tangent and an out-tangent.
    | CubicSpline

/// Keys for one animated property. Tangents are only used by CubicSpline (empty otherwise) and, as in glTF,
/// are expressed per second: they are scaled by the key spacing during evaluation.
type Sampler<'T> =
    { Times: float[]
      Values: 'T[]
      InTangents: 'T[]
      OutTangents: 'T[]
      Interpolation: Interpolation
      /// Optional retiming of each segment; an authoring extension that glTF export bakes away.
      Ease: Easing.Ease option }

type Track =
    | Translation of Sampler<Vector>
    | Rotation of Sampler<Quaternion>
    | Scale of Sampler<Vector>

/// One track driving one node, addressed by the node's unique name.
type Channel = { Node: string; Track: Track }

type Clip =
    { Name: string
      Channels: Channel list }
    member this.Duration =
        this.Channels
        |> List.map (fun channel ->
            let times =
                match channel.Track with
                | Translation s | Scale s -> s.Times
                | Rotation s -> s.Times
            times.[times.Length - 1])
        |> List.fold max 0.

module Sampler =
    let private validate (times: float[]) valueCount interpolation (inTangents: 'T[]) (outTangents: 'T[]) =
        if isNull times || times.Length = 0 then invalidArg (nameof times) "A sampler needs at least one key."
        if valueCount <> times.Length then invalidArg (nameof times) "A sampler needs exactly one value per key."
        for i in 0 .. times.Length - 1 do
            if not (Double.IsFinite times.[i]) || (i > 0 && not (times.[i] > times.[i - 1])) then
                invalidArg (nameof times) "Key times must be finite and strictly increasing."
        match interpolation with
        | CubicSpline when inTangents.Length <> times.Length || outTangents.Length <> times.Length ->
            invalidArg (nameof inTangents) "A cubic-spline sampler needs an in- and out-tangent per key."
        | _ -> ()

    let create interpolation (keys: (float * 'T) list) : Sampler<'T> =
        if interpolation = CubicSpline then invalidArg (nameof interpolation) "Use Sampler.cubic for cubic-spline keys."
        let times = keys |> List.map fst |> Array.ofList
        let values = keys |> List.map snd |> Array.ofList
        validate times values.Length interpolation [||] [||]
        { Times = times; Values = values; InTangents = [||]; OutTangents = [||]; Interpolation = interpolation; Ease = None }

    let linear keys = create Linear keys
    let step keys = create Step keys

    /// Keys as (time, inTangent, value, outTangent), matching glTF's cubic-spline element order.
    let cubic (keys: (float * 'T * 'T * 'T) list) : Sampler<'T> =
        let times = keys |> List.map (fun (t, _, _, _) -> t) |> Array.ofList
        let ins = keys |> List.map (fun (_, a, _, _) -> a) |> Array.ofList
        let values = keys |> List.map (fun (_, _, v, _) -> v) |> Array.ofList
        let outs = keys |> List.map (fun (_, _, _, b) -> b) |> Array.ofList
        validate times values.Length CubicSpline ins outs
        { Times = times; Values = values; InTangents = ins; OutTangents = outs; Interpolation = CubicSpline; Ease = None }

    let withEase ease (sampler: Sampler<'T>) = { sampler with Ease = Some ease }

    /// Segment index k and local parameter s in [0, 1) such that t lies in [times[k], times[k+1]).
    /// The flag is false when t is clamped to an end key, whose index is then returned.
    let internal locate (times: float[]) (t: float) : struct (int * float * bool) =
        let last = times.Length - 1
        if last = 0 || t <= times.[0] || Double.IsNaN t then struct (0, 0., false)
        elif t >= times.[last] then struct (last, 0., false)
        else
            let mutable low, high = 0, last
            while high - low > 1 do
                let middle = (low + high) / 2
                if times.[middle] <= t then low <- middle else high <- middle
            struct (low, (t - times.[low]) / (times.[low + 1] - times.[low]), true)

    let internal hermite (s: float) =
        let s2, s3 = s * s, s * s * s
        struct (2. * s3 - 3. * s2 + 1., s3 - 2. * s2 + s, -2. * s3 + 3. * s2, s3 - s2)

    let evaluateVector (sampler: Sampler<Vector>) (t: float) =
        match locate sampler.Times t with
        | struct (k, _, false) -> sampler.Values.[k]
        | struct (k, s, true) ->
            let s = match sampler.Ease with Some ease -> ease s | None -> s
            let a, b = sampler.Values.[k], sampler.Values.[k + 1]
            match sampler.Interpolation with
            | Step -> a
            | Linear -> a + s * (b - a)
            | CubicSpline ->
                let td = sampler.Times.[k + 1] - sampler.Times.[k]
                let struct (h00, h10, h01, h11) = hermite s
                h00 * a + (h10 * td) * sampler.OutTangents.[k] + h01 * b + (h11 * td) * sampler.InTangents.[k + 1]

    let evaluateRotation (sampler: Sampler<Quaternion>) (t: float) =
        match locate sampler.Times t with
        | struct (k, _, false) -> Quaternion.normalise sampler.Values.[k]
        | struct (k, s, true) ->
            let s = match sampler.Ease with Some ease -> ease s | None -> s
            let a, b = sampler.Values.[k], sampler.Values.[k + 1]
            match sampler.Interpolation with
            | Step -> Quaternion.normalise a
            | Linear -> Quaternion.slerp a b s
            | CubicSpline ->
                // glTF: evaluate the spline component-wise, then normalise.
                let td = sampler.Times.[k + 1] - sampler.Times.[k]
                let struct (h00, h10, h01, h11) = hermite s
                let m, n = sampler.OutTangents.[k], sampler.InTangents.[k + 1]
                let c (av: float) (mv: float) (bv: float) (nv: float) = h00 * av + h10 * td * mv + h01 * bv + h11 * td * nv
                Quaternion.normalise
                    { X = c a.X m.X b.X n.X; Y = c a.Y m.Y b.Y n.Y
                      Z = c a.Z m.Z b.Z n.Z; W = c a.W m.W b.W n.W }

/// Smooth keys in the style of Blender's auto-clamped handles: Catmull-Rom tangents, flattened wherever a
/// component turns around (so there is no overshoot) and at the first and last key (so motion eases in and
/// out). Stored as glTF cubic-spline keys, so they export exactly.
module Smooth =
    let private tangents (times: float[]) (values: float[][]) =
        let n = times.Length
        Array.init n (fun k ->
            if k = 0 || k = n - 1 then Array.zeroCreate values.[k].Length
            else
                Array.init values.[k].Length (fun c ->
                    let before = values.[k].[c] - values.[k - 1].[c]
                    let after = values.[k + 1].[c] - values.[k].[c]
                    if before * after <= 0. then 0.
                    else (values.[k + 1].[c] - values.[k - 1].[c]) / (times.[k + 1] - times.[k - 1])))

    let vector (keys: (float * Vector) list) : Sampler<Vector> =
        let times = keys |> List.map fst |> Array.ofList
        let values = keys |> List.map (fun (_, v) -> [| v.X; v.Y; v.Z |]) |> Array.ofList
        let m = tangents times values |> Array.map (fun t -> Vector(t.[0], t.[1], t.[2]))
        Sampler.cubic [ for k in 0 .. times.Length - 1 -> times.[k], m.[k], Vector(values.[k].[0], values.[k].[1], values.[k].[2]), m.[k] ]

    let rotation (keys: (float * Quaternion) list) : Sampler<Quaternion> =
        let times = keys |> List.map fst |> Array.ofList
        // Keep consecutive rotations in one hemisphere so the spline takes the short way round.
        let quats = keys |> List.map snd |> Array.ofList
        for k in 1 .. quats.Length - 1 do
            if Quaternion.dot quats.[k - 1] quats.[k] < 0. then quats.[k] <- Quaternion.negate quats.[k]
        let values = quats |> Array.map (fun q -> [| q.X; q.Y; q.Z; q.W |])
        let m = tangents times values |> Array.map (fun t -> { X = t.[0]; Y = t.[1]; Z = t.[2]; W = t.[3] })
        Sampler.cubic [ for k in 0 .. times.Length - 1 -> times.[k], m.[k], quats.[k], m.[k] ]

/// Helpers for authoring clips in code.
module Clip =
    let create name channels = { Name = name; Channels = channels }

    let translate node sampler = { Node = node; Track = Translation sampler }

    /// Delays every key of the clip by `offset` seconds.
    let shift (offset: float) (clip: Clip) =
        let move (s: Sampler<'T>) = { s with Times = s.Times |> Array.map ((+) offset) }
        { clip with
            Channels =
                clip.Channels |> List.map (fun channel ->
                    { channel with
                        Track =
                            match channel.Track with
                            | Translation s -> Translation (move s)
                            | Rotation s -> Rotation (move s)
                            | Scale s -> Scale (move s) }) }
    let rotate node sampler = { Node = node; Track = Rotation sampler }
    let scale node sampler = { Node = node; Track = Scale sampler }

    /// Applies every channel of the clip at time t to the rest poses in `pose` (node name -> TRS).
    let apply (clip: Clip) (t: float) (pose: Collections.Generic.Dictionary<string, Trs>) =
        for channel in clip.Channels do
            match pose.TryGetValue channel.Node with
            | true, trs ->
                pose.[channel.Node] <-
                    match channel.Track with
                    | Translation s -> { trs with Translation = Sampler.evaluateVector s t }
                    | Rotation s -> { trs with Rotation = Sampler.evaluateRotation s t }
                    | Scale s -> { trs with Scale = Sampler.evaluateVector s t }
            | _ -> invalidArg (nameof clip) $"Clip {clip.Name} animates unknown node {channel.Node}."
