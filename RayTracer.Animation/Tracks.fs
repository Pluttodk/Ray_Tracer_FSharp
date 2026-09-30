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

/// Helpers for authoring clips in code.
module Clip =
    let create name channels = { Name = name; Channels = channels }

    let translate node sampler = { Node = node; Track = Translation sampler }
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
