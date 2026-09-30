namespace Tracer.Animation

open System

/// Timing curves mapping normalised time in [0, 1] to progress. Every curve maps 0 to 0 and 1 to 1.
module Easing =
    type Ease = float -> float

    let linear: Ease = id
    let smoothstep: Ease = fun t -> t * t * (3. - 2. * t)

    let quadIn: Ease = fun t -> t * t
    let quadOut: Ease = fun t -> t * (2. - t)
    let quadInOut: Ease = fun t -> if t < 0.5 then 2. * t * t else 1. - 2. * (1. - t) * (1. - t)

    let cubicIn: Ease = fun t -> t * t * t
    let cubicOut: Ease = fun t -> 1. - pown (1. - t) 3
    let cubicInOut: Ease = fun t -> if t < 0.5 then 4. * t * t * t else 1. - 4. * pown (1. - t) 3

    /// Overshoot constant of the classic "back" curves (about 10% overshoot).
    let private back = 1.70158
    /// Pulls back before moving: animation's anticipation.
    let backIn: Ease = fun t -> t * t * ((back + 1.) * t - back)
    /// Overshoots the target and settles: follow-through.
    let backOut: Ease = fun t -> let u = t - 1. in 1. + u * u * ((back + 1.) * u + back)
    let backInOut: Ease =
        fun t ->
            let c = back * 1.525
            if t < 0.5 then (pown (2. * t) 2 * ((c + 1.) * 2. * t - c)) / 2.
            else (pown (2. * t - 2.) 2 * ((c + 1.) * (t * 2. - 2.) + c) + 2.) / 2.

    /// CSS-style cubic Bezier through (0,0), (x1,y1), (x2,y2), (1,1); x1 and x2 must lie in [0, 1].
    let cubicBezier x1 y1 x2 y2 : Ease =
        if not (x1 >= 0. && x1 <= 1. && x2 >= 0. && x2 <= 1.) then
            invalidArg (nameof x1) "Bezier control x coordinates must lie in [0, 1]."
        let curve a b s = 3. * a * s * (1. - s) * (1. - s) + 3. * b * s * s * (1. - s) + s * s * s
        let slope a b s = 3. * a * (1. - s) * (1. - s) + 6. * (b - a) * s * (1. - s) + 3. * (1. - b) * s * s
        fun t ->
            if t <= 0. then 0.
            elif t >= 1. then 1.
            else
                // Newton iteration on x(s) = t, with bisection as the fallback when the slope vanishes.
                let mutable s = t
                let mutable iteration = 0
                let mutable finished = false
                while not finished && iteration < 8 do
                    let error = curve x1 x2 s - t
                    let d = slope x1 x2 s
                    if abs error < 1e-12 then finished <- true
                    elif abs d < 1e-9 then iteration <- 8
                    else s <- Math.Clamp(s - error / d, 0., 1.)
                    iteration <- iteration + 1
                if not finished then
                    let mutable low, high = 0., 1.
                    for _ in 1 .. 60 do
                        s <- 0.5 * (low + high)
                        if curve x1 x2 s < t then low <- s else high <- s
                curve y1 y2 s

    let ofName (name: string) : Ease =
        match name.ToLowerInvariant() with
        | "linear" -> linear
        | "smoothstep" -> smoothstep
        | "quadin" -> quadIn
        | "quadout" -> quadOut
        | "quadinout" | "ease" -> quadInOut
        | "cubicin" -> cubicIn
        | "cubicout" -> cubicOut
        | "cubicinout" -> cubicInOut
        | "backin" -> backIn
        | "backout" -> backOut
        | "backinout" -> backInOut
        | other -> invalidArg (nameof name) $"Unknown easing curve {other}."
