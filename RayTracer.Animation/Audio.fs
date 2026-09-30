namespace Tracer.Animation

open System
open System.IO

/// A small offline audio engine for soundtracks: stereo float buffers, a few synthesis and filter primitives,
/// sample playback with pitch change, a reverb, and a WAV writer.
module Audio =
    let sampleRate = 48000

    /// Stereo audio, one float per sample per channel, nominally in [-1, 1].
    type Buffer = { Left: float[]; Right: float[] }

    let silence (seconds: float) =
        let n = max 0 (int (ceil (seconds * float sampleRate)))
        { Left = Array.zeroCreate n; Right = Array.zeroCreate n }

    let duration (b: Buffer) = float b.Left.Length / float sampleRate

    /// Deterministic white noise in [-1, 1].
    type Noise(seed: uint64) =
        let mutable state = if seed = 0UL then 0x9E3779B97F4A7C15UL else seed
        member _.Next() =
            state <- state ^^^ (state <<< 13)
            state <- state ^^^ (state >>> 7)
            state <- state ^^^ (state <<< 17)
            float (state >>> 11) / float (1UL <<< 52) - 1.

    /// RBJ-cookbook biquad; call Process per sample. Coefficients may be changed while running.
    type Biquad() =
        let mutable b0, b1, b2, a1, a2 = 1., 0., 0., 0., 0.
        let mutable x1, x2, y1, y2 = 0., 0., 0., 0.
        member _.Set(nb0, nb1, nb2, na0, na1, na2) =
            b0 <- nb0 / na0; b1 <- nb1 / na0; b2 <- nb2 / na0; a1 <- na1 / na0; a2 <- na2 / na0
        member this.LowPass(frequency: float, q: float) =
            let w = 2. * Math.PI * min 0.45 (frequency / float sampleRate)
            let alpha = sin w / (2. * q)
            let c = cos w
            this.Set((1. - c) / 2., 1. - c, (1. - c) / 2., 1. + alpha, -2. * c, 1. - alpha)
        member this.BandPass(frequency: float, q: float) =
            let w = 2. * Math.PI * min 0.45 (frequency / float sampleRate)
            let alpha = sin w / (2. * q)
            let c = cos w
            this.Set(alpha, 0., -alpha, 1. + alpha, -2. * c, 1. - alpha)
        member this.HighPass(frequency: float, q: float) =
            let w = 2. * Math.PI * min 0.45 (frequency / float sampleRate)
            let alpha = sin w / (2. * q)
            let c = cos w
            this.Set((1. + c) / 2., -(1. + c), (1. + c) / 2., 1. + alpha, -2. * c, 1. - alpha)
        member _.Process(x: float) =
            let y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
            x2 <- x1; x1 <- x; y2 <- y1; y1 <- y
            y

    /// Equal-power pan: -1 is hard left, +1 hard right.
    let panGains (pan: float) =
        let angle = (Math.Clamp(pan, -1., 1.) + 1.) * Math.PI / 4.
        cos angle, sin angle

    /// Adds `mono` into `target` starting at `time` seconds, with a gain and a pan that may vary per sample
    /// (both functions take the absolute time in seconds).
    let mixMono (target: Buffer) (time: float) (mono: float[]) (gain: float -> float) (pan: float -> float) =
        let start = int (round (time * float sampleRate))
        for i in 0 .. mono.Length - 1 do
            let j = start + i
            if j >= 0 && j < target.Left.Length then
                let t = float j / float sampleRate
                let g = gain t
                let l, r = panGains (pan t)
                target.Left.[j] <- target.Left.[j] + mono.[i] * g * l
                target.Right.[j] <- target.Right.[j] + mono.[i] * g * r

    let mixStereo (target: Buffer) (time: float) (source: Buffer) (gain: float) =
        let start = int (round (time * float sampleRate))
        for i in 0 .. source.Left.Length - 1 do
            let j = start + i
            if j >= 0 && j < target.Left.Length then
                target.Left.[j] <- target.Left.[j] + gain * source.Left.[i]
                target.Right.[j] <- target.Right.[j] + gain * source.Right.[i]

    /// Resamples by `ratio` (below 1 lowers the pitch and lengthens the sound), with linear interpolation.
    let repitch (ratio: float) (samples: float[]) =
        let n = int (float samples.Length / ratio)
        Array.init n (fun i ->
            let x = float i * ratio
            let k = int x
            let f = x - float k
            if k + 1 < samples.Length then samples.[k] * (1. - f) + samples.[k + 1] * f
            elif k < samples.Length then samples.[k] else 0.)

    let toMono (b: Buffer) = Array.map2 (fun l r -> 0.5 * (l + r)) b.Left b.Right

    /// Short-time RMS envelope (one value per `window` seconds) and the time of its loudest window.
    let loudest (samples: float[]) (window: float) =
        let w = max 1 (int (window * float sampleRate))
        let windows = samples.Length / w
        let rms = Array.init windows (fun k -> sqrt (Array.sub samples (k * w) w |> Array.averageBy (fun x -> x * x)))
        let peak = rms |> Array.indexed |> Array.maxBy snd |> fst
        (float peak + 0.5) * window

    /// Schroeder reverb (parallel combs into series allpasses); returns only the wet signal, in stereo.
    let reverb (roomSize: float) (damping: float) (input: float[]) (tail: float) =
        let n = input.Length + int (tail * float sampleRate)
        let render (spread: int) =
            let combs = [| 1557; 1617; 1491; 1422; 1277; 1356 |] |> Array.map (fun d -> int (float (d + spread) * roomSize))
            let out = Array.zeroCreate<float> n
            for delay in combs do
                let line = Array.zeroCreate<float> delay
                let mutable index, filtered = 0, 0.
                for i in 0 .. n - 1 do
                    let x = if i < input.Length then input.[i] else 0.
                    let y = line.[index]
                    filtered <- y * (1. - damping) + filtered * damping
                    line.[index] <- x + filtered * 0.84
                    index <- (index + 1) % delay
                    out.[i] <- out.[i] + y / float combs.Length
            for delay in [| 556 + spread; 441 + spread; 341 + spread |] do
                let line = Array.zeroCreate<float> delay
                let mutable index = 0
                for i in 0 .. n - 1 do
                    let buffered = line.[index]
                    let y = -out.[i] + buffered
                    line.[index] <- out.[i] + buffered * 0.5
                    index <- (index + 1) % delay
                    out.[i] <- y
            out
        { Left = render 0; Right = render 23 }

    /// Soft-knee limiting and normalisation of the peak to `ceiling` (e.g. 0.89 = -1 dBFS).
    let master (b: Buffer) (ceiling: float) =
        let soft x = Math.Tanh(x * 1.2) / Math.Tanh 1.2
        let peak = Seq.append b.Left b.Right |> Seq.map abs |> Seq.fold max 1e-9
        let pre = 1. / peak
        let l = b.Left |> Array.map (fun x -> soft (x * pre))
        let r = b.Right |> Array.map (fun x -> soft (x * pre))
        let peak2 = Seq.append l r |> Seq.map abs |> Seq.fold max 1e-9
        { Left = l |> Array.map (fun x -> x * ceiling / peak2); Right = r |> Array.map (fun x -> x * ceiling / peak2) }

    /// Writes 16-bit PCM stereo WAV.
    let writeWav (path: string) (b: Buffer) =
        use stream = File.Create path
        use w = new BinaryWriter(stream)
        let frames = b.Left.Length
        let dataBytes = frames * 4
        w.Write("RIFF"B); w.Write(36 + dataBytes); w.Write("WAVE"B)
        w.Write("fmt "B); w.Write(16); w.Write(int16 1); w.Write(int16 2); w.Write(sampleRate); w.Write(sampleRate * 4); w.Write(int16 4); w.Write(int16 16)
        w.Write("data"B); w.Write(dataBytes)
        let pcm (x: float) = int16 (Math.Round(Math.Clamp(x, -1., 1.) * 32767.))
        for i in 0 .. frames - 1 do
            w.Write(pcm b.Left.[i])
            w.Write(pcm b.Right.[i])

    /// Returns the buffer from `start` seconds on (for renders that begin mid-animation).
    let slice (b: Buffer) (start: float) (length: float) =
        let s = max 0 (int (start * float sampleRate))
        let n = max 0 (min (b.Left.Length - s) (int (length * float sampleRate)))
        { Left = Array.sub b.Left s n; Right = Array.sub b.Right s n }
