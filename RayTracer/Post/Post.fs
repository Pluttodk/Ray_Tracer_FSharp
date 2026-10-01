/// Post-processing on the linear float framebuffer (RGB triples, row-major). Everything here runs in linear
/// light, after denoising and before the display transfer: bloom, then grade, then vignette.
module Tracer.Basics.Post

open System
open System.Threading.Tasks

type PostSettings =
    { /// Bloom strength: how much of the blurred bright regions is added back (0 disables).
      Bloom: float
      /// Linear luminance above which pixels feed the bloom.
      BloomThreshold: float
      /// Corner darkening in [0, 1] (0 disables).
      Vignette: float
      /// Linear exposure multiplier.
      Exposure: float
      /// Warm (> 0) or cool (< 0) shift, roughly in [-1, 1].
      WhiteBalance: float
      /// 1 leaves colours alone, 0 is greyscale.
      Saturation: float }
    static member Identity =
        { Bloom = 0.; BloomThreshold = 1.; Vignette = 0.; Exposure = 1.; WhiteBalance = 0.; Saturation = 1. }
    member this.IsIdentity = this = PostSettings.Identity

/// Standard deviations of the bloom's Gaussians as fractions of the image height, and their shares of the glow
/// (the shares sum to 1).
let bloomScales = [| 0.004, 0.3; 0.012, 0.35; 0.035, 0.35 |]

/// A normalised 1-D Gaussian kernel of the given standard deviation (in pixels), truncated at 3 sigma.
let gaussianKernel (sigma: float) =
    let radius = max 1 (int (ceil (3. * sigma)))
    let raw = Array.init (2 * radius + 1) (fun i -> let x = float (i - radius) in exp (-(x * x) / (2. * sigma * sigma)))
    let total = Array.sum raw
    raw |> Array.map (fun v -> v / total)

/// Separable Gaussian blur of an RGB buffer; pixels outside the image count as black.
let blur (width: int) (height: int) (sigma: float) (pixels: float[]) =
    let kernel = gaussianKernel sigma
    let radius = kernel.Length / 2
    let horizontal = Array.zeroCreate<float> pixels.Length
    Parallel.For(0, height, fun y ->
        for x in 0 .. width - 1 do
            for c in 0 .. 2 do
                let mutable sum = 0.
                for k in max (-radius) (-x) .. min radius (width - 1 - x) do
                    sum <- sum + kernel.[k + radius] * pixels.[(y * width + x + k) * 3 + c]
                horizontal.[(y * width + x) * 3 + c] <- sum) |> ignore
    let result = Array.zeroCreate<float> pixels.Length
    Parallel.For(0, height, fun y ->
        for x in 0 .. width - 1 do
            for c in 0 .. 2 do
                let mutable sum = 0.
                for k in max (-radius) (-y) .. min radius (height - 1 - y) do
                    sum <- sum + kernel.[k + radius] * horizontal.[((y + k) * width + x) * 3 + c]
                result.[(y * width + x) * 3 + c] <- sum) |> ignore
    result

let private luminance r g b = 0.2126 * r + 0.7152 * g + 0.0722 * b

/// The glow: the part of each pixel above the threshold, blurred at several scales. With a zero threshold the
/// total energy equals the image's (away from the borders), so `strength` is the fraction of light spread.
let bloomLayer (width: int) (height: int) (threshold: float) (pixels: float[]) =
    let bright = Array.zeroCreate<float> pixels.Length
    for i in 0 .. width * height - 1 do
        let r, g, b = pixels.[i * 3], pixels.[i * 3 + 1], pixels.[i * 3 + 2]
        let l = luminance r g b
        if l > threshold then
            let keep = (l - threshold) / l
            bright.[i * 3] <- r * keep
            bright.[i * 3 + 1] <- g * keep
            bright.[i * 3 + 2] <- b * keep
    let glow = Array.zeroCreate<float> pixels.Length
    for sigma, weight in bloomScales do
        let blurred = blur width height (max 0.75 (sigma * float height)) bright
        for i in 0 .. glow.Length - 1 do glow.[i] <- glow.[i] + weight * blurred.[i]
    glow

/// Applies the settings to a linear RGB buffer, returning a new one. Identity settings return an equal copy.
let apply (settings: PostSettings) (width: int) (height: int) (pixels: float[]) =
    if not (Double.IsFinite settings.Exposure) || settings.Exposure <= 0. then invalidArg "settings" "Exposure must be finite and positive."
    if settings.Bloom < 0. || settings.Vignette < 0. || settings.Vignette > 1. || settings.Saturation < 0. then
        invalidArg "settings" "Bloom and saturation must be non-negative, and vignette must lie in [0, 1]."
    let glow = if settings.Bloom > 0. then Some (bloomLayer width height settings.BloomThreshold pixels) else None
    // Warm shifts red up and blue down by equal log amounts, so overall brightness is unchanged.
    let wb = settings.WhiteBalance
    let gainR, gainB = exp (0.25 * wb), exp (-0.25 * wb)
    let result = Array.zeroCreate<float> pixels.Length
    let cx, cy = float (width - 1) / 2., float (height - 1) / 2.
    let corner2 = max 1e-9 (cx * cx + cy * cy)
    Parallel.For(0, height, fun y ->
        for x in 0 .. width - 1 do
            let i = (y * width + x) * 3
            let mutable r, g, b = pixels.[i], pixels.[i + 1], pixels.[i + 2]
            match glow with
            | Some layer ->
                r <- r + settings.Bloom * layer.[i]
                g <- g + settings.Bloom * layer.[i + 1]
                b <- b + settings.Bloom * layer.[i + 2]
            | None -> ()
            let e = settings.Exposure
            r <- r * e * gainR; g <- g * e; b <- b * e * gainB
            let l = luminance r g b
            let s = settings.Saturation
            r <- max 0. (l + s * (r - l)); g <- max 0. (l + s * (g - l)); b <- max 0. (l + s * (b - l))
            let dx, dy = float x - cx, float y - cy
            let falloff = 1. - settings.Vignette * ((dx * dx + dy * dy) / corner2)
            result.[i] <- r * falloff; result.[i + 1] <- g * falloff; result.[i + 2] <- b * falloff) |> ignore
    result
