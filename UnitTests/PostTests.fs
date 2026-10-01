module PostTests

open Assert
open Tracer.Basics.Post

let private sum (a: float[]) = Array.sum a

let kernelTests () =
    for sigma in [ 0.75; 2.; 9.5 ] do
        Assert.True(abs (sum (gaussianKernel sigma) - 1.) < 1e-12, $"post-kernel-normalised-{sigma}")

/// With a zero threshold the glow carries exactly the image's energy (away from the borders), at any scale.
let bloomEnergyTests () =
    let width, height = 160, 90
    let image = Array.zeroCreate<float> (width * height * 3)
    for (x, y, v) in [ 80, 45, 10.; 70, 40, 3.; 90, 50, 2. ] do
        for c in 0 .. 2 do image.[(y * width + x) * 3 + c] <- v * float (c + 1)
    let glow = bloomLayer width height 0. image
    Assert.True(abs (sum glow - sum image) / sum image < 1e-6, "post-bloom-energy")
    let bloomed = apply { PostSettings.Identity with Bloom = 0.5; BloomThreshold = 0. } width height image
    Assert.True(abs (sum bloomed - 1.5 * sum image) / sum image < 1e-6, "post-bloom-added")
    // A threshold above every pixel gives no glow.
    Assert.True(sum (bloomLayer width height 100. image) = 0., "post-bloom-threshold")

let gradeTests () =
    let image = Array.init (8 * 6 * 3) (fun i -> 0.05 * float i)
    Assert.True(apply PostSettings.Identity 8 6 image = image, "post-identity")
    // Saturation 0 gives grey; exposure scales; warm raises red over blue.
    let grey = apply { PostSettings.Identity with Saturation = 0. } 8 6 image
    Assert.True(abs (grey.[0] - grey.[1]) < 1e-12 && abs (grey.[1] - grey.[2]) < 1e-12, "post-desaturate")
    let doubled = apply { PostSettings.Identity with Exposure = 2. } 8 6 image
    Assert.True(abs (doubled.[10] - 2. * image.[10]) < 1e-12, "post-exposure")
    let warm = apply { PostSettings.Identity with WhiteBalance = 1. } 8 6 image
    Assert.True(warm.[3] / image.[3] > 1. && warm.[5] / image.[5] < 1., "post-warm")
    let vignetted = apply { PostSettings.Identity with Vignette = 0.5 } 8 6 image
    Assert.True(vignetted.[2] < image.[2] * 0.55 && abs (vignetted.[(3 * 8 + 3) * 3 + 1] - image.[(3 * 8 + 3) * 3 + 1]) < 0.1 * image.[(3 * 8 + 3) * 3 + 1], "post-vignette")

let allTest () =
    kernelTests ()
    bloomEnergyTests ()
    gradeTests ()
