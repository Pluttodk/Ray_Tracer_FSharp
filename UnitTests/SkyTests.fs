module SkyTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Render

// The analytic sky (Sky.fs) and the environment light's luminance-table importance sampling.

let private sunAt elevationDegrees azimuthDegrees =
    let e, a = elevationDegrees * Math.PI / 180., azimuthDegrees * Math.PI / 180.
    Vector(cos e * sin a, sin e, cos e * cos a)

let private skyWith elevation turbidity =
    Sky.create { SunDirection = sunAt elevation -130.; Turbidity = turbidity; SunIrradiance = 3.; SunToSky = None }

let private finitePositive (c: Colour) =
    Double.IsFinite c.R && Double.IsFinite c.G && Double.IsFinite c.B
    && c.R >= 0. && c.G >= 0. && c.B >= 0. && Sky.luminance c > 0.

/// Sky luminance is finite and positive in every direction above the horizon, for low and high
/// suns and the whole turbidity range, and the region below the horizon is finite and non-negative.
let private skyRadianceIsFinitePositive () =
    for elevation in [ 1.; 6.; 30.; 85. ] do
        for turbidity in [ 1.7; 2.2; 4.; 10. ] do
            let sky = skyWith elevation turbidity
            let mutable ok = true
            for j in 0 .. 30 do
                for i in 0 .. 35 do
                    let polar = float j / 30. * (Math.PI / 2.)
                    let azimuth = float i / 36. * 2. * Math.PI
                    let d = Vector(sin polar * sin azimuth, cos polar, sin polar * cos azimuth)
                    if not (finitePositive (Sky.radiance sky d)) then ok <- false
                    if not (finitePositive (Sky.visible sky d)) then ok <- false
                    let below = Sky.radiance sky (Vector(d.X, -d.Y - 0.01, d.Z))
                    if not (Double.IsFinite below.R && below.R >= 0. && below.G >= 0. && below.B >= 0.) then ok <- false
            Assert.True(ok, sprintf "sky-radiance-finite-positive-el%.0f-T%.1f" elevation turbidity)
            let sun = Sky.sunRadiance sky
            Assert.True(finitePositive sun, sprintf "sky-sun-finite-positive-el%.0f-T%.1f" elevation turbidity)

/// A low sun is redder than a high one (longer path through the atmosphere).
let private lowSunIsRedder () =
    let ratio (sky: Sky) = let s = Sky.sunRadiance sky in s.B / s.R
    Assert.True(ratio (skyWith 6. 2.2) < 0.5 * ratio (skyWith 60. 2.2), "sky-low-sun-redder")

/// The disc is only in what the camera sees; lighting radiance never contains it, and the
/// DirectionalLight carries exactly the disc's integrated irradiance.
let private sunCountedOnce () =
    let sky = skyWith 6. 2.2
    let s = Sky.sunDirection sky
    let lit = Sky.radiance sky s
    let seen = Sky.visible sky s
    Assert.True(Sky.luminance seen > 100. * Sky.luminance lit, "sky-disc-visible-to-camera")
    Assert.True(Sky.luminance (Sky.discRadiance sky (sunAt 20. 50.)) = 0., "sky-disc-black-away-from-sun")
    // Integrate the limb-darkened disc over its solid angle; it must equal SunRadiance * omega.
    let u, v, w = SurfaceSampling.frame s
    let n = 400
    let mutable sum = Colour.Black
    let r = Sky.sunAngularRadius
    for j in 0 .. n - 1 do
        for i in 0 .. n - 1 do
            let x = (2. * (float i + 0.5) / float n - 1.) * r
            let y = (2. * (float j + 0.5) / float n - 1.) * r
            let d = (x * u + y * v + w).Normalise
            sum <- sum + Sky.discRadiance sky d * ((2. * r / float n) ** 2.)
    let expected = Sky.sunIrradiance sky
    let light = Sky.sunLight sky
    Assert.True(abs (Sky.luminance sum / Sky.luminance expected - 1.) < 0.01,
                sprintf "sky-disc-integrates-to-sun-irradiance (%.4f vs %.4f)" (Sky.luminance sum) (Sky.luminance expected))
    Assert.True(abs (Sky.luminance (light.GetColour(HitPoint(Point.Zero))) / Sky.luminance expected - 1.) < 1e-9,
                "sky-sun-light-carries-disc-irradiance")

/// SunToSky sets the rendered ratio of sun irradiance to horizontal sky irradiance.
let private sunToSkyRatioHonoured () =
    let sky = Sky.create { SunDirection = sunAt 6. -130.; Turbidity = 2.2; SunIrradiance = 3.; SunToSky = Some 6. }
    Assert.True(abs (Sky.sunToSkyRatio sky - 6.) < 1e-6, sprintf "sky-sun-to-sky-ratio (%.4f)" (Sky.sunToSkyRatio sky))
    Assert.True(abs (Sky.luminance (Sky.sunIrradiance sky) - 3.) < 1e-9, "sky-sun-irradiance-scale")

let private skyDistribution () =
    let sky = skyWith 6. 2.2
    EnvironmentDistribution.OfLuminance(128, 64, fun d -> Sky.luminance (Sky.radiance sky d))

/// The tabulated density integrates to one over the sphere (midpoint rule on a fine grid).
let private distributionPdfIntegratesToOne () =
    let table = skyDistribution ()
    let rows, columns = 400, 800
    let mutable sum = 0.
    for j in 0 .. rows - 1 do
        let c0, c1 = 1. - 2. * float j / float rows, 1. - 2. * float (j + 1) / float rows
        let cosine = 0.5 * (c0 + c1)
        let sine = sqrt (1. - cosine * cosine)
        for i in 0 .. columns - 1 do
            let phi = 2. * Math.PI * (float i + 0.5) / float columns
            sum <- sum + table.Pdf(Vector(sine * sin phi, cosine, sine * cos phi)) * (c0 - c1) * (2. * Math.PI / float columns)
    Assert.True(abs (sum - 1.) < 0.01, sprintf "env-cdf-pdf-integrates-to-one (%.5f)" sum)

/// Samples report the same density Pdf gives, and E[f/pdf] reproduces the integral of f.
let private distributionSamplingIsConsistent () =
    let table = skyDistribution ()
    let sky = skyWith 6. 2.2
    let f (d: Vector) = Sky.luminance (Sky.radiance sky d)
    let mutable consistent = true
    let mutable estimate = 0.
    let count = 200000
    for index in 0 .. count - 1 do
        let u1, u2 = sample2D (mixKey (uint64 index)) 0
        let struct (d, pdf) = table.Sample(u1, u2)
        if abs (d.Magnitude - 1.) > 1e-9 || abs (pdf / table.Pdf d - 1.) > 1e-6 then consistent <- false
        estimate <- estimate + f d / pdf
    estimate <- estimate / float count
    // Reference: midpoint rule over the sphere.
    let rows, columns = 300, 600
    let mutable reference = 0.
    for j in 0 .. rows - 1 do
        let c0, c1 = 1. - 2. * float j / float rows, 1. - 2. * float (j + 1) / float rows
        let cosine = 0.5 * (c0 + c1)
        let sine = sqrt (1. - cosine * cosine)
        for i in 0 .. columns - 1 do
            let phi = 2. * Math.PI * (float i + 0.5) / float columns
            reference <- reference + f (Vector(sine * sin phi, cosine, sine * cos phi)) * (c0 - c1) * (2. * Math.PI / float columns)
    Assert.True(consistent, "env-cdf-sample-pdf-matches")
    Assert.True(abs (estimate / reference - 1.) < 0.01,
                sprintf "env-cdf-estimator-unbiased (%.5f vs %.5f)" estimate reference)

/// The light's own mixture density (table plus cosine) integrates to one over the sphere.
let private lightPdfIntegratesToOne () =
    let sky = skyWith 6. 2.2
    let light = Sky.light sky 2 128
    let normal = Vector(0.3, 0.8, -0.2).Normalise
    let rows, columns = 300, 600
    let mutable sum = 0.
    for j in 0 .. rows - 1 do
        let c0, c1 = 1. - 2. * float j / float rows, 1. - 2. * float (j + 1) / float rows
        let cosine = 0.5 * (c0 + c1)
        let sine = sqrt (1. - cosine * cosine)
        for i in 0 .. columns - 1 do
            let phi = 2. * Math.PI * (float i + 0.5) / float columns
            sum <- sum + light.Pdf(normal, Vector(sine * sin phi, cosine, sine * cos phi)) * (c0 - c1) * (2. * Math.PI / float columns)
    Assert.True(abs (sum - 1.) < 0.01, sprintf "env-light-mixture-pdf-integrates-to-one (%.5f)" sum)

/// The furnace with an importance-sampled environment: a diffuse sphere under a uniform
/// environment of radiance 1 reflects its albedo, in both integrators.
let private importanceFurnace () =
    for integrator in [ Path; Classic ] do
        let albedo = 0.5
        let material = MatteMaterial(Colour.Black, 0., Colour(albedo, albedo, albedo), 1.)
        let sphere = SphereShape(Point(0., 0., 0.), 1., Textures.mkMatTexture material) :> Shape
        let envTexture = Textures.mkMatTexture (EmissiveMaterial(Colour.White, 1.))
        let env = EnvironmentLight(1e6, envTexture, multiJittered 4 83, None, None, 32) :> Light
        let scene = Scene([ sphere ], [ env ], AmbientLight(Colour.Black, 0.), 1)
        let camera =
            PinholeCamera(Point(0., 0., 4.), Point(0., 0., 0.), Vector(0., 1., 0.), 2., 2., 2., 32, 32, regular 8)
        let options =
            { RenderOptions.Default with Integrator = integrator; Threads = Environment.ProcessorCount; Seed = 7 }
        let film = Render(scene, camera, options).RenderLinear
        let mutable sum = 0.
        let mutable count = 0
        for y in 12 .. 19 do
            for x in 12 .. 19 do
                let offset = ((film.Height - 1 - y) * film.Width + x) * 3
                sum <- sum + (film.Pixels.[offset] + film.Pixels.[offset + 1] + film.Pixels.[offset + 2]) / 3.
                count <- count + 1
        let measured = sum / float count
        // Depth 1 in the classic integrator is direct light only, which is all of it here.
        Assert.True(abs (measured - albedo) / albedo < 0.02,
                    sprintf "env-importance-furnace-%A (measured %.4f)" integrator measured)

let allTest () =
    skyRadianceIsFinitePositive ()
    lowSunIsRedder ()
    sunCountedOnce ()
    sunToSkyRatioHonoured ()
    distributionPdfIntegratesToOne ()
    distributionSamplingIsConsistent ()
    lightPdfIntegratesToOne ()
    importanceFurnace ()
