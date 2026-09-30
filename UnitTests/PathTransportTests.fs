module PathTransportTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Render

// Transport-level tests for the path tracer. The BSDF suite checks one surface
// in isolation; these check the integrator that connects surfaces together -
// next-event estimation, the MIS weights, throughput, and Russian roulette.

/// Renders a convex diffuse sphere lit only by a uniform environment and
/// returns the mean radiance over a patch that is solidly on the sphere.
let private furnaceSphere albedo depth samplesPerAxis envSampler =
    let colour = Colour(albedo, albedo, albedo)
    let material = MatteMaterial(Colour.Black, 0., colour, 1.)
    let sphere = SphereShape(Point(0., 0., 0.), 1., Textures.mkMatTexture material) :> Shape
    let envTexture = Textures.mkMatTexture (EmissiveMaterial(Colour.White, 1.))
    let env = EnvironmentLight(1e6, envTexture, envSampler) :> Light
    let scene = Scene([ sphere ], [ env ], AmbientLight(Colour.Black, 0.), depth)
    let camera =
        PinholeCamera(Point(0., 0., 4.), Point(0., 0., 0.), Vector(0., 1., 0.),
                      2., 2., 2., 32, 32, regular samplesPerAxis)
    let options =
        { RenderOptions.Default with
            Integrator = Path; Threads = Environment.ProcessorCount; Seed = 7 }
    let film = Render(scene, camera, options).RenderLinear
    let mutable sum = 0.
    let mutable count = 0
    for y in 12 .. 19 do
        for x in 12 .. 19 do
            let offset = ((film.Height - 1 - y) * film.Width + x) * 3
            sum <- sum + (film.Pixels.[offset] + film.Pixels.[offset + 1] + film.Pixels.[offset + 2]) / 3.
            count <- count + 1
    sum / float count

/// The furnace test, at the level of the whole transport loop.
///
/// A convex diffuse sphere of albedo rho, lit only by a uniform environment of
/// radiance 1 and with nothing to occlude it, must reflect exactly rho in every
/// direction, at every bounce depth. This fails if next-event estimation double
/// counts, if the MIS weights do not sum to one, or if a throughput factor is
/// dropped. It caught both real transport bugs in this integrator.
let private sceneFurnace () =
    for albedo in [ 0.2; 0.5; 1.0 ] do
        for depth in [ 1; 4 ] do
            let measured = furnaceSphere albedo depth 8 (multiJittered 2 83)
            let error = abs (measured - albedo) / albedo
            Assert.True(error < 0.02,
                        sprintf "scene-furnace-albedo%.1f-depth%d (measured %.4f, expected %.4f)"
                            albedo depth measured albedo)

/// The result must not depend on which sampler the lights happen to carry.
///
/// Light samplers hold a fixed table indexed by a key, and `regular` has just
/// one set, so a naive implementation reuses the same directions at every path
/// vertex. Under MIS that is a deterministic quadrature rather than a Monte
/// Carlo estimate, and it read 1-3% high before the integrator was changed to
/// draw its own decorrelated samples.
let private independentOfLightSampler () =
    let albedo = 0.5
    let samplers = [ "regular", regular 2; "multi-jittered", multiJittered 2 83; "random", random 16 83 ]
    for name, sampler in samplers do
        let measured = furnaceSphere albedo 3 8 sampler
        let error = abs (measured - albedo) / albedo
        Assert.True(error < 0.02,
                    sprintf "furnace-independent-of-%s-light-sampler (measured %.4f)" name measured)

/// Per-pixel RMS deviation from the analytic answer, over a patch on the
/// sphere. This must be measured PER PIXEL: averaging the patch first cancels
/// exactly the noise the test is trying to observe, leaving only bias.
let private furnaceRms albedo samplesPerAxis =
    let colour = Colour(albedo, albedo, albedo)
    let material = MatteMaterial(Colour.Black, 0., colour, 1.)
    let sphere = SphereShape(Point(0., 0., 0.), 1., Textures.mkMatTexture material) :> Shape
    let envTexture = Textures.mkMatTexture (EmissiveMaterial(Colour.White, 1.))
    let env = EnvironmentLight(1e6, envTexture, multiJittered 2 83) :> Light
    let scene = Scene([ sphere ], [ env ], AmbientLight(Colour.Black, 0.), 3)
    let camera =
        PinholeCamera(Point(0., 0., 4.), Point(0., 0., 0.), Vector(0., 1., 0.),
                      2., 2., 2., 32, 32, regular samplesPerAxis)
    let options =
        { RenderOptions.Default with
            Integrator = Path; Threads = Environment.ProcessorCount; Seed = 7 }
    let film = Render(scene, camera, options).RenderLinear
    let mutable squared = 0.
    let mutable count = 0
    for y in 12 .. 19 do
        for x in 12 .. 19 do
            let offset = ((film.Height - 1 - y) * film.Width + x) * 3
            let value = (film.Pixels.[offset] + film.Pixels.[offset + 1] + film.Pixels.[offset + 2]) / 3.
            squared <- squared + (value - albedo) * (value - albedo)
            count <- count + 1
    sqrt (squared / float count)

/// Monte Carlo error must fall as 1/sqrt(N). Going from 4 to 64 samples per
/// pixel is 16x the work, so RMS error should drop by roughly 4x. The bounds
/// are generous in both directions: too small a drop means the estimator is not
/// converging, while a much larger drop would mean the coarse render was not
/// actually noisy and the test is measuring nothing.
let private convergenceRate () =
    let albedo = 0.5
    let coarse = furnaceRms albedo 2
    let fine = furnaceRms albedo 8
    let ratio = coarse / max 1e-9 fine
    Assert.True(coarse > 1e-3,
                sprintf "convergence-test-has-noise-to-measure (coarse rms %.5f)" coarse)
    Assert.True(ratio > 2.0 && ratio < 8.0,
                sprintf "path-tracer-converges-as-inverse-sqrt-n (rms %.5f -> %.5f, ratio %.2f, expected ~4)"
                    coarse fine ratio)

/// Russian roulette changes cost, never the expected value. Starting it at
/// different depths must produce the same answer within noise.
let private rouletteIsUnbiased () =
    let albedo = 0.7
    let run rouletteDepth =
        let colour = Colour(albedo, albedo, albedo)
        let material = MatteMaterial(Colour.Black, 0., colour, 1.)
        let sphere = SphereShape(Point(0., 0., 0.), 1., Textures.mkMatTexture material) :> Shape
        let envTexture = Textures.mkMatTexture (EmissiveMaterial(Colour.White, 1.))
        let env = EnvironmentLight(1e6, envTexture, multiJittered 2 83) :> Light
        let scene = Scene([ sphere ], [ env ], AmbientLight(Colour.Black, 0.), 8)
        let camera =
            PinholeCamera(Point(0., 0., 4.), Point(0., 0., 0.), Vector(0., 1., 0.),
                          2., 2., 2., 32, 32, regular 8)
        let options =
            { RenderOptions.Default with
                Integrator = Path; Threads = Environment.ProcessorCount
                Seed = 11; RouletteDepth = rouletteDepth }
        let film = Render(scene, camera, options).RenderLinear
        let mutable sum = 0.
        let mutable count = 0
        for y in 12 .. 19 do
            for x in 12 .. 19 do
                let offset = ((film.Height - 1 - y) * film.Width + x) * 3
                sum <- sum + film.Pixels.[offset]
                count <- count + 1
        sum / float count
    let early = run 1
    let late = run 6
    Assert.True(abs (early - late) / albedo < 0.03,
                sprintf "russian-roulette-unbiased (depth1 %.4f vs depth6 %.4f)" early late)

let allTest () =
    sceneFurnace ()
    independentOfLightSampler ()
    convergenceRate ()
    rouletteIsUnbiased ()
