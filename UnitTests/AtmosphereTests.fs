module AtmosphereTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Render

/// A small lit scene: a matte sphere on a large ground sphere under a sky and a sun. The lights are built
/// once and shared, because the classic integrator draws from the samplers they carry.
let private sceneParts =
    lazy (
        let matte = MatteMaterial(Colour(0.2, 0.2, 0.2), 0.3, Colour(0.7, 0.5, 0.3), 0.8)
        let sphere = SphereShape(Point(0., 1., 0.), 1., Textures.mkMatTexture matte) :> Shape
        let floor = SphereShape(Point(0., -100., 0.), 100., Textures.mkMatTexture matte) :> Shape
        let sky = Textures.mkTexture (fun _ v -> EmissiveMaterial(Colour(0.4 + 0.4 * v, 0.5 + 0.3 * v, 0.9), 1.) :> Material)
        let env = EnvironmentLight(1e6, sky, multiJittered 2 83) :> Light
        let sun = DirectionalLight(Colour(1., 0.8, 0.6), 1.2, Vector(-0.6, 0.4, 0.5)) :> Light
        sphere, floor, sun, env, regular 2)

let private render (integrator: IntegratorKind) (atmosphere: Atmosphere option) (explicitNone: bool) =
    let sphere, floor, sun, env, cameraSampler = sceneParts.Value
    let ambient = AmbientLight(Colour.Black, 0.)
    let scene =
        match atmosphere with
        | Some a -> Scene([ sphere; floor ], [ sun; env ], ambient, 2, a)
        | None when explicitNone -> Scene([ sphere; floor ], [ sun; env ], ambient, 2, ?atmosphere = None)
        | None -> Scene([ sphere; floor ], [ sun; env ], ambient, 2)
    let camera =
        PinholeCamera(Point(0., 1.5, 6.), Point(0., 1., 0.), Vector(0., 1., 0.), 2., 2., 2., 24, 24, cameraSampler)
    let options = { RenderOptions.Default with Integrator = integrator; Threads = Environment.ProcessorCount; Seed = 5 }
    Render(scene, camera, options).RenderLinear.Pixels

/// An absent atmosphere, or one with zero density, leaves every pixel bit-identical.
let private noAtmosphereIsIdentical () =
    for kind, name in [ Classic, "classic"; Path, "path" ] do
        let reference = render kind None false
        let explicitNone = render kind None true
        let empty = render kind (Some { Atmosphere.Default with Density = 0. }) false
        Assert.True((reference = explicitNone), sprintf "atmosphere-none-identical-%s" name)
        Assert.True((reference = empty), sprintf "atmosphere-zero-density-identical-%s" name)
        let foggy = render kind (Some { Atmosphere.Default with Density = 0.05; ScaleHeight = 5. }) false
        Assert.True((reference <> foggy), sprintf "atmosphere-fog-changes-image-%s" name)

/// Transmittance lies in [0, 1] and never increases with distance, for rays climbing, level and descending.
let private transmittanceMonotonic () =
    let atmosphere = { Atmosphere.Default with Density = 2e-3; ScaleHeight = 150.; Tint = Colour(0.8, 1., 1.3) }
    let mutable ok = true
    let mutable bounded = true
    for originY in [ -200.; 0.; 120.; 900. ] do
        for dy in [ -0.9; -0.2; -1e-9; 0.; 1e-9; 0.05; 0.5; 1. ] do
            let direction = Vector(sqrt (max 0. (1. - dy * dy)), dy, 0.).Normalise
            let origin = Point(0., originY, 0.)
            let mutable previous = Colour(1., 1., 1.)
            for distance in [ 0.; 1e-3; 1.; 10.; 100.; 500.; 1000.; 4000.; 1e4; 1e5; infinity ] do
                let t = Atmosphere.transmittance atmosphere origin direction distance
                for c, p in [ t.R, previous.R; t.G, previous.G; t.B, previous.B ] do
                    if not (c >= 0. && c <= 1.) then bounded <- false
                    if c > p + 1e-15 then ok <- false
                previous <- t
    Assert.True(bounded, "atmosphere-transmittance-in-unit-interval")
    Assert.True(ok, "atmosphere-transmittance-monotonic-in-distance")

/// The closed-form optical depth matches a numerical integral of the exponential density.
let private opticalDepthMatchesQuadrature () =
    let atmosphere = { Atmosphere.Default with Density = 1e-3; BaseHeight = 20.; ScaleHeight = 120. }
    let mutable worst = 0.
    for dy in [ -0.3; 0.; 0.2; 0.8 ] do
        let direction = Vector(sqrt (1. - dy * dy), dy, 0.)
        let origin = Point(0., 150., 0.)
        let distance = 400.
        let steps = 20000
        let ds = distance / float steps
        let mutable sum = 0.
        for i in 0 .. steps - 1 do
            let h = origin.Y + (float i + 0.5) * ds * dy
            sum <- sum + atmosphere.Density * exp (-(h - atmosphere.BaseHeight) / atmosphere.ScaleHeight) * ds
        let analytic = Atmosphere.opticalDepth atmosphere origin direction distance
        worst <- max worst (abs (analytic - sum) / sum)
    Assert.True(worst < 1e-6, sprintf "atmosphere-optical-depth-closed-form (worst relative error %g)" worst)

/// With a uniform sky and no sun term, a surface as bright as the sky is unchanged by any amount of fog, so
/// distant ranges fade into the sky behind them rather than to some other colour.
let private fogFadesToSky () =
    let sky = Colour(0.6, 0.7, 0.9)
    let atmosphere = { Atmosphere.Default with Density = 1e-2; Sky = Some (fun _ -> sky); SunWeight = 0. }
    let medium = AtmosphereMedium(atmosphere, [])
    let result = medium.Apply(Point(0., 0., 0.), Vector(1., 0., 0.), 300., sky)
    let close a b = abs (a - b) < 1e-9
    Assert.True(close result.R sky.R && close result.G sky.G && close result.B sky.B, "atmosphere-fades-to-sky")
    let far = medium.Apply(Point(0., 0., 0.), Vector(1., 0., 0.), 1e6, Colour.Black)
    Assert.True(close far.R sky.R && close far.B sky.B, "atmosphere-opaque-distance-is-sky")

let allTest () =
    transmittanceMonotonic ()
    opticalDepthMatchesQuadrature ()
    fogFadesToSky ()
    noAtmosphereIsIdentical ()
