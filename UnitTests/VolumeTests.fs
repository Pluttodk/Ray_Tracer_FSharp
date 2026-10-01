module VolumeTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Render

/// A point light whose radiance falls with the inverse square of distance, to exercise equiangular sampling.
type private InverseSquareLamp(colour: Colour, intensity: float, position: Point) =
    inherit PointLight(colour, intensity, position)
    override _.GetColour hit =
        let d2 = (position - hit.Point).MagnitudeSquared
        colour * (intensity / d2)

let private white = Colour(1., 1., 1.)
let private unshadowed = fun (_: Ray) (_: float) -> white

/// A thin slab |y| <= 1, 20 wide in x and z, which camera rays cross along x at y = 0.
let private slab =
    { Volume.Default with
        Min = Point(-10., -1., -10.); Max = Point(10., 1., 10.)
        Scattering = Colour(0.05, 0.1, 0.2); Absorption = Colour(0.01, 0.01, 0.01)
        SunSamples = 4; LampSamples = 4; SunAnisotropy = 0.6; LampAnisotropy = 0.3 }

let private medium (volume: Volume) (lights: Light list) =
    AtmosphereMedium({ Atmosphere.Default with Density = 0.; Volume = Some volume }, lights)

/// Mean in-scatter and transmittance of the segment from (-20, 0, 0) along +x over many sample keys.
let private estimate (m: AtmosphereMedium) (visible: Ray -> float -> Colour) keys =
    let mutable r, g, b = 0., 0., 0.
    let mutable transmittance = white
    for k in 0 .. keys - 1 do
        let struct (s, t) =
            m.VolumeSegment(Point(-20., 0., 0.), Vector(1., 0., 0.), infinity, 0., sampleKey 7 k 0, 0, visible)
        r <- r + s.R; g <- g + s.G; b <- b + s.B
        transmittance <- t
    Colour(r / float keys, g / float keys, b / float keys), transmittance

let private relativeError (expected: Colour) (actual: Colour) =
    List.max [ for e, a in [ expected.R, actual.R; expected.G, actual.G; expected.B, actual.B ] -> abs (a - e) / e ]

/// Sunlight straight down onto a fully lit homogeneous slab: the single-scatter integral has the closed form
/// sigma_s phase E T_sun (1 - exp(-sigma_t L)) / sigma_t, with T_sun = exp(-sigma_t) for the 1-unit climb out.
let private slabMatchesAnalytic () =
    let sun = DirectionalLight(Colour(1., 0.9, 0.8), 3., Vector(0., 1., 0.)) :> Light
    let e = Colour(3., 2.7, 2.4)
    let analytic (lit: float) =
        let phase = Atmosphere.henyeyGreenstein slab.SunAnisotropy 0.
        let channel s a irradiance =
            let t = s + a
            s * phase * irradiance * exp (-t) * (1. - exp (-t * lit)) / t
        Colour(channel 0.05 0.01 e.R, channel 0.1 0.01 e.G, channel 0.2 0.01 e.B)
    let scattered, transmittance = estimate (medium slab [ sun ]) unshadowed 4000
    let error = relativeError (analytic 20.) scattered
    Assert.True(error < 0.01, sprintf "volume-slab-matches-analytic-single-scatter (relative error %g)" error)
    let expectedT = Colour(exp (-0.06 * 20.), exp (-0.11 * 20.), exp (-0.21 * 20.))
    Assert.True(relativeError expectedT transmittance < 1e-9, "volume-slab-transmittance-closed-form")
    // A shadowed half-space (x > 0) contributes nothing: only the lit half of the slab scatters.
    let halfShadowed = fun (ray: Ray) (_: float) -> if ray.GetOrigin.X > 0. then Colour(0., 0., 0.) else white
    let halfLit, _ = estimate (medium slab [ sun ]) halfShadowed 4000
    let error = relativeError (analytic 10.) halfLit
    Assert.True(error < 0.01, sprintf "volume-shadowed-half-space-contributes-nothing (relative error %g)" error)
    let allShadowed, _ = estimate (medium slab [ sun ]) (fun _ _ -> Colour(0., 0., 0.)) 64
    Assert.True(allShadowed.IsBlack, "volume-fully-shadowed-is-black")

/// Lamps inside the slab against a fine quadrature of the single-scatter integral, for an inverse-square lamp
/// (equiangular mixture), a constant one (transmittance sampling) and a sphere lamp, with the slab thinning
/// with height.
let private lampsMatchQuadrature () =
    let position = Point(1.5, 0.4, 0.3)
    let volume = { slab with ScaleHeight = 0.8; BaseHeight = -0.2 }
    for lamp, name in [ InverseSquareLamp(Colour(1., 0.8, 0.6), 2., position) :> Light, "inverse-square"
                        PointLight(Colour(1., 0.8, 0.6), 0.5, position) :> Light, "constant"
                        SphereLight(Colour(1., 0.8, 0.6), 2., position, 0.05) :> Light, "sphere" ] do
        let steps = 400000
        let a, length = 10., 20.
        let ds = length / float steps
        let sigmaT = Volume.extinction volume
        let mutable sum = Colour(0., 0., 0.)
        for i in 0 .. steps - 1 do
            let s = (float i + 0.5) * ds
            let p = Point(-a + s, 0., 0.)
            let offset = position - p
            let r = offset.Magnitude
            let toLight = offset.Normalise
            let rho = Volume.density volume 0.
            let tauLight = Volume.densityIntegral volume p.Y toLight.Y r
            let tauView = rho * s
            let incoming = let h = HitPoint(p) in lamp.GetColour h * lamp.GetGeometricFactor h
            let phase = Atmosphere.henyeyGreenstein volume.LampAnisotropy (toLight * Vector(1., 0., 0.))
            let term (scatter: float) (extinction: float) (li: float) =
                scatter * rho * phase * li * exp (-extinction * (tauView + tauLight)) * ds
            sum <- sum + Colour(term volume.Scattering.R sigmaT.R incoming.R,
                                term volume.Scattering.G sigmaT.G incoming.G,
                                term volume.Scattering.B sigmaT.B incoming.B)
        let scattered, _ = estimate (medium volume [ lamp ]) unshadowed 8000
        let error = relativeError sum scattered
        Assert.True(error < 0.02, sprintf "volume-%s-lamp-matches-quadrature (relative error %g)" name error)

/// Transmittance through the medium lies in [0, 1] and never increases with distance, homogeneous or not,
/// for rays climbing, level and descending; the medium leaves rays that miss the region alone.
let private transmittanceMonotonic () =
    let mutable ok = true
    let mutable bounded = true
    for volume in [ slab; { slab with ScaleHeight = 0.5 }; { slab with ScaleHeight = 3.; BaseHeight = 1. } ] do
        for y in [ -1.; 0.; 0.7 ] do
            for dy in [ -0.9; -1e-9; 0.; 0.3; 1. ] do
                let mutable previous = white
                for distance in [ 0.; 1e-6; 0.1; 1.; 5.; 50.; 1e4 ] do
                    let t = Volume.transmittance volume y dy distance
                    for c, p in [ t.R, previous.R; t.G, previous.G; t.B, previous.B ] do
                        if not (c >= 0. && c <= 1.) then bounded <- false
                        if c > p + 1e-15 then ok <- false
                    previous <- t
        let m = medium volume []
        let mutable previous = white
        for distance in [ 0.; 5.; 10.; 15.; 25.; 30.; infinity ] do
            let struct (_, t) = m.VolumeSegment(Point(-20., 0.2, 0.), Vector(1., 0., 0.), distance, 0., 1UL, 0, unshadowed)
            if t.R > previous.R + 1e-15 || t.B > previous.B + 1e-15 then ok <- false
            previous <- t
    Assert.True(bounded, "volume-transmittance-in-unit-interval")
    Assert.True(ok, "volume-transmittance-monotonic-in-distance")
    let struct (s, t) =
        (medium slab []).VolumeSegment(Point(-20., 5., 0.), Vector(1., 0., 0.), infinity, 0., 1UL, 0, unshadowed)
    Assert.True(s.IsBlack && t.R = 1. && t.G = 1. && t.B = 1., "volume-misses-region-untouched")

let private sceneParts =
    lazy (
        let matte = MatteMaterial(Colour(0.2, 0.2, 0.2), 0.3, Colour(0.7, 0.5, 0.3), 0.8)
        let sphere = SphereShape(Point(0., 1., 0.), 1., Textures.mkMatTexture matte) :> Shape
        let floor = SphereShape(Point(0., -100., 0.), 100., Textures.mkMatTexture matte) :> Shape
        let sky = Textures.mkTexture (fun _ v -> EmissiveMaterial(Colour(0.4 + 0.4 * v, 0.5 + 0.3 * v, 0.9), 1.) :> Material)
        let env = EnvironmentLight(1e6, sky, multiJittered 2 83) :> Light
        let sun = DirectionalLight(Colour(1., 0.8, 0.6), 1.2, Vector(-0.6, 0.4, 0.5)) :> Light
        let lamp = PointLight(Colour(1., 0.9, 0.7), 0.6, Point(1.5, 2.5, 1.)) :> Light
        sphere, floor, [ sun; env; lamp ], regular 2)

let private render (integrator: IntegratorKind) (atmosphere: Atmosphere option) =
    let sphere, floor, lights, cameraSampler = sceneParts.Value
    let ambient = AmbientLight(Colour.Black, 0.)
    let scene =
        match atmosphere with
        | Some a -> Scene([ sphere; floor ], lights, ambient, 2, a)
        | None -> Scene([ sphere; floor ], lights, ambient, 2)
    let camera =
        PinholeCamera(Point(0., 1.5, 6.), Point(0., 1., 0.), Vector(0., 1., 0.), 2., 2., 2., 24, 24, cameraSampler)
    let options = { RenderOptions.Default with Integrator = integrator; Threads = Environment.ProcessorCount; Seed = 5 }
    Render(scene, camera, options).RenderLinear.Pixels

/// With no scattering or absorption the volume changes nothing, with or without the height fog; with
/// scattering it changes the image.
let private emptyVolumeIsIdentical () =
    let region = { Volume.Default with Min = Point(-4., -1., -4.); Max = Point(4., 4., 4.) }
    let empty = { region with Scattering = Colour(0., 0., 0.); Absorption = Colour(0., 0., 0.) }
    let fog = { Atmosphere.Default with Density = 0.05; ScaleHeight = 5. }
    for kind, name in [ Classic, "classic"; Path, "path" ] do
        let reference = render kind None
        let withEmpty = render kind (Some { Atmosphere.Default with Density = 0.; Volume = Some empty })
        Assert.True((reference = withEmpty), sprintf "volume-empty-identical-%s" name)
        let fogged = render kind (Some fog)
        let foggedEmpty = render kind (Some { fog with Volume = Some empty })
        Assert.True((fogged = foggedEmpty), sprintf "volume-empty-identical-with-fog-%s" name)
        let hazy = render kind (Some { Atmosphere.Default with Density = 0.; Volume = Some { region with Scattering = Colour(0.2, 0.2, 0.2) } })
        Assert.True((reference <> hazy), sprintf "volume-haze-changes-image-%s" name)

/// A sun above an opaque ceiling lights none of the haze below it: every pixel stays black. Without the
/// ceiling the same haze glows.
let private ceilingShadowsHaze () =
    let matte = MatteMaterial(Colour(0.5, 0.5, 0.5), 0., Colour(0.5, 0.5, 0.5), 1.)
    let ceiling = Box(Point(-100., 2., -100.), Point(100., 3., 100.), Textures.mkMatTexture matte, Textures.mkMatTexture matte,
                      Textures.mkMatTexture matte, Textures.mkMatTexture matte, Textures.mkMatTexture matte,
                      Textures.mkMatTexture matte) :> Shape
    let sun = DirectionalLight(Colour(1., 1., 1.), 5., Vector(0.3, 1., 0.2)) :> Light
    let volume =
        { Volume.Default with Min = Point(-20., -5., -20.); Max = Point(20., 1.9, 20.); Scattering = Colour(0.2, 0.2, 0.2) }
    let atmosphere = { Atmosphere.Default with Density = 0.; Volume = Some volume }
    let image shapes =
        let scene = Scene(shapes, [ sun ], AmbientLight(Colour.Black, 0.), 2, atmosphere)
        let camera = PinholeCamera(Point(0., 0., 6.), Point(0., 0., 0.), Vector(0., 1., 0.), 2., 2., 2., 12, 12, regular 1)
        Render(scene, camera, { RenderOptions.Default with Integrator = Path; Seed = 3 }).RenderLinear.Pixels
    let shadowed = image [ ceiling ]
    Assert.True(shadowed |> Array.forall (fun v -> v = 0.), "volume-shadowed-by-ceiling-is-black")
    let open' = image []
    Assert.True(open' |> Array.exists (fun v -> v > 0.), "volume-unshadowed-haze-glows")

let allTest () =
    slabMatchesAnalytic ()
    lampsMatchQuadrature ()
    transmittanceMonotonic ()
    emptyVolumeIsIdentical ()
    ceilingShadowsHaze ()
