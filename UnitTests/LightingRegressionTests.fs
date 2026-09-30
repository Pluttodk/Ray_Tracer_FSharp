module LightingRegressionTests

open System
open System.Threading
open System.Threading.Tasks
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.BaseShape

let private close name expected actual =
    Assert.True(abs (expected - actual) <= 1e-9 * max 1. (abs expected), name)

let private colour name (expected: Colour) (actual: Colour) =
    close (name + " R") expected.R actual.R
    close (name + " G") expected.G actual.G
    close (name + " B") expected.B actual.B

let private rejects name action =
    let mutable rejected = false
    try action ()
    with :? ArgumentException -> rejected <- true
    Assert.True(rejected, name)

let private brute (shapes: Shape array) =
    { new IRayQuery with
        member _.Closest(ray, minimum, maximum) =
            let mutable closest = HitPoint ray
            let mutable limit = maximum
            for shape in shapes do
                let hit = shape.hitFunction ray
                if hit.DidHit && hit.Time > minimum && hit.Time < limit then
                    closest <- hit
                    limit <- hit.Time
            closest
        member _.Any(ray, minimum, maximum) =
            shapes |> Array.exists (fun shape ->
                let hit = shape.hitFunction ray
                hit.DidHit && hit.Time > minimum && hit.Time < maximum) }

let allTest () =
    let blackAmbient = AmbientLight(Colour.Black, 0.)
    let matte = MatteMaterial(Colour.White, 0., Colour.White, 1.)
    let ray = Ray(Point(0., 0., 3.), Vector(0., 0., -1.))
    let hit = HitPoint(ray, 1., Vector(0., 0., 1.), matte, Shape.None)

    Assert.Equal((0., 0.), mapToDisc(0.5, 0.5), "disk-center-is-finite")
    for sample in [ (0., 0.); (0., 1.); (1., 0.); (1., 1.); (0.5, 0.5) ] do
        let x, y = mapToDisc sample
        Assert.True(Double.IsFinite x && Double.IsFinite y && x * x + y * y <= 1. + 1e-12, "disk-sample-in-unit-disk")
    rejects "zero-sampler-count" (fun () -> regular 0 |> ignore)
    rejects "empty-sampler-array" (fun () -> Sampler([||]) |> ignore)
    setRandomSeed 19
    let first = multiJittered 4 3
    setRandomSeed 19
    let second = multiJittered 4 3
    for key = 0 to 2 do
        Assert.Equal(first.SampleSetAt(uint64 key), second.SampleSetAt(uint64 key), "seeded-sampling-repeats")
    let expected = Array.init 64 (fun index -> first.SampleSetAt(mixKey (uint64 index)))
    let actual = Array.zeroCreate<(float * float) array> 64
    Parallel.For(0, 64, fun index -> actual.[index] <- first.SampleSetAt(mixKey (uint64 index))) |> ignore
    Assert.Equal(expected, actual, "sampling-independent-of-scheduling")
    Assert.True(sampleKey 42 1 2 <> sampleKey 42 2 1, "sample-key-dimensions-are-separated")
    let rooks = (nRooks 32 1).SampleSetAt 0UL
    let columns = rooks |> Array.map (fun (x, _) -> int (x * 32.)) |> Array.distinct
    let rows = rooks |> Array.map (fun (_, y) -> int (y * 32.)) |> Array.distinct
    Assert.Equal(32, columns.Length, "n-rooks-one-per-column")
    Assert.Equal(32, rows.Length, "n-rooks-one-per-row")

    let fresnel, transmitted = SurfaceSampling.dielectric (Vector(0., 0., -1.)) (Vector(0., 0., 1.)) 1. 1.5
    close "normal-incidence-fresnel" 0.04 fresnel
    Assert.True(transmitted.IsSome, "normal-incidence-transmits")
    let tir, missing = SurfaceSampling.dielectric (Vector(sqrt 0.75, 0., -0.5)) (Vector(0., 0., 1.)) 1.5 1.
    close "total-internal-reflection" 1. tir
    Assert.True(missing.IsNone, "tir-does-not-construct-nan-transmission")
    let matched, grazing = SurfaceSampling.dielectric (Vector(1., 0., 0.)) (Vector(0., 0., 1.)) 1. 1.
    close "matched-indices-no-reflection" 0. matched
    Assert.True(grazing.IsSome, "matched-indices-grazing-transmission")
    colour "beer-absorption" (Colour(0.25, 0.0625, 1.)) (MaterialTransport.absorption (Colour(0.5, 0.25, 1.)) 2.)

    let glossy = MatteGlossyReflectiveMaterial(Colour.Black, 0., Colour.Black, 0., Colour(0.8, 0.4, 0.2), 0.5, 32, regular 4)
    let glossyHit = HitPoint(ray, 1., Vector(0., 0., 1.), glossy, Shape.None)
    let children = MaterialTransport.scatter glossyHit 0UL 1. 1.
    colour "glossy-sample-count-normalization" (Colour(0.4, 0.2, 0.1)) (children |> Array.fold (fun sum child -> sum + child.Weight) Colour.Black)
    Assert.True(children |> Array.forall (fun child -> child.Ray.GetOrigin.Z > glossyHit.Point.Z), "glossy-rays-use-offset-origins")

    let emitter = EmissiveMaterial(Colour.White, 2.)
    let area = RectangleAreaLight(emitter, BaseRectangle(Point.Zero, Point(0., 3., 0.), Point(2., 0., 0.)), regular 2)
    close "area-density-is-reciprocal-area" (1. / 6.) (area.GetProbabilityDensity hit)
    let transformation = Transformation.mergeTransformations [ Transformation.scale 2. 3. 4.; Transformation.rotateX(Math.PI / 2.) ]
    let moved = TransformLight.transformLight area transformation :?> AreaLight
    let surface = moved.SampleSurface(Point.Zero, 0.5, 0.5)
    close "transformed-area-density" (1. / 36.) surface.AreaPdf
    close "transformed-light-normal-y" -1. surface.Normal.Y
    close "transformed-light-normal-z" 0. surface.Normal.Z

    let reflectedEmitter = EmissiveMaterial(Colour(1., 2., 3.), 1.)
    let plane = InfinitePlane(Textures.mkMatTexture(MatteReflectiveMaterial(Colour.Black, 0., Colour.Black, 0., Colour.White, 0.5))) :> Shape
    let sphere = SphereShape(Point(0., 0., 3.), 0.5, Textures.mkMatTexture reflectedEmitter) :> Shape
    let shapes = [| plane; sphere |]
    let scene = Scene(List.ofArray shapes, [], blackAmbient, 2)
    let integrator = ClassicIntegrator(scene, brute shapes, true, CancellationToken.None)
    colour "mirror-sees-emitter-without-explicit-lights" (Colour(0.5, 1., 1.5))
        (integrator.Trace(Ray(Point(0., 0., 1.), Vector(0., 0., -1.)), 0UL))
    let unusedLights =
        [ PointLight(Colour.White, 1., Point(0., 4., 2.)) :> Light
          PointLight(Colour.White, 1., Point(2., 4., 2.)) :> Light ]
    let moreLights = ClassicIntegrator(Scene(List.ofArray shapes, unusedLights, blackAmbient, 2), brute shapes, true, CancellationToken.None)
    colour "secondary-contributions-are-not-counted-per-light" (Colour(0.5, 1., 1.5))
        (moreLights.Trace(Ray(Point(0., 0., 1.), Vector(0., 0., -1.)), 0UL))

    let query = brute [| sphere |]
    let visibility = ClassicIntegrator(Scene([sphere], [], blackAmbient, 0), query, true, CancellationToken.None)
    let toSource = Ray(Point.Zero, Vector(0., 0., 1.))
    colour "blocker-behind-light-does-not-shadow" Colour.White (visibility.Visibility(toSource, 1.))
    colour "emissive-geometry-can-occlude" Colour.Black (visibility.Visibility(toSource, 4.))

    for scale in [ 1e-15; 1.; 1e15 ] do
        for opaqueOnly in [ true; false ] do
            let receiver = InfinitePlane(Textures.mkMatTexture matte) :> Shape
            let blocker = SphereShape(Point(0., 0., scale), 0.25 * scale, Textures.mkMatTexture matte) :> Shape
            let light = PointLight(Colour.White, 1., Point(0., 0., 2. * scale)) :> Light
            let primary = Ray(Point(3. * scale, 0., 3. * scale), Vector(-1., 0., -1.).Normalise)
            for blocked in [ true; false ] do
                let shapes = if blocked then [| receiver; blocker |] else [| receiver |]
                let integrator =
                    ClassicIntegrator(Scene(List.ofArray shapes, [light], blackAmbient, 0), brute shapes, opaqueOnly, CancellationToken.None)
                colour $"point-shadow-scale-{scale}-{opaqueOnly}-{blocked}"
                    (if blocked then Colour.Black else Colour.White * (1. / Math.PI))
                    (integrator.Trace(primary, 0UL))

    let whiteEnvironment = EnvironmentLight(100., Textures.mkMatTexture(EmissiveMaterial(Colour.White, 1.)), regular 1) :> Light
    let glass radius filter =
        SphereShape(Point.Zero, radius, Textures.mkMatTexture(TransparentMaterial(filter, Colour.White, 1., 1.))) :> Shape
    let tinted = glass 1. (Colour(0.5, 0.25, 1.))
    let glassQuery = brute [| tinted |]
    let glassIntegrator = ClassicIntegrator(Scene([tinted], [whiteEnvironment], blackAmbient, 4), glassQuery, false, CancellationToken.None)
    colour "closed-glass-absorbs-over-physical-diameter" (Colour(0.25, 0.0625, 1.))
        (glassIntegrator.Trace(Ray(Point(0., 0., 3.), Vector(0., 0., -1.)), 0UL))
    colour "camera-starting-inside-glass" (Colour(0.5, 0.25, 1.))
        (glassIntegrator.Trace(Ray(Point.Zero, Vector(0., 0., -1.)), 0UL))
    colour "transparent-shadow-is-coloured-not-opaque" (Colour(0.25, 0.0625, 1.))
        (glassIntegrator.Visibility(Ray(Point(0., 0., 3.), Vector(0., 0., -1.)), 6.))
    let outer = glass 2. (Colour(0.5, 1., 1.))
    let inner = glass 0.5 (Colour(1., 0.5, 1.))
    let nested = [| outer; inner |]
    let nestedIntegrator = ClassicIntegrator(Scene(List.ofArray nested, [whiteEnvironment], blackAmbient, 4), brute nested, false, CancellationToken.None)
    colour "nested-glass-restores-enclosing-medium" (Colour(0.125, 0.5, 1.))
        (nestedIntegrator.Trace(Ray(Point(0., 0., 4.), Vector(0., 0., -1.)), 0UL))
    colour "camera-starting-inside-nested-glass" (Colour(0.5 ** 1.5, sqrt 0.5, 1.))
        (nestedIntegrator.Trace(Ray(Point.Zero, Vector(0., 0., -1.)), 0UL))
    let matchedMedium =
        SphereShape(Point.Zero, 1., Textures.mkMatTexture(TransparentMaterial(Colour.White, Colour.White, 1.33, 1.33))) :> Shape
    let matchedIntegrator =
        ClassicIntegrator(Scene([matchedMedium], [whiteEnvironment], blackAmbient, 2), brute [|matchedMedium|], false, CancellationToken.None)
    colour "declared-exterior-ior-is-not-assumed-air" Colour.White
        (matchedIntegrator.Trace(Ray(Point(0., 0., 3.), Vector(0., 0., -1.)), 0UL))
    colour "shadow-honors-declared-matched-indices" Colour.White
        (matchedIntegrator.Visibility(Ray(Point(0., 0., 3.), Vector(0., 0., -1.)), 6.))

    let renderScene = Scene([ SphereShape(Point.Zero, 1., Textures.mkMatTexture glossy) ], [whiteEnvironment], blackAmbient, 2)
    let camera = PinholeCamera(Point(0., 0., 4.), Point.Zero, Vector(0., 1., 0.), 2., 2., 1.5, 24, 18, multiJittered 2 5)
    let renderOptions =
        { RenderOptions.Default with Seed = 42; Threads = 1; TileSize = 5
                                     Acceleration = Some Acceleration.Acceleration.FlatBVH }
    let serial = Render.Render(renderScene, camera, renderOptions).RenderLinear
    let parallel = Render.Render(renderScene, camera, { renderOptions with Threads = 4; TileSize = 8 }).RenderLinear
    Assert.Equal(serial.Pixels, parallel.Pixels, "film-is-independent-of-threads-and-tiles")

    rejects "negative-colour-scale-is-not-white" (fun () -> Colour.White.Scale(-1.) |> ignore)
    rejects "zero-colour-divisor" (fun () -> Colour.White / 0. |> ignore)
    Assert.Equal(255, Colour(1e100, 1e100, 1e100).ToColor.R |> int, "hdr-clamps-before-integer-conversion")
    let srgb = Colour(0.0031308, 0.5, 1.).ToDisplayColor("srgb")
    Assert.Equal((10, 187, 255), (int srgb.R, int srgb.G, int srgb.B), "explicit-srgb-transfer")
