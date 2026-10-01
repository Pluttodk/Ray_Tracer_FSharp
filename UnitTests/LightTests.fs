module LightTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Render
open Tracer.BaseShape

// HDR environment maps (HdrImage.fs), sphere lights and many-light selection (LightSelection.fs).

let private constantImage width height (value: float32) =
    { Width = width; Height = height; Pixels = Array.create (width * height * 3) value }

/// A known RGBE pixel decodes to (r, g, b) * 2^(e - 136), in both scanline encodings, and
/// encode/decode round-trips within RGBE precision.
let private hdrRoundTrip () =
    let header = Text.Encoding.ASCII.GetBytes "#?RADIANCE\nFORMAT=32-bit_rle_rgbe\nEXPOSURE=1.0\n\n-Y 1 +X 2\n"
    let flat = Array.append header [| 128uy; 64uy; 32uy; 129uy; 0uy; 0uy; 0uy; 0uy |]
    let image = HdrImage.decode flat
    let f = Math.ScaleB(1., 129 - 136)
    Assert.True(image.Width = 2 && image.Height = 1, "hdr-flat-dimensions")
    Assert.True(abs (float image.Pixels.[0] - 128. * f) < 1e-6 && abs (float image.Pixels.[1] - 64. * f) < 1e-6
                && abs (float image.Pixels.[2] - 32. * f) < 1e-6 && image.Pixels.[3] = 0.f,
                sprintf "hdr-known-pixel (%f %f %f)" image.Pixels.[0] image.Pixels.[1] image.Pixels.[2])
    // A 37x5 image with runs, literals and a wide dynamic range.
    let w, h = 37, 5
    let pixels =
        Array.init (w * h * 3) (fun k ->
            let p, c = k / 3, k % 3
            let x, y = p % w, p / w
            if y = 1 then 0.75f
            elif y = 3 && x > 10 && x < 20 then 5000.f
            else float32 (Math.Pow(1.7, float ((x * 7 + y * 3 + c * 5) % 23 - 11)))
        )
    let source = { Width = w; Height = h; Pixels = pixels }
    for runLength in [ true; false ] do
        let decoded = HdrImage.decode (HdrImage.encode source runLength)
        let mutable worst = 0.
        for k in 0 .. pixels.Length - 1 do
            let p = k / 3
            let o = p * 3
            let m = float (max pixels.[o] (max pixels.[o + 1] pixels.[o + 2]))
            worst <- max worst (abs (float decoded.Pixels.[k] - float pixels.[k]) / m)
        Assert.True(decoded.Width = w && decoded.Height = h && worst < 1. / 128.,
                    sprintf "hdr-roundtrip-rle-%b (worst relative error %.5f)" runLength worst)
    // Run-length coding actually compresses the constant row.
    Assert.True((HdrImage.encode source true).Length < (HdrImage.encode source false).Length, "hdr-rle-compresses")

/// The mapping convention: the image centre looks down +X, u = 0.25 down -Z, u = 0.75 down +Z,
/// the top row up; a rotation of 90 degrees makes +X read what -Z read before.
let private hdrConvention () =
    let w, h = 64, 32
    let image = constantImage w h 0.f
    let mark u v (value: float32) =
        let x, y = int (u * float w), int (v * float h)
        let o = (y * w + x) * 3
        image.Pixels.[o] <- value; image.Pixels.[o + 1] <- value; image.Pixels.[o + 2] <- value
    // Pixel-sized markers: point lookups at pixel centres return them exactly.
    let at u v = HdrEnvironment.direction ((floor (u * float w) + 0.5) / float w) ((floor (v * float h) + 0.5) / float h)
    mark 0.5 0.5 1.f
    mark 0.25 0.5 2.f
    mark 0.75 0.5 3.f
    let lookup rotation (d: Vector) = (HdrEnvironment.sample image rotation d).R
    let centre = at 0.5 0.5
    Assert.True(centre.X > 0.99 && lookup 0. centre = 1., "hdr-centre-is-plus-x")
    let minusZ = at 0.25 0.5
    Assert.True(minusZ.Z < -0.99 && lookup 0. minusZ = 2., "hdr-quarter-is-minus-z")
    let plusZ = at 0.75 0.5
    Assert.True(plusZ.Z > 0.99 && lookup 0. plusZ = 3., "hdr-three-quarters-is-plus-z")
    Assert.True((HdrEnvironment.direction 0.3 0.).Y > 0.999, "hdr-top-row-is-up")
    // Blender Mapping-node rotation: lookup R_y(90) (1, 0, 0) = (0, 0, -1).
    let r = HdrEnvironment.rotate (Math.PI / 2.) (Vector(1., 0., 0.))
    Assert.True(abs (r.Z + 1.) < 1e-9, "hdr-rotation-matches-blender")
    let struct (u, v) = HdrEnvironment.uv (Vector(0.3, -0.4, 0.5))
    let back = HdrEnvironment.direction u v
    Assert.True((back - Vector(0.3, -0.4, 0.5).Normalise).Magnitude < 1e-9, "hdr-uv-direction-inverse")

/// A map with a bright spot plus a dim gradient, for the importance table tests.
let private spotImage () =
    let w, h = 256, 128
    let pixels =
        Array.init (w * h * 3) (fun k ->
            let p, c = k / 3, k % 3
            let x, y = p % w, p / w
            let spot = if abs (x - 77) <= 1 && abs (y - 40) <= 1 then 2000.f else 0.f
            spot + float32 (0.2 + 0.8 * float y / float h) * (if c = 2 then 1.5f else 1.f))
    { Width = w; Height = h; Pixels = pixels }

let private sphereIntegral (f: Vector -> float) rows columns =
    let mutable sum = 0.
    for j in 0 .. rows - 1 do
        let c0, c1 = 1. - 2. * float j / float rows, 1. - 2. * float (j + 1) / float rows
        let cosine = 0.5 * (c0 + c1)
        let sine = sqrt (1. - cosine * cosine)
        for i in 0 .. columns - 1 do
            let phi = 2. * Math.PI * (float i + 0.5) / float columns
            sum <- sum + f (Vector(sine * sin phi, cosine, sine * cos phi)) * (c0 - c1) * (2. * Math.PI / float columns)
    sum

/// The pixel-tabulated density integrates to one, samples report their own density, and the
/// estimator of the map's luminance integral is unbiased (with a rotation applied).
let private hdrDistribution () =
    let image = spotImage ()
    let rotation = 0.7
    let table = HdrEnvironment.distribution image rotation 128
    // Integrate on a grid nested in the table's cells (uniform in polar angle), which is exact
    // for a piecewise-constant density.
    let integral =
        let k = 3
        let rows, columns = table.Height * k, table.Width * k
        let mutable sum = 0.
        for j in 0 .. rows - 1 do
            let t0, t1 = Math.PI * float j / float rows, Math.PI * float (j + 1) / float rows
            let polar = 0.5 * (t0 + t1)
            for i in 0 .. columns - 1 do
                let phi = 2. * Math.PI * (float i + 0.5) / float columns
                let d = Vector(sin polar * sin phi, cos polar, sin polar * cos phi)
                sum <- sum + table.Pdf d * (cos t0 - cos t1) * (2. * Math.PI / float columns)
        sum
    Assert.True(abs (integral - 1.) < 0.01, sprintf "hdr-table-pdf-integrates-to-one (%.5f)" integral)
    let luminance d = let c = HdrEnvironment.sample image rotation d in 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B
    let mutable consistent = true
    let mutable estimate = 0.
    let count = 400000
    for index in 0 .. count - 1 do
        let u1, u2 = sample2D (mixKey (uint64 index + 99UL)) 0
        let struct (d, pdf) = table.Sample(u1, u2)
        if abs (pdf / table.Pdf d - 1.) > 1e-6 then consistent <- false
        estimate <- estimate + luminance d / pdf
    estimate <- estimate / float count
    let reference = sphereIntegral luminance 1200 2400
    Assert.True(consistent, "hdr-table-sample-pdf-matches")
    Assert.True(abs (estimate / reference - 1.) < 0.01, sprintf "hdr-table-estimator-unbiased (%.5f vs %.5f)" estimate reference)
    // The spot dominates the table: most samples land near it.
    let spot = HdrEnvironment.unrotate rotation (HdrEnvironment.direction (77.5 / 256.) (40.5 / 128.))
    let mutable near = 0
    for index in 0 .. 9999 do
        let u1, u2 = sample2D (mixKey (uint64 index + 7UL)) 0
        let struct (d, _) = table.Sample(u1, u2)
        if d * spot > cos (5. * Math.PI / 180.) then near <- near + 1
    Assert.True(near > 5000, sprintf "hdr-table-finds-the-spot (%d of 10000)" near)

/// Taking the sun out conserves energy: the clamped map plus the sun's irradiance carries what the
/// map did, and the sun points at the spot.
let private hdrSunExtraction () =
    let image = spotImage ()
    let rotation = -1.1
    let clamped, sun = HdrEnvironment.extractSun image rotation None 5.
    let spot = HdrEnvironment.unrotate rotation (HdrEnvironment.direction (77.5 / 256.) (40.5 / 128.))
    Assert.True(sun.Direction * spot > cos (0.5 * Math.PI / 180.), sprintf "hdr-sun-direction (%A)" sun.Direction)
    Assert.True(sun.Pixels = 9, sprintf "hdr-sun-pixels (%d)" sun.Pixels)
    let power (img: HdrImage) =
        let mutable total = 0.
        for row in 0 .. img.Height - 1 do
            let omega = 2. * Math.PI / float img.Width * (cos (Math.PI * float row / float img.Height) - cos (Math.PI * float (row + 1) / float img.Height))
            for x in 0 .. img.Width - 1 do
                let o = (row * img.Width + x) * 3
                total <- total + float img.Pixels.[o] * omega
        total
    let before, after = power image, power clamped
    Assert.True(abs ((after + sun.Irradiance.R) / before - 1.) < 1e-6, "hdr-sun-energy-conserved")
    Assert.True(sun.PowerFraction > 0.5, sprintf "hdr-sun-power-fraction (%.3f)" sun.PowerFraction)

let private renderCentre (scene: Scene) integrator (spp: int) =
    let camera = PinholeCamera(Point(0., 0., 4.), Point(0., 0., 0.), Vector(0., 1., 0.), 2., 2., 2., 32, 32, regular spp)
    let options = { RenderOptions.Default with Integrator = integrator; Threads = Environment.ProcessorCount; Seed = 11 }
    let film = Render(scene, camera, options).RenderLinear
    let mutable sum = 0.
    let mutable count = 0
    for y in 12 .. 19 do
        for x in 12 .. 19 do
            let offset = ((film.Height - 1 - y) * film.Width + x) * 3
            sum <- sum + (film.Pixels.[offset] + film.Pixels.[offset + 1] + film.Pixels.[offset + 2]) / 3.
            count <- count + 1
    sum / float count

/// The furnace with an HDR map: a diffuse sphere of albedo a under a constant map of radiance L
/// reflects a * L * intensity, in both integrators.
let private hdrFurnace () =
    for integrator in [ Path; Classic ] do
        let albedo = 0.5
        let material = MatteMaterial(Colour.Black, 0., Colour(albedo, albedo, albedo), 1.)
        let sphere = SphereShape(Point(0., 0., 0.), 1., Textures.mkMatTexture material) :> Shape
        let env = HdrEnvironment.light (constantImage 64 32 2.f) None 0.4 0.75 4 64 :> Light
        let scene = Scene([ sphere ], [ env ], AmbientLight(Colour.Black, 0.), 1)
        let measured = renderCentre scene integrator 8
        let expected = albedo * 2. * 0.75
        // The classic integrator reuses fixed jittered sample sets, which reads a little high.
        let tolerance = if integrator = Path then 0.02 else 0.03
        Assert.True(abs (measured - expected) / expected < tolerance,
                    sprintf "hdr-furnace-%A (measured %.4f, expected %.4f)" integrator measured expected)

/// A sphere light's irradiance on a facing surface is intensity / d^2 for any radius (outside it).
let private sphereLightIrradiance () =
    for radius in [ 0.; 0.05; 0.8 ] do
        let light = SphereLight(Colour(1., 0.5, 0.25), 3., Point(0., 2., 0.), radius)
        let normal = Vector(0., 1., 0.)
        let mutable sum = 0.
        let count = 20000
        for index in 0 .. count - 1 do
            let u1, u2 = sample2D (mixKey (uint64 index)) 0
            let s = light.Sample(Point.Zero, u1, u2)
            sum <- sum + s.Radiance.R * s.Weight * max 0. (s.Direction * normal)
        let measured = sum / float count
        // Exact for a sphere seen whole above the plane: pi L sin^2(theta_max) = I / d^2.
        Assert.True(abs (measured / (3. / 4.) - 1.) < 0.01, sprintf "sphere-light-irradiance-r%.2f (%.5f)" radius measured)
    Assert.True(abs ((SphereLight(Colour.White, 2., Point.Zero, 0.1)).Power - 8. * Math.PI) < 1e-9, "sphere-light-power")

let private manyLights () =
    let lights =
        [ for k in 0 .. 13 do
            let a = float k * 2.399
            let p = Point(2.5 * cos a, 1.5 * sin (1.7 * a), 2.5 * sin a + 1.)
            yield SphereLight(Colour(0.5 + 0.5 * cos a, 0.7, 0.5 + 0.5 * sin a), 0.5 + float (k % 4), p, (if k % 3 = 0 then 0. else 0.15)) :> Light
          let emitter = EmissiveMaterial(Colour(1., 0.9, 0.8), 2.)
          yield RectangleAreaLight(emitter, BaseRectangle(Point(-0.5, 2.5, -0.5), Point(-0.5, 2.5, 0.5), Point(0.5, 2.5, -0.5)), regular 1) :> Light
          yield RectangleAreaLight(emitter, BaseRectangle(Point(1.5, -2., 1.), Point(1.5, -2., 1.6), Point(2.1, -2., 1.)), regular 1) :> Light ]
    lights

/// Selection probabilities over the local lights sum to one for any receiver, and Select reports
/// the probability that Probability recomputes.
let private selectionProbabilities () =
    let lights = manyLights () |> List.toArray
    let all = Array.append lights [| DirectionalLight(Colour.White, 1., Vector(0., 1., 0.)) :> Light |]
    let selection = LightSelection(all)
    Assert.True(not selection.IsExhaustive && selection.Globals = [| all.Length - 1 |], "selection-splits-global-and-local")
    let mutable sumsOk = true
    let mutable consistent = true
    for index in 0 .. 199 do
        let a, b = sample2D (mixKey (uint64 index)) 0
        let c, d = sample2D (mixKey (uint64 index)) 1
        let p = Point(4. * a - 2., 4. * b - 2., 4. * c - 2.)
        let n = (HdrEnvironment.direction a d)
        let translucent = index % 5 = 0
        let total = selection.Locals |> Array.sumBy (fun i -> selection.Probability(p, n, translucent, i))
        // Exactly one without culling. Behind an opaque surface a subtree can turn out to hold no
        // facing light after all; that mass is lost (Select returns nothing), never invented.
        if (translucent && abs (total - 1.) > 1e-9) || total > 1. + 1e-9 then sumsOk <- false
        let struct (chosen, probability) = selection.Select(p, n, translucent, c)
        if chosen >= 0 && abs (probability - selection.Probability(p, n, translucent, chosen)) > 1e-12 then consistent <- false
        if chosen >= 0 && probability <= 0. then consistent <- false
    Assert.True(sumsOk, "selection-probabilities-sum-to-one")
    Assert.True(consistent, "selection-select-matches-probability")
    Assert.True(LightSelection(Array.truncate 8 lights).IsExhaustive, "selection-exhaustive-for-few-lights")

/// The many-light estimator converges to the exhaustive one (sphere lights and area lights, whose
/// BSDF-hit MIS weight includes the selection probability).
let private selectionMatchesExhaustive () =
    let material = MatteMaterial(Colour.Black, 0., Colour(0.6, 0.6, 0.6), 1.)
    let sphere = SphereShape(Point(0., 0., 0.), 1., Textures.mkMatTexture material) :> Shape
    let scene = Scene([ sphere ], manyLights (), AmbientLight(Colour.Black, 0.), 2)
    let previous = LightSelection.ExhaustiveLimit
    try
        for integrator in [ Path; Classic ] do
            LightSelection.ExhaustiveLimit <- 100
            let exhaustive = renderCentre scene integrator 12
            LightSelection.ExhaustiveLimit <- 8
            let selected = renderCentre scene integrator 12
            Assert.True(abs (selected / exhaustive - 1.) < 0.02,
                        sprintf "selection-matches-exhaustive-%A (%.5f vs %.5f)" integrator selected exhaustive)
    finally LightSelection.ExhaustiveLimit <- previous

let allTest () =
    hdrRoundTrip ()
    hdrConvention ()
    hdrDistribution ()
    hdrSunExtraction ()
    hdrFurnace ()
    sphereLightIrradiance ()
    selectionProbabilities ()
    selectionMatchesExhaustive ()
