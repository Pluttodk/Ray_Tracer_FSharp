module PbrTests

open System
open Assert
open Tracer.Basics
open Tracer.Basics.PathTracing
open Tracer.Animation
open Tracer.Basics.Render

let private lcg (state: uint64 ref) =
    state.Value <- state.Value * 6364136223846793005UL + 1442695040888963407UL
    float ((state.Value >>> 11) &&& ((1UL <<< 53) - 1UL)) / float (1UL <<< 53)

let private woAt cosTheta = Vector(sqrt (max 0. (1. - cosTheta * cosTheta)), 0., cosTheta)

/// A front-face hit on the z = 0 plane seen along -wo, so the integrators' local frame is the identity.
let private hitWith (material: Material) (wo: Vector) =
    let origin = Point(wo.X, wo.Y, wo.Z)
    HitPoint(Ray(origin, -wo), 1., Vector(0., 0., 1.), material, Shape.None)

let private surfaceOf (sample: PbrSample) (wo: Vector) =
    MaterialAdapter.surfaceAt (hitWith (PbrMaterial sample) wo) 1.

let private albedo (p: SurfaceParams) (wo: Vector) =
    let state = ref 0xA24BAED4963EE407UL
    let n = 60000
    let mutable sum = 0.
    for _ in 1 .. n do
        let s = Bsdf.sampleLocal p wo (lcg state) (lcg state) (lcg state)
        if s.Pdf > 0. || s.IsSpecular then sum <- sum + (s.Weight.R + s.Weight.G + s.Weight.B) / 3.
    sum / float n

/// White furnace through the PBR material: what the path tracer sees for it must neither create energy
/// nor, for white GGX metal and dielectric, lose more than a few percent of it.
let private whiteFurnace () =
    let white = Colour(1., 1., 1.)
    for roughness in [ 0.2; 0.55; 1.0 ] do
        for metallic in [ 0.; 1. ] do
            for cosTheta in [ 0.2; 0.7; 0.95 ] do
                let wo = woAt cosTheta
                let p = surfaceOf { PbrSample.defaults with BaseColour = white; Roughness = roughness; Metallic = metallic } wo
                let a = albedo p wo
                let label = sprintf "r%.2f-m%.0f-cos%.2f" roughness metallic cosTheta
                Assert.True(a <= 1.005, sprintf "pbr-furnace-%s-conserves (albedo %.4f)" label a)
                Assert.True(a >= 0.90, sprintf "pbr-furnace-%s-retains (albedo %.4f)" label a)
    // Sheen, diffuse transmission and a tilted normal redistribute energy but must not add any.
    let tilted = Vector(0.35, -0.2, 1.).Normalise
    for cosTheta in [ 0.2; 0.7; 0.95 ] do
        let wo = woAt cosTheta
        let sample =
            { PbrSample.defaults with
                BaseColour = white; Roughness = 0.55; Sheen = 1.; DiffuseTransmission = 0.6
                DiffuseTransmissionColour = white; Normal = tilted }
        let a = albedo (surfaceOf sample wo) wo
        Assert.True(a <= 1.005, sprintf "pbr-furnace-translucent-sheen-cos%.2f-conserves (albedo %.4f)" cosTheta a)
        Assert.True(a >= 0.80, sprintf "pbr-furnace-translucent-sheen-cos%.2f-retains (albedo %.4f)" cosTheta a)

/// MIS needs every sampled direction's pdf to match pdfLocal, including the new lobes.
let private samplePdfConsistency () =
    let configurations =
        [ "translucent", { PbrSample.defaults with BaseColour = Colour(0.3, 0.1, 0.1); Roughness = 0.6; DiffuseTransmission = 0.5; DiffuseTransmissionColour = Colour(0.9, 0.4, 0.2) }
          "normal-mapped", { PbrSample.defaults with Roughness = 0.4; Normal = Vector(-0.3, 0.25, 1.).Normalise }
          "sheen-metal", { PbrSample.defaults with Roughness = 0.3; Metallic = 0.5; Sheen = 0.5 } ]
    for name, sample in configurations do
        let state = ref 0x9E3779B97F4A7C15UL
        let mutable worst = 0.
        let mutable counted = 0
        let mutable transmitted = 0
        for _ in 1 .. 20000 do
            let wo = woAt (0.05 + 0.9 * lcg state)
            let p = surfaceOf sample wo
            let s = Bsdf.sampleLocal p wo (lcg state) (lcg state) (lcg state)
            if s.Pdf > 0. && not s.IsSpecular then
                let recomputed = Bsdf.pdfLocal p wo s.Direction
                worst <- max worst (abs (recomputed - s.Pdf) / max 1e-12 s.Pdf)
                counted <- counted + 1
                // The sample weight must be the evaluated BSDF over its pdf.
                let f = Bsdf.evalLocal p wo s.Direction
                worst <- max worst (abs (f.R / s.Pdf - s.Weight.R) / max 1e-9 s.Weight.R)
                if s.IsTransmitted then
                    transmitted <- transmitted + 1
                    if s.Direction.Z >= 0. then worst <- infinity
                elif s.Direction.Z <= 0. then worst <- infinity
        Assert.True(counted > 1000, sprintf "pbr-%s-produced-samples" name)
        Assert.True(worst < 1e-9, sprintf "pbr-%s-sample-pdf-eval-consistent (worst rel err %g)" name worst)
        if name = "translucent" then Assert.True(transmitted > 1000, "pbr-translucent-transmits")

/// Back light through a translucent surface: zero without the lobe, positive with it, and coloured by it.
let private diffuseTransmission () =
    let wo = woAt 0.8
    let behind = Vector(0.2, 0.1, -0.97).Normalise
    let opaque = surfaceOf { PbrSample.defaults with BaseColour = Colour(0.5, 0.5, 0.5) } wo
    let membrane = surfaceOf { PbrSample.defaults with BaseColour = Colour(0.5, 0.5, 0.5); DiffuseTransmission = 0.5; DiffuseTransmissionColour = Colour(1., 0.3, 0.1) } wo
    Assert.True((Bsdf.evalLocal opaque wo behind).IsBlack && Bsdf.pdfLocal opaque wo behind = 0., "pbr-opaque-blocks-back-light")
    let f = Bsdf.evalLocal membrane wo behind
    Assert.True(f.R > 0. && f.R > f.G && f.G > f.B && Bsdf.pdfLocal membrane wo behind > 0., "pbr-membrane-passes-tinted-back-light")

/// A flat normal map (0.5, 0.5, 1) decodes to +Z and must leave the BSDF bit-for-bit unchanged.
let private flatNormalMapIdentity () =
    let image = TextureFilter.create 2 2 3 (Array.init 12 (fun i -> if i % 3 = 2 then 1.f else 0.5f))
    let map u v =
        let struct (r, g, b, _) = TextureFilter.bilinear image Repeat Repeat u v
        Vector(2. * r - 1., 2. * g - 1., 2. * b - 1.)
    let plain = { PbrParams.defaults with BaseColour = Colour(0.6, 0.3, 0.2); Roughness = 0.45 }
    let mapped = { plain with NormalMap = Some map; NormalScale = 1.7 }
    let state = ref 0x14057B7EF767814FUL
    let mutable identical = true
    for _ in 1 .. 200 do
        let u, v = lcg state * 3. - 1., lcg state * 3. - 1.
        let a, b = PbrParams.sampleAt plain u v, PbrParams.sampleAt mapped u v
        identical <- identical && b.Normal.X = 0. && b.Normal.Y = 0. && b.Normal.Z = 1.
        let wo = woAt (0.1 + 0.85 * lcg state)
        let wi = let w = woAt (0.1 + 0.85 * lcg state) in Vector(-w.X * cos 1.3, w.X * sin 1.3, w.Z)
        let pa, pb = surfaceOf a wo, surfaceOf b wo
        let fa, fb = Bsdf.evalLocal pa wo wi, Bsdf.evalLocal pb wo wi
        identical <- identical && fa.R = fb.R && fa.G = fb.G && fa.B = fb.B && Bsdf.pdfLocal pa wo wi = Bsdf.pdfLocal pb wo wi
    Assert.True(identical, "pbr-flat-normal-map-is-identity")
    // And a tilted map really does tilt the shading normal.
    let tilted = PbrParams.sampleAt { plain with NormalMap = Some (fun _ _ -> Vector(0.3, 0., 1.).Normalise) } 0.5 0.5
    let p = surfaceOf tilted (woAt 0.9)
    Assert.True(abs p.Normal.Z < 0.999 && abs (p.Normal.Magnitude - 1.) < 1e-12, "pbr-tilted-normal-map-perturbs")

/// Bilinear filtering reproduces texels exactly at their centres and blends linearly between them.
let private bilinearSampling () =
    let w, h = 5, 3
    let data = Array.init (w * h * 3) (fun i -> float32 ((i * 37) % 23) / 22.f)
    let image = TextureFilter.create w h 3 data
    let mutable worst = 0.
    for mode in [ Repeat; ClampToEdge; MirroredRepeat ] do
        for y in 0 .. h - 1 do
            for x in 0 .. w - 1 do
                let s, t = (float x + 0.5) / float w, (float y + 0.5) / float h
                let struct (r, g, b, a) = TextureFilter.bilinear image mode mode s t
                let struct (er, eg, eb, _) = TextureFilter.texel image x y
                worst <- max worst (max (abs (r - er)) (max (abs (g - eg)) (max (abs (b - eb)) (abs (a - 1.)))))
    Assert.True(worst < 1e-12, sprintf "bilinear-exact-at-texel-centres (worst %g)" worst)
    // Halfway between texels (1,1) and (2,1).
    let struct (r, _, _, _) = TextureFilter.bilinear image Repeat Repeat (2. / float w) (1.5 / float h)
    let struct (r1, _, _, _) = TextureFilter.texel image 1 1
    let struct (r2, _, _, _) = TextureFilter.texel image 2 1
    Assert.True(abs (r - 0.5 * (r1 + r2)) < 1e-12, "bilinear-blends-midpoint")
    // Repeat wraps across the edge: half a texel left of the first centre blends the first and last texels.
    let struct (rw, _, _, _) = TextureFilter.bilinear image Repeat ClampToEdge 0. (0.5 / float h)
    let struct (ra, _, _, _) = TextureFilter.texel image 0 0
    let struct (rb, _, _, _) = TextureFilter.texel image (w - 1) 0
    Assert.True(abs (rw - 0.5 * (ra + rb)) < 1e-12, "bilinear-repeat-wraps")
    let struct (rc, _, _, _) = TextureFilter.bilinear image ClampToEdge ClampToEdge -3. 0.
    Assert.True(abs (rc - ra) < 1e-12, "bilinear-clamp-to-edge")
    Assert.True(TextureFilter.wrapIndex MirroredRepeat -1 w = 0 && TextureFilter.wrapIndex MirroredRepeat w w = w - 1, "wrap-mirrored")

/// The importer builds PBR materials and hands each one to the override hook by name.
let private importerOverride () =
    let path = IO.Path.Combine(IO.Path.GetTempPath(), $"raytracer-pbr-test-{Environment.ProcessId}.glb")
    try
        let demo = Demos.all |> List.head
        Gltf.save (demo.Build () |> AnimatedScene.validate) path 60. |> ignore
        let seen = Collections.Generic.List<string>()
        let options =
            { Gltf.ImportOptions.Default with
                MaterialOverride = fun name p ->
                    seen.Add name
                    { p with Roughness = 0.55; DiffuseTransmission = 0.25 } }
        let scene = (Gltf.load path options).Scene
        let frame = Frame.sceneAt scene 0. 0. 2
        let pose = AnimatedScene.cameraPose scene 0.
        let forward = (pose.LookAt - pose.Position).Normalise
        let struct (right, up) = PbrShading.tangentFrame forward
        let overridden =
            Seq.allPairs [ -10 .. 10 ] [ -10 .. 10 ] |> Seq.exists (fun (i, j) ->
                let direction = forward + (0.06 * float i) * right + (0.06 * float j) * up
                let ray = Ray(pose.Position, direction.Normalise)
                frame.Shapes |> List.exists (fun shape ->
                    let hit = shape.hitFunction ray
                    hit.DidHit &&
                    match hit.Material with
                    | :? PbrMaterial as m -> m.Sample.Roughness = 0.55 && m.Sample.DiffuseTransmission = 0.25
                    | _ -> false))
        Assert.True(seen.Count > 0 && seen |> Seq.forall (fun n -> n.StartsWith "material"), "gltf-material-override-sees-names")
        Assert.True(overridden, "gltf-material-override-applies")
    finally
        try IO.File.Delete path with _ -> ()

/// End to end through the path tracer: a sun behind a thin sheet lights its visible face only when the
/// sheet transmits diffusely (next-event estimation has to look behind the surface).
let private backLitSheet () =
    let render (sample: PbrSample) =
        let sheet = Disc(Point(0., 0., 0.), 1., Textures.mkMatTexture (PbrMaterial sample)) :> Shape
        let sun = DirectionalLight(Colour.White, 1., Vector(0.2, 0., -1.).Normalise) :> Light
        let scene = Scene([ sheet ], [ sun ], AmbientLight(Colour.Black, 0.), 3)
        let camera =
            PinholeCamera(Point(0., 0., 4.), Point(0., 0., 0.), Vector(0., 1., 0.), 2., 0.5, 0.5, 8, 8, Sampling.regular 2)
        let options = { RenderOptions.Default with Integrator = Path; Threads = 2; Seed = 3 }
        let film = Render(scene, camera, options).RenderLinear
        Array.average film.Pixels
    let opaque = render { PbrSample.defaults with BaseColour = Colour(0.5, 0.5, 0.5) }
    let sheet = render { PbrSample.defaults with BaseColour = Colour(0.5, 0.5, 0.5); DiffuseTransmission = 0.5 }
    // Lambertian transmission: 0.5 * (1 - specular albedo) * cos / pi of the unit sun, about 0.15.
    Assert.True((opaque = 0.), sprintf "path-opaque-sheet-dark-when-back-lit (%g)" opaque)
    Assert.True(sheet > 0.1 && sheet < 0.2, sprintf "path-translucent-sheet-glows-when-back-lit (%g)" sheet)

/// A translucent sheet in a uniform, importance-sampled environment reflects and transmits exactly its
/// BSDF's total albedo: the MIS weights of light sampling and BSDF sampling must also cover the
/// hemisphere behind the surface.
let private translucentFurnace () =
    let sample = { PbrSample.defaults with BaseColour = Colour(0.6, 0.6, 0.6); Roughness = 0.5; DiffuseTransmission = 0.5 }
    let wo = Vector(0., 0., 1.)
    let expected = albedo (surfaceOf sample wo) wo
    let sheet = Disc(Point(0., 0., 0.), 1., Textures.mkMatTexture (PbrMaterial sample)) :> Shape
    let envTexture = Textures.mkMatTexture (EmissiveMaterial(Colour.White, 1.))
    let env = EnvironmentLight(1e6, envTexture, Sampling.multiJittered 4 83, None, None, 32) :> Light
    let scene = Scene([ sheet ], [ env ], AmbientLight(Colour.Black, 0.), 3)
    let camera =
        PinholeCamera(Point(0., 0., 40.), Point(0., 0., 0.), Vector(0., 1., 0.), 2., 0.05, 0.05, 8, 8, Sampling.regular 8)
    let options = { RenderOptions.Default with Integrator = Path; Threads = 4; Seed = 11 }
    let film = Render(scene, camera, options).RenderLinear
    let measured = Array.average film.Pixels
    Assert.True(abs (measured - expected) < 0.02 * expected,
                sprintf "path-translucent-furnace (measured %.4f, BSDF albedo %.4f)" measured expected)

let allTest () =
    translucentFurnace ()
    backLitSheet ()
    bilinearSampling ()
    flatNormalMapIdentity ()
    diffuseTransmission ()
    samplePdfConsistency ()
    whiteFurnace ()
    importerOverride ()
