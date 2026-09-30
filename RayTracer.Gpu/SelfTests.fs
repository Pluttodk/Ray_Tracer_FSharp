namespace Tracer.Gpu

open System
open System.IO
open System.Text.Json
open Tracer.SceneFormat
open DeviceMath

module SelfTests =
    let private material kind : DeviceMaterial =
        { Colour = v3 0.6f 0.4f 0.2f; AmbientColour = v3 0.2f 0.3f 0.4f
          SpecularColour = v3 0.9f 0.8f 0.7f; ReflectionColour = v3 0.8f 0.7f 0.6f
          Filter = one (); Ambient = 0.1f; Diffuse = 0.6f; Specular = 0.2f
          Reflectivity = 0.5f; Ior = 1.5f; Emission = 2.f; Kind = kind
          Exponent = 8; GlossExponent = 16; GlossySampleOffset = 0; GlossySampleSets = 1
          TextureOffset = 0; TextureWidth = 0; TextureHeight = 0 }

    let private triangle material objectId a b c =
        let e1, e2 = sub b a, sub c a
        let normal = normalize (cross e1 e2)
        { A = a; B = b; C = c; GeometricNormal = normal
          NormalA = normal; NormalB = normal; NormalC = normal
          UvA = v2 0.f 0.f; UvB = v2 0.f 0.f; UvC = v2 0.f 0.f
          Material = material; Object = objectId }

    let private scene () =
        let materials =
            Array.init 6 (fun kind ->
                let value = material kind
                if kind = 3 then { value with GlossySampleOffset = 1 } else value)
        let triangles =
            [| for index = 0 to 5 do
                   let left = float32 index - 3.f
                   let right = left + 1.f
                   let a, b, c, d = v3 left -2.f 0.f, v3 left 2.f 0.f, v3 right -2.f 0.f, v3 right 2.f 0.f
                   yield triangle index index a b c
                   yield triangle index index b d c |]
        let light radiance : DeviceLight =
            { Position = zero (); Direction = v3 0.f 0.f -1.f; EdgeU = zero (); EdgeV = zero ()
              Radiance = radiance; Area = 0.f; Kind = 1; SampleOffset = 0; SampleCount = 1; SampleSets = 1 }
        { Triangles = triangles
          Nodes = [| { Low = v3 -3.f -2.f 0.f; High = v3 3.f 2.f 0.f; Left = -1; Right = -1; Start = 0; Count = triangles.Length } |]
          PrimitiveIndices = Array.init triangles.Length id
          Materials = materials; ObjectMaterials = Array.init 6 id
          WorldToObject = Array.init 6 (fun _ -> identityTransform ())
          ObjectToWorld = Array.init 6 (fun _ -> identityTransform ())
          Lights = [| light (v3 0.9f 0.7f 0.5f); light (v3 0.2f 0.4f 0.6f) |]
          TexturePixels = [||]
          Samples = [| v2 0.5f 0.5f; v2 0.25f 0.25f; v2 0.25f 0.75f; v2 0.75f 0.25f; v2 0.75f 0.75f |]
          Camera = { Position = v3 0.f 0.f -3.f; U = v3 0.f 1.f 0.f; V = v3 1.f 0.f 0.f; W = v3 0.f 0.f -1.f
                     ViewDistance = 3.f; PixelWidth = 1.f; PixelHeight = 0.f }
          Ambient = v3 0.1f 0.1f 0.1f; Background = v3 0.11f 0.13f 0.17f
          CameraSampleOffset = 0; CameraSampleSets = 1; BvhDepth = 1 }

    /// Analytic reference radiance for one test pixel.
    ///
    /// The device renders at FP32, but this reference stays in FP64 and is
    /// narrowed only at the end. Computing the reference at FP32 too would let
    /// both sides make the same rounding error and the test would agree for the
    /// wrong reason; comparisons against it use an explicit FP32 tolerance
    /// instead of exact equality.
    /// Agreement bounds against the FP64 analytic references below.
    /// float32 epsilon is 1.1920929e-7; these are small multiples of it.
    [<Literal>]
    let private MaterialTolerance = 9.5e-7f

    /// Transport tolerance is dominated by the surface offset, not by raw
    /// rounding.
    ///
    /// Every spawned ray starts 128 ULP clear of the surface so it cannot
    /// re-hit it. At FP64 that is ~2.8e-14 and invisible; at FP32 it is
    /// ~1.5e-5 at unit scale, and each offset shortens the path slightly. That
    /// shows up directly in distance-dependent transport: for the coloured
    /// absorption case, d(value)/d(distance) is about 0.32, and three offsets
    /// along the path give ~1.5e-5 of error. This bound covers that with margin
    /// while staying far below any algorithmic divergence.
    [<Literal>]
    let private TransportTolerance = 5.e-5f

    let private expectedColour (scene: PreparedGpuScene) index =
        let material = scene.Materials.[index]
        let ior = double material.Ior
        let x = double index - 2.5
        let cosine = 3. / Math.Sqrt(9. + x * x)
        if material.Kind = 5 then scale material.Colour material.Emission
        elif material.Kind = 4 then
            let transmittedCosine = Math.Sqrt(1. - (1. - cosine * cosine) / (ior * ior))
            let rs = (cosine - ior * transmittedCosine) / (cosine + ior * transmittedCosine)
            let rp = (ior * cosine - transmittedCosine) / (ior * cosine + transmittedCosine)
            let fresnel = (rs * rs + rp * rp) / 2.
            scale scene.Background (float32 (fresnel + (1. - fresnel) / (ior * ior)))
        else
            let ambient = scale (mul material.AmbientColour scene.Ambient) material.Ambient
            let diffuse = scale material.Colour (float32 (double material.Diffuse / Math.PI))
            let specular =
                if material.Kind = 0 then zero ()
                else
                    scale material.SpecularColour
                          (float32 (double material.Specular * Math.Pow(cosine, double material.Exponent)))
            let local = add ambient (mul (add diffuse specular) (v3 1.1f 1.1f 1.1f))
            if material.Kind = 2 || material.Kind = 3 then
                add local (scale (mul scene.Background material.ReflectionColour) material.Reflectivity)
            else local

    let private quad material objectId a b c d =
        [| triangle material objectId a b c; triangle material objectId b d c |]

    let private box material objectId center u v w hx hy hz =
        let vertex x y z = add center (add (add (scale u (x * hx)) (scale v (y * hy))) (scale w (z * hz)))
        let vertices =
            [| vertex -1.f -1.f -1.f; vertex 1.f -1.f -1.f; vertex 1.f 1.f -1.f; vertex -1.f 1.f -1.f
               vertex -1.f -1.f 1.f; vertex 1.f -1.f 1.f; vertex 1.f 1.f 1.f; vertex -1.f 1.f 1.f |]
        [| 0, 3, 1; 1, 3, 2; 4, 5, 7; 5, 6, 7; 0, 4, 3; 3, 4, 7
           1, 2, 5; 2, 6, 5; 0, 1, 4; 1, 5, 4; 3, 7, 2; 2, 7, 6 |]
        |> Array.map (fun (a, b, c) -> triangle material objectId vertices.[a] vertices.[b] vertices.[c])

    let private prepareTest triangles materials objectMaterials lights origin direction background =
        let vertices = triangles |> Array.collect (fun triangle -> [| triangle.A; triangle.B; triangle.C |])
        let low =
            v3 ((vertices |> Array.minBy (fun vertex -> vertex.X)).X - 1e-6f)
               ((vertices |> Array.minBy (fun vertex -> vertex.Y)).Y - 1e-6f)
               ((vertices |> Array.minBy (fun vertex -> vertex.Z)).Z - 1e-6f)
        let high =
            v3 ((vertices |> Array.maxBy (fun vertex -> vertex.X)).X + 1e-6f)
               ((vertices |> Array.maxBy (fun vertex -> vertex.Y)).Y + 1e-6f)
               ((vertices |> Array.maxBy (fun vertex -> vertex.Z)).Z + 1e-6f)
        let w = neg (normalize direction)
        let v = frameV w
        { Triangles = triangles
          Nodes = [| { Low = low; High = high; Left = -1; Right = -1; Start = 0; Count = triangles.Length } |]
          PrimitiveIndices = Array.init triangles.Length id
          Materials = materials; ObjectMaterials = objectMaterials; Lights = lights; TexturePixels = [||]
          WorldToObject = Array.init objectMaterials.Length (fun _ -> identityTransform ())
          ObjectToWorld = Array.init objectMaterials.Length (fun _ -> identityTransform ())
          Samples = [| v2 0.5f 0.5f |]
          Camera = { Position = origin; U = cross w v; V = v; W = w; ViewDistance = 1.f; PixelWidth = 0.f; PixelHeight = 0.f }
          Ambient = zero (); Background = background
          CameraSampleOffset = 0; CameraSampleSets = 1; BvhDepth = 1 }

    let private testLight position radiance =
        { Position = position; Direction = zero (); EdgeU = zero (); EdgeV = zero ()
          Radiance = radiance; Area = 0.f; Kind = 0; SampleOffset = 0; SampleCount = 1; SampleSets = 1 }

    let private focusedCases () =
        let white =
            { material 0 with Colour = one (); Ambient = 0.f; Diffuse = 1.f; Specular = 0.f }
        let glass =
            { material 4 with Filter = v3 0.25f 0.5f 0.8f }
        let origin = v3 0.f 0.f -3.f
        let forward = v3 0.f 0.f 1.f
        let floor = quad 0 0 (v3 -4.f -4.f 0.f) (v3 -4.f 4.f 0.f) (v3 4.f -4.f 0.f) (v3 4.f 4.f 0.f)
        let behindLight =
            quad 0 2 (v3 -8.f -8.f -4.f) (v3 -8.f 8.f -4.f) (v3 8.f -8.f -4.f) (v3 8.f 8.f -4.f)
        let slab = box 0 0 (v3 0.f 0.f 0.5f) (v3 1.f 0.f 0.f) (v3 0.f 1.f 0.f) forward 2.f 2.f 0.5f
        let outerGlass = { glass with Filter = v3 0.6f 0.7f 0.9f }
        let innerGlass = { glass with Ior = 1.2f }
        let nestedGlass =
            prepareTest
                (Array.append
                    (box 0 0 (zero ()) (v3 1.f 0.f 0.f) (v3 0.f 1.f 0.f) forward 2.f 2.f 2.f)
                    (box 1 1 (zero ()) (v3 1.f 0.f 0.f) (v3 0.f 1.f 0.f) forward 1.f 1.f 1.f))
                [| outerGlass; innerGlass |] [| 0; 1 |] [||] (zero ()) forward (one ())
        let innerFresnel = ((innerGlass.Ior - outerGlass.Ior) / (innerGlass.Ior + outerGlass.Ior)) ** 2.f
        let expectedNested =
            scale (mul innerGlass.Filter outerGlass.Filter) ((1.f - innerFresnel) * 0.96f * innerGlass.Ior * innerGlass.Ior)
        let nestedOblique displacement =
            let direction = normalize (v3 displacement -displacement 1.f)
            // Reference transport stays in FP64 and is narrowed only at the end;
            // see expectedColour for why.
            let innerIor = double innerGlass.Ior
            let outerIor = double outerGlass.Ior
            let cosZ = double direction.Z
            let transmission (etaI: double) (etaT: double) (cosine: double) =
                let ratio = etaI / etaT
                let transmitted = Math.Sqrt(1. - ratio * ratio * (1. - cosine * cosine))
                let rs = (etaI * cosine - etaT * transmitted) / (etaI * cosine + etaT * transmitted)
                let rp = (etaT * cosine - etaI * transmitted) / (etaT * cosine + etaI * transmitted)
                transmitted, 1. - 0.5 * (rs * rs + rp * rp)
            let outerCosine, innerTransmission = transmission innerIor outerIor cosZ
            let _, outerTransmission = transmission outerIor 1. outerCosine
            let channel (innerFilter: float32) (outerFilter: float32) =
                float32 (Math.Pow(double innerFilter, 1. / cosZ) * Math.Pow(double outerFilter, 1. / outerCosine)
                         * innerTransmission * outerTransmission * innerIor * innerIor)
            let expected =
                v3 (channel innerGlass.Filter.X outerGlass.Filter.X)
                   (channel innerGlass.Filter.Y outerGlass.Filter.Y)
                   (channel innerGlass.Filter.Z outerGlass.Filter.Z)
            { nestedGlass with Camera = { nestedGlass.Camera with W = neg direction } }, expected
        let nestedNearDiagonal, expectedNearDiagonal = nestedOblique 0.0234375f
        let nestedWideDiagonal, expectedWideDiagonal = nestedOblique 0.4921875f
        let reflectionMaterial =
            { material 2 with Ambient = 0.f; Diffuse = 0.f; Specular = 0.f; Reflectivity = 0.75f }
        let emitter = { material 5 with Colour = v3 0.9f 0.4f 0.1f; Emission = 2.f }
        let reflectingFloor = floor |> Array.map (fun triangle -> { triangle with Material = 0; Object = 0 })
        let reflectedEmitter =
            quad 1 1 (v3 -8.f -8.f -5.f) (v3 8.f -8.f -5.f) (v3 -8.f 8.f -5.f) (v3 8.f 8.f -5.f)
        let reflectionScene =
            prepareTest (Array.append reflectingFloor reflectedEmitter) [| reflectionMaterial; emitter |] [| 0; 1 |]
                        [| testLight (v3 2.f 2.f -2.f) (one ()); testLight (v3 -2.f 2.f -2.f) (one ()) |]
                        origin forward (zero ())
        let expectedReflection = scale (mul emitter.Colour reflectionMaterial.ReflectionColour) (2.f * reflectionMaterial.Reflectivity)
        let shadowDirection = normalize (v3 1.f 0.f -1.f)
        let shadowU = frameV shadowDirection
        let shadowV = cross shadowDirection shadowU
        let blocker = box 1 1 (v3 1.f 0.f -1.f) shadowU shadowV shadowDirection 0.4f 0.4f (sqrt 2.f / 4.f)
        let shadowTriangles = Array.concat [| floor; blocker; behindLight |]
        let diffuseScale = 1.f / (sqrt 2.f * float32 Math.PI)
        let shadowLights =
            [| testLight (v3 -2.f 0.f -2.f) (v3 1.f 0.f 0.f); testLight (v3 2.f 0.f -2.f) (v3 0.f 1.f 0.f) |]
        let opaqueShadow =
            prepareTest shadowTriangles [| white; white |] [| 0; 1; 0 |] shadowLights origin forward (zero ())
        let tinyReceiver =
            quad 0 0 (v3 -1e-12f -1e-12f 0.f) (v3 -1e-12f 1e-12f 0.f)
                     (v3 1e-12f -1e-12f 0.f) (v3 1e-12f 1e-12f 0.f)
        let tinyBlocker =
            quad 0 1 (v3 -1e-12f -1e-12f -0.999e-12f) (v3 -1e-12f 1e-12f -0.999e-12f)
                     (v3 1e-12f -1e-12f -0.999e-12f) (v3 1e-12f 1e-12f -0.999e-12f)
        let tinyShadow =
            prepareTest (Array.append tinyReceiver tinyBlocker) [| white |] [| 0; 0 |]
                        [| testLight (v3 0.f 0.f -1e-12f) (one ()) |]
                        (v3 0.f 0.f -0.5e-12f) forward (zero ())
        let subFemtometreShadow =
            let triangles =
                Array.append tinyReceiver tinyBlocker
                |> Array.map (fun triangle ->
                    { triangle with A = scale triangle.A 1e-4f
                                    B = scale triangle.B 1e-4f
                                    C = scale triangle.C 1e-4f })
            prepareTest triangles [| white |] [| 0; 0 |]
                        [| testLight (v3 0.f 0.f -1e-16f) (one ()) |]
                        (v3 0.f 0.f -0.5e-16f) forward (zero ())
        let transparentShadow =
            prepareTest shadowTriangles [| white; glass |] [| 0; 1; 0 |]
                        [| testLight (v3 2.f 0.f -2.f) (one ()) |] origin forward (zero ())
        let expectedTransmission =
            let exponent = Math.Sqrt 2. / 2.
            v3 (float32 (Math.Pow(double glass.Filter.X, exponent)))
               (float32 (Math.Pow(double glass.Filter.Y, exponent)))
               (float32 (Math.Pow(double glass.Filter.Z, exponent)))
            |> fun absorption -> scale absorption (diffuseScale * 0.96f * 0.96f)
        let areaRadiance = v3 0.4f 0.7f 0.9f
        let areaEmitter = { material 5 with Colour = areaRadiance; Emission = 1.f }
        let areaGeometry =
            quad 1 1 (v3 -1.f -1.f -2.f) (v3 1.f -1.f -2.f) (v3 -1.f 1.f -2.f) (v3 1.f 1.f -2.f)
        let areaLight =
            { Position = v3 -1.f -1.f -2.f; Direction = forward; EdgeU = v3 2.f 0.f 0.f; EdgeV = v3 0.f 2.f 0.f
              Radiance = areaRadiance; Area = 4.f; Kind = 2; SampleOffset = 1; SampleCount = 4; SampleSets = 1 }
        let areaScene =
            { prepareTest (Array.append floor areaGeometry) [| white; areaEmitter |] [| 0; 1 |]
                          [| areaLight |] (v3 0.f 0.f -1.f) forward (zero ()) with
                Samples = [| v2 0.5f 0.5f; v2 (1.f / 3.f) (1.f / 3.f); v2 (1.f / 3.f) (2.f / 3.f)
                             v2 (2.f / 3.f) (1.f / 3.f); v2 (2.f / 3.f) (2.f / 3.f) |] }
        let areaDistanceSquared = 4.f + 2.f / 9.f
        let expectedArea =
            let d = double areaDistanceSquared
            scale areaRadiance (float32 (16. / (Math.PI * d * d)))
        let environmentRadiance = v3 0.3f 0.5f 0.7f
        let environment =
            { areaLight with Kind = 3; SampleCount = 1; SampleOffset = 0; Radiance = environmentRadiance }
        let environmentScene =
            prepareTest floor [| white |] [| 0 |] [| environment |] origin forward environmentRadiance
        let largeFloor =
            quad 0 0 (v3 -100000.f 0.f -100000.f) (v3 -100000.f 0.f 100000.f)
                     (v3 100000.f 0.f -100000.f) (v3 100000.f 0.f 100000.f)
        let environmentSamples =
            [| for x = 1 to 4 do
                   for y = 1 to 4 do yield v2 (float32 x / 5.f) (float32 y / 5.f)
               yield v2 0.5f 0.5f |]
        let largeEnvironmentScene =
            let inverse = { identityTransform () with RowX = v3 0.00001f 0.f 0.f; RowZ = v3 0.f 0.f 0.00001f }
            let forward = { identityTransform () with RowX = v3 100000.f 0.f 0.f; RowZ = v3 0.f 0.f 100000.f }
            { prepareTest largeFloor [| white |] [| 0 |]
                          [| { environment with SampleCount = 16 } |]
                          (v3 2.f 6.f 12.f) (v3 -2.f -3.5f -12.f) environmentRadiance with
                Samples = environmentSamples; CameraSampleOffset = 16
                Triangles =
                    largeFloor |> Array.map (fun triangle ->
                        { triangle with A = transformPoint inverse triangle.A
                                        B = transformPoint inverse triangle.B
                                        C = transformPoint inverse triangle.C })
                WorldToObject = [| inverse |]; ObjectToWorld = [| forward |] }
        let backgroundScene =
            prepareTest (behindLight |> Array.map (fun triangle -> { triangle with Object = 0 }))
                        [| white |] [| 0 |] [| environment |] origin forward environmentRadiance
        let textured =
            { white with Colour = one (); AmbientColour = one (); Ambient = 1.f
                         TextureWidth = 1; TextureHeight = 2 }
        let constantUvScene uvA uvB uvC material pixels =
            { prepareTest
                (floor |> Array.map (fun triangle -> { triangle with UvA = uvA; UvB = uvB; UvC = uvC }))
                [| material |] [| 0 |] [||] (v3 0.268736343f 0.484347f -3.f) forward (zero ()) with
                Ambient = one (); TexturePixels = pixels }
        let constantV =
            constantUvScene (v2 0.1f 1.f) (v2 0.5f 1.f) (v2 0.9f 1.f) textured [| v3 1.f 0.f 0.f; v3 0.f 0.f 1.f |]
        let constantU =
            constantUvScene (v2 1.f 0.1f) (v2 1.f 0.5f) (v2 1.f 0.9f)
                            { textured with TextureWidth = 2; TextureHeight = 1 }
                            [| v3 0.f 1.f 0.f; v3 1.f 0.f 0.f |]
        let beforeOne = MathF.BitDecrement 1.f
        let beforeV =
            constantUvScene (v2 0.1f beforeOne) (v2 0.5f beforeOne) (v2 0.9f beforeOne)
                            { textured with TextureHeight = 512 }
                            (Array.init 512 (fun row -> if row = 0 then v3 1.f 0.f 0.f else v3 0.f 0.f 1.f))
        let beforeU =
            constantUvScene (v2 beforeOne 0.1f) (v2 beforeOne 0.5f) (v2 beforeOne 0.9f)
                            { textured with TextureWidth = 512; TextureHeight = 1 }
                            (Array.init 512 (fun column -> if column = 511 then v3 1.f 0.f 0.f else v3 0.f 1.f 0.f))
        [| "material-change-on-reflection", reflectionScene, 1, expectedReflection
           "closed-slab-coloured-absorption",
           prepareTest slab [| glass |] [| 0 |] [||] origin forward (one ()), 2,
           add (v3 0.04f 0.04f 0.04f) (scale glass.Filter (0.96f * 0.96f))
           "camera-inside-closed-glass",
           prepareTest slab [| glass |] [| 0 |] [||] (v3 0.f 0.f 0.5f) forward (one ()), 1,
           scale (v3 (sqrt glass.Filter.X) (sqrt glass.Filter.Y) (sqrt glass.Filter.Z)) (0.96f * 1.5f * 1.5f)
           "nested-camera-inside-coloured-glass", nestedGlass, 2, expectedNested
           "nested-near-normal-shared-diagonal", nestedNearDiagonal, 2, expectedNearDiagonal
           "nested-oblique-shared-diagonal", nestedWideDiagonal, 2, expectedWideDiagonal
           "total-internal-reflection",
           prepareTest slab [| glass |] [| 0 |] [||] (v3 0.f 0.f 0.5f) (v3 0.9f 0.f (sqrt 0.19f)) (one ()), 1, zero ()
           "per-light-opaque-visibility-with-finite-segments", opaqueShadow, 0, v3 diffuseScale 0.f 0.f
           "small-scale-finite-shadow-preserves-near-light-blocker", tinyShadow, 0, zero ()
           "sub-femtometre-shadow-has-no-unit-epsilon-floor", subFemtometreShadow, 0, zero ()
           "coloured-transparent-finite-shadow", transparentShadow, 0, expectedTransmission
           "four-sample-rectangle-with-bounded-emitter-visibility", areaScene, 0, expectedArea
           "large-coordinate-environment-without-self-shadow", largeEnvironmentScene, 0, environmentRadiance
           "constant-v-clamp-boundary", constantV, 0, v3 1.f 0.f 0.f
           "constant-u-clamp-boundary", constantU, 0, v3 1.f 0.f 0.f
           "next-down-v-clamps-to-top-row-512", beforeV, 0, v3 1.f 0.f 0.f
           "next-down-u-clamps-to-last-column-512", beforeU, 0, v3 1.f 0.f 0.f
           "constant-environment-direct-light", environmentScene, 0, environmentRadiance
           "constant-environment-background", backgroundScene, 0, environmentRadiance |]

    let run (reportPath: string) =
        let scene = scene ()
        let settings =
            { SceneFiles.preset "quick" with
                Width = 6; Height = 1; CameraSamples = 1
                LightSamples = 1; GlossySamples = 4; MaxBounces = 1
                // The shared preset is FP64 for the CPU workers; CUDA is FP32.
                Precision = "float32" }
        let rendered = CudaRenderer.render scene settings
        let expected = Array.init 6 (expectedColour scene)
        let errors =
            Array.map2 (fun (actual: V3) (reference: V3) ->
                max (abs (actual.X - reference.X)) (max (abs (actual.Y - reference.Y)) (abs (actual.Z - reference.Z))))
                rendered.Pixels expected
        let maxError = Array.max errors
        // FP32 against an FP64 analytic reference. The old bound was 1e-11,
        // which only held while the device was FP64 too. Eight FP32 epsilons
        // (~9.5e-7) leaves room for rounding to accumulate through a shading
        // path while staying far below any real algorithmic divergence.
        if maxError > MaterialTolerance then
            invalidOp $"CUDA classic material self-test failed: maximum absolute RGB error {maxError:G17}."
        let focused =
            focusedCases ()
            |> Array.map (fun (name, prepared, depth, expected) ->
                let options =
                    { settings with
                        Width = 1; Height = 1; GlossySamples = 1; MaxBounces = depth
                        LightSamples = if prepared.Lights.Length = 0 then 1 else prepared.Lights |> Array.map (fun light -> light.SampleCount) |> Array.max }
                let result = CudaRenderer.render prepared options
                let actual = result.Pixels.[0]
                let error = max (abs (actual.X - expected.X)) (max (abs (actual.Y - expected.Y)) (abs (actual.Z - expected.Z)))
                if error > TransportTolerance then
                    invalidOp $"CUDA {name} failed: max error {error:G17}; actual={actual}; expected={expected}."
                {| name = name; expected = expected; actual = actual; maxAbsoluteRgbError = error; traceMs = result.TraceMs |})
        let rejectedBranching =
            try
                CudaRenderer.render scene { settings with GlossySamples = 4096; MaxBounces = 32 } |> ignore
                false
            with :? InvalidOperationException as error -> error.Message.Contains("pending rays", StringComparison.Ordinal)
        if not rejectedBranching then invalidOp "CUDA failed to reject an unsafe ray-stack capacity request."
        let rejectedInvalidRay =
            let invalid = { scene with Camera = { scene.Camera with Position = v3 Single.PositiveInfinity 0.f -3.f } }
            try
                CudaRenderer.render invalid settings |> ignore
                false
            with :? CudaExecutionException as error -> error.InvalidPixels > 0
        if not rejectedInvalidRay then invalidOp "CUDA treated a non-finite ray as a normal background miss."
        let report =
            {| status = "passed"; device = rendered.Device; precision = "float32"
               cases = [| "lambert"; "phong"; "mirror"; "glossy-four-children"; "fresnel-interface"; "front-face-emission" |]
               twoDirectLights = true; maxAbsoluteRgbError = maxError; perCaseErrors = errors
               expected = expected; actual = rendered.Pixels
               focusedTransportCases = focused; unsafeBranchingRejectedBeforeLaunch = rejectedBranching
               invalidRayRejectedOnDevice = rejectedInvalidRay
               compileMs = rendered.CompileMs; uploadMs = rendered.UploadMs; traceMs = rendered.TraceMs
               downloadMs = rendered.DownloadMs; cleanupMs = rendered.CleanupMs
               deviceBytes = rendered.PeakDeviceBytes; rayStackCapacity = rendered.RayStackCapacity
               note = "Actual CUDA full integration kernel versus independent analytical test expectations, not a CPU accelerator." |}
        let json = JsonSerializer.Serialize(report, SceneFiles.jsonOptions)
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath reportPath)) |> ignore
        File.WriteAllText(reportPath, json + Environment.NewLine)
        printfn "%s" json
