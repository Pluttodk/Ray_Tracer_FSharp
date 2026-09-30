#if LEGACY
#r "../benchmarks/legacy/Runner/bin/Release/net10.0/LegacyRayTracer.dll"
#r "../benchmarks/legacy/Runner/bin/Release/net10.0/SceneFormat.dll"
#r "../benchmarks/legacy/Runner/bin/Release/net10.0/LegacyRunner.dll"
#else
#r "../BenchmarkRunner/bin/Release/net10.0/Basics.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/SceneFormat.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll"
#endif

open System
open System.IO
open Tracer
open Tracer.Basics
open Tracer.Basics.Transformation
open Tracer.Benchmarks
open Tracer.SceneFormat

let mutable assertions = 0
let check name condition =
    if not condition then failwith $"FAIL: {name}"
    assertions <- assertions + 1
let close a b = abs (a - b) < 1e-10
let samePoint (a: Point) (b: Point) = close a.X b.X && close a.Y b.Y && close a.Z b.Z
for values in
    [ SceneFiles.identity
      [| 2.; 0.4; -0.3; 5.; -0.2; 0.5; 0.1; -4.; 0.3; -0.1; -1.5; 3.; 0.; 0.; 0.; 1. |]
      [| 0.; 0.; 2.; 3.; 0.; -2.; 0.; 4.; 0.5; 0.; 0.; -1.; 0.; 0.; 0.; 1. |] ] do
    let transform = SceneBuilder.affineTransform values
    for p in [ API.mkPoint 0. 0. 0.; API.mkPoint 1. 2. 3.; API.mkPoint -2. 0.5 4. ] do
        let world = transformPoint (p, getMatrix transform)
        let expected =
            API.mkPoint (values.[0] * p.X + values.[1] * p.Y + values.[2] * p.Z + values.[3])
                        (values.[4] * p.X + values.[5] * p.Y + values.[6] * p.Z + values.[7])
                        (values.[8] * p.X + values.[9] * p.Y + values.[10] * p.Z + values.[11])
        check "row-major transform matches declared geometry" (samePoint expected world)
        check "actual affine inverse, including shear/reflection" (samePoint p (transformPoint (world, getInvMatrix transform)))
let settings = SceneFiles.preset "quick"
for name in [ "regular"; "multi-jittered" ] do
    for count in [ 1; 4; 16; 64 ] do
        check "sampler uses actual counts, not squared request" ((SceneBuilder.sampler { settings with Sampler = name } count).SampleCount = count)
let throws action = try action (); false with _ -> true
check "nonsquare sample count rejected" (throws (fun () -> SceneBuilder.sampler settings 3 |> ignore))
check "singular transform rejected" (throws (fun () -> SceneBuilder.affineTransform (Array.zeroCreate 16) |> ignore))
ScenePolicy.validateVariants "gold-dragon" [| "authored" |]
check "extra remains absent from primary scene matrix" (not (Array.contains "gold-dragon" (ScenePolicy.selectScenes "all")))
check "shared worker policy rejects a gold dragon glass sweep"
    (throws (fun () -> ScenePolicy.validateVariants "gold-dragon" [| "glass" |]))
for direction in [ [| 0.; -1.; 0. |]; [| 1.; 2.; 3. |]; [| 0.; 0.; -1. |] ] do
    let light =
        { Id = "rectangle"; Kind = LightKind.Rectangle; Position = [| 3.; 4.; 5. |]; Direction = direction
          Size = [| 2.; 3. |]; Colour = [| 1.; 1.; 1. |]; Intensity = 1. }
    let transform = SceneBuilder.rectangleTransform light
    check "rectangle transform centers original canonical rectangle"
        (samePoint (API.mkPoint 3. 4. 5.) (transformPoint (API.mkPoint 1. 1.5 0., getMatrix transform)))
    let normal = (transformVector (API.mkVector 0. 0. 1., getMatrix transform)).Normalise
    let expected = (API.mkVector direction.[0] direction.[1] direction.[2]).Normalise
    check "rectangle emitting normal follows declaration" (close normal.X expected.X && close normal.Y expected.Y && close normal.Z expected.Z)

check "null render settings rejected" (throws (fun () -> SceneFiles.validateSettings Unchecked.defaultof<RenderSettings> |> ignore))
let environment =
    { Id = "environment"; Kind = LightKind.Environment; Position = [|0.;0.;0.|]; Direction = [|0.;0.;0.|]
      Size = [|1000000.;0.|]; Colour = [|0.2;0.4;0.8|]; Intensity = 0.5 }
let scene =
    { SchemaVersion = 1; Id = "environment-contract"; Title = "Environment contract"; Description = "Extra corrected-semantics scene"
      Camera =
        { Position = [|0.;0.;3.|]; Target = [|0.;0.;0.|]; Up = [|0.;1.;0.|]
          ViewDistance = 1.; ViewWidth = 1.; ViewHeight = 1.; LensRadius = 0.; FocusDistance = 3. }
      AmbientColour = [|0.;0.;0.|]; AmbientIntensity = 0.; MaxBounces = 0
      Materials = [||]; Meshes = [||]; Objects = [||]; Lights = [|environment|]; Subjects = [||] }
check "environment requires explicit positive legacy radius"
    (throws (fun () -> SceneFiles.validate { scene with Lights = [|{ environment with Size = [|0.;0.|] }|] } |> ignore))
let directory = Path.Combine(Path.GetTempPath(), "raytracer-environment-" + Guid.NewGuid().ToString("N"))
Directory.CreateDirectory directory |> ignore
try
    let file = Path.Combine(directory, "scene.json")
    SceneFiles.save file scene
    let built =
        SceneBuilder.build file "authored"
            { settings with Width = 4; Height = 4; CameraSamples = 1; LightSamples = 1; MaxBounces = 0 }
            (fun _ -> failwith "The environment fixture must not load a texture.")
    check "environment constructor is shared by both adapters" (built.Scene.Lights.Length = 1)
#if LEGACY
    check "legacy environment keeps its enclosing geometry" (built.Scene.Shapes.Length = 1)
#else
    check "modern environment is miss radiance, not scene geometry" built.Scene.Shapes.IsEmpty
#endif
    let material: MaterialSpec =
        { Id = "texels"; Kind = MaterialKind.Matte; Colour = [|1.;1.;1.|]
          AmbientColour = null; SpecularColour = null; ReflectionColour = null
          Ambient = 1.; Diffuse = 1.; Specular = 0.; Exponent = 0
          Reflectivity = 0.; GlossExponent = 0; Ior = 1.5; Filter = [|1.;1.;1.|]
          Emission = 0.; Texture = "fixture.png" }
    let meshFile = Path.Combine(directory, "uv.ply")
    let textured =
        { scene with Id = "uv-endpoint"; Materials = [|material|]; Lights = [||]
                     Meshes = [|{ Id = "uv"; Path = "uv.ply"; Smooth = false; Closed = false }|]
                     Objects = [|{ Id = "uv"; Mesh = "uv"; Material = "texels"; Transform = SceneFiles.identity }|] }
    SceneFiles.save file textured
    let endpoints = [| -1e-12; 0.; 1e-12; 1. - 1e-12; Math.BitDecrement 1.; 1.; 1. + 1e-12 |]
    let coordinates = [| for u in endpoints do for v in endpoints do yield u, v |]
    let format (value: float) = value.ToString("R", Globalization.CultureInfo.InvariantCulture)
    // One mesh keeps every probe in the same frozen acceleration/cache-call sequence.
    let rows = ResizeArray<string>()
    rows.Add "ply"
    rows.Add "format ascii 1.0"
    rows.Add $"element vertex {coordinates.Length * 3}"
    for name in [ "x"; "y"; "z"; "u"; "v" ] do rows.Add $"property float {name}"
    rows.Add $"element face {coordinates.Length}"
    rows.Add "property list uchar int vertex_indices"
    rows.Add "end_header"
    for index = 0 to coordinates.Length - 1 do
        let u, v = coordinates.[index]
        let x = index * 3
        rows.Add $"{x} 0 0 {format u} {format v}"
        rows.Add $"{x + 1} 0 0 {format u} {format v}"
        rows.Add $"{x} 1 0 {format u} {format v}"
    for index = 0 to coordinates.Length - 1 do
        let first = index * 3
        rows.Add $"3 {first} {first + 1} {first + 2}"
    File.WriteAllLines(meshFile, rows)
    let fixture =
        SceneBuilder.build file "authored" settings
            (fun _ ->
                { Width = 2; Height = 2
                  Pixels = [|255uy;0uy;0uy; 0uy;255uy;0uy; 0uy;0uy;255uy; 255uy;255uy;0uy|] })
    for index = 0 to coordinates.Length - 1 do
        let u, v = coordinates.[index]
        let label = $"UV ({format u},{format v})"
        let hit = fixture.Scene.Shapes.Head.hitFunction(Ray(Point(float (index * 3) + 0.2,0.2,1.), Vector(0.,0.,-1.)))
        check (label + ": endpoint fixture intersects") hit.DidHit
        let colour = hit.Material.AmbientColour(hit, API.mkAmbientLight (API.mkColour 1. 1. 1.) 1.)
#if LEGACY
        // Preserve the frozen triangle's UV swap; only the expected address changes.
        let addressedU, addressedV = v, u
#else
        let addressedU, addressedV = u, v
#endif
        let expected =
            match addressedU < 0.5, addressedV < 0.5 with
            | true, false -> 1., 0., 0.
            | false, false -> 0., 1., 0.
            | true, true -> 0., 0., 1.
            | false, true -> 1., 1., 0.
        check (label + ": clamped corner and epsilon choose the correct texel")
            ((colour.R, colour.G, colour.B) = expected)
finally
    Directory.Delete(directory, true)
printfn "All %d shared-builder assertions passed." assertions
