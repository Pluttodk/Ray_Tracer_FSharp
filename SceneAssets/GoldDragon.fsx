#load "../SceneFormat/SceneFormat.fs"
#load "Geometry.fs"
#load "Meshes.fs"

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open Tracer.SceneAssets
open Tracer.SceneFormat

CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let relativeMesh = "artifacts/scene-assets/stanford/dragon_recon/dragon_vrip.ply"
let meshPath = Path.Combine(root, relativeMesh)
let meshHash = "fea87ff48f2aba22fb53e7b67c3ff3f7b8c2a3b3a0653af62c48bba67c6d5744"
let floorRelative = "artifacts/scene-assets/gold-dragon/staging-plane.ply"
let floorPath = Path.Combine(root, floorRelative)
let floorProvenanceRelative = "artifacts/scene-assets/gold-dragon/staging-plane.provenance.json"
let scenePath = Path.Combine(root, "benchmarks/scenes/gold-dragon.json")
let settingsPath = Path.Combine(root, "artifacts/scene-assets/gold-dragon/original-settings.json")

let require condition message = if not condition then raise (InvalidDataException message)

require (File.Exists meshPath)
    "The optional Stanford scan is missing. Run scripts/prepare-gold-dragon.fsx first; this scene generator never downloads data."
require (SceneFiles.hashFile meshPath = meshHash) "The Stanford Dragon differs from the pinned, unmodified source scan."

let header =
    use reader = File.OpenText meshPath
    let lines = ResizeArray<string>()
    let mutable complete = false
    while not complete do
        let line = reader.ReadLine()
        require (not (isNull line) && lines.Count < 128) "The Stanford PLY header is incomplete or unexpectedly large."
        lines.Add line
        complete <- line = "end_header"
    lines.ToArray()
require (header.[0] = "ply" && header.[1] = "format ascii 1.0") "Expected the original ASCII Stanford PLY."
require (Array.contains "element vertex 437645" header && Array.contains "element face 871414" header)
    "Unexpected Stanford Dragon vertex/triangle counts."

// The scan is only read. This separate original slab approximates the historical infinite plane.
let floorMesh =
    Meshes.extrusion "gold-dragon-staging-plane" 0.02
        [| -1., -1.; 1., -1.; 1., 1.; -1., 1. |]
Directory.CreateDirectory(Path.GetDirectoryName floorPath) |> ignore
File.WriteAllText(floorPath, Meshes.plyTextWithProvenance floorProvenanceRelative floorMesh, UTF8Encoding(false))
let floorTopology = Meshes.readPly floorMesh.Id floorMesh.Recipe floorMesh.Smooth floorPath |> Meshes.validate

let white = [| 1.; 1.; 1. |]
let blue = [| 0.; 0.; 1. |]
let lemon = [| 1.; 1.; 0.3 |]
let gold: MaterialSpec =
    { Id = "original-glossy-gold"; Kind = MaterialKind.Glossy; Colour = lemon
      AmbientColour = [| 1.; 0.75; 0.5 |]; SpecularColour = null; ReflectionColour = null
      Ambient = 0.1; Diffuse = 0.9; Specular = 0.4; Exponent = 10
      Reflectivity = 0.3; GlossExponent = 10; Ior = 1.5; Filter = white
      Emission = 0.; Texture = "" }
let floorMaterial =
    { gold with Id = "original-blue-reflector"; Kind = MaterialKind.Mirror; Colour = blue
                AmbientColour = null; ReflectionColour = blue
                Ambient = 1.; Diffuse = 1.; Specular = 0.; Exponent = 0
                Reflectivity = 0.5; GlossExponent = 0 }
let light id position colour =
    { Id = id; Kind = LightKind.Point; Position = position; Direction = [| 0.; 0.; 0. |]
      Colour = colour; Intensity = 0.5; Size = [| 1.; 1. |] }
let environment =
    { Id = "original-lemon-environment"; Kind = LightKind.Environment; Position = [| 0.; 0.; 0. |]
      Direction = [| 0.; 1.; 0. |]; Colour = lemon; Intensity = 0.85
      Size = [| 1000000.; 1000000. |] }
let dragonTransform = Matrix.trs (0., -2.3, 0.) (40., 40., 40.) (0., 45., 0.)
let halfExtent = 100000.
let floorTransform =
    [| halfExtent; 0.; 0.; 0.
       0.; 0.; 1.; -0.01
       0.; -halfExtent; 0.; 0.
       0.; 0.; 0.; 1. |]
let scene: SceneSpec =
    { SchemaVersion = 1; Id = "gold-dragon"; Title = "Stanford Gold Dragon - optional historical bonus"
      Description =
        "Extra authored/gold-only adaptation of TracerTest/Meshes.fs:renderGoldDragon. Original unmodified Stanford Dragon reconstruction, 437645 vertices/871414 triangles, flat shaded and open; never a glass subject. Credit: Stanford University Computer Graphics Laboratory, https://graphics.stanford.edu/data/3Dscanrep/. Attributed noncommercial research/free redistribution only; commercial use needs permission. The scan and derived renders are optional ignored research artifacts, not GPL-relicensed. Original gold colours/coefficients, transform, camera, two point lights, environment and ambient are retained. The blue floor here is an original finite 200000-by-200000 slab with its top at y=0, not the truly infinite plane; the separately restored original function preserves that exact primitive. Render settings: 1024x768, sixteen multi-jittered camera/light/glossy samples, 83 sample sets, two bounces; see benchmarks/gold-dragon.json."
      Camera =
        { Position = [| 2.; 6.; 12. |]; Target = [| 0.; 1.5; 0. |]; Up = [| 0.; 1.; 0. |]
          ViewDistance = 4.; ViewWidth = 4.; ViewHeight = 3.; LensRadius = 0.
          FocusDistance = V3.length (V3.create 2. 4.5 12.) }
      AmbientColour = blue; AmbientIntensity = 0.1; MaxBounces = 2
      Materials = [| gold; floorMaterial |]
      Meshes =
        [| { Id = "original-stanford-dragon"; Path = "../../" + relativeMesh; Smooth = false; Closed = false }
           { Id = floorMesh.Id; Path = "../../" + floorRelative; Smooth = false; Closed = true } |]
      Objects =
        [| { Id = "dragon"; Mesh = "original-stanford-dragon"; Material = gold.Id; Transform = dragonTransform }
           { Id = "blue-reflective-floor"; Mesh = floorMesh.Id; Material = floorMaterial.Id; Transform = floorTransform } |]
      Lights =
        [| light "original-white-point" [| 6.; 2.; 6. |] white
           light "original-red-point" [| -6.; 2.; 6. |] [| 1.; 0.; 0. |]
           environment |]
      Subjects = [| "dragon" |] }

Directory.CreateDirectory(Path.GetDirectoryName scenePath) |> ignore
SceneFiles.save scenePath scene
let settings =
    { SceneFiles.preset "quick" with
        Width = 1024; Height = 768; CameraSamples = 16; LightSamples = 16
        GlossySamples = 16; MaxBounces = 2; Sampler = "multi-jittered" }
    |> SceneFiles.validateSettings
File.WriteAllText(settingsPath, JsonSerializer.Serialize(settings, SceneFiles.jsonOptions) + "\n", UTF8Encoding(false))
let floorProvenance =
    {| id = floorMesh.Id
       path = floorRelative
       sha256 = SceneFiles.hashFile floorPath
       license = "GPL-2.0-only"
       authors = "Ray_Tracer_FSharp contributors (procedural design with Copilot assistance)"
       origin = "Original capped extrusion; not derived from the Stanford scan"
       recipe = floorMesh.Recipe
       topology = floorTopology
       worldTransform = floorTransform
       restriction = "This license applies only to the original floor mesh, NOT the Stanford Dragon dataset or its rendered images."
       sources =
           [| "SceneAssets/GoldDragon.fsx"; "SceneAssets/Geometry.fs"; "SceneAssets/Meshes.fs"; "SceneFormat/SceneFormat.fs" |]
           |> Array.map (fun path -> {| path = path; sha256 = SceneFiles.hashFile(Path.Combine(root, path)) |}) |}
File.WriteAllText(Path.Combine(root, floorProvenanceRelative), JsonSerializer.Serialize(floorProvenance, SceneFiles.jsonOptions) + "\n", UTF8Encoding(false))
let saved = SceneFiles.load scenePath
for mesh in saved.Meshes do
    require (File.Exists(SceneFiles.resolveAsset scenePath mesh.Path)) $"Missing bonus scene mesh: {mesh.Path}."
require (SceneFiles.specularColour gold = white && SceneFiles.reflectionColour gold = white)
    "The historical gold specular/reflection colours must be white."
let glassRejected =
    try SceneFiles.applyMaterialVariant "glass" saved |> ignore; false
    with :? InvalidDataException -> true
require glassRejected "The open Stanford scan must not be accepted as solid glass."
require (SceneFiles.hashFile meshPath = meshHash) "The raw Stanford scan changed during scene preparation."

printfn "Optional gold-only scene ready: %s" scenePath
printfn "Raw Stanford scan unchanged: %s" meshHash
printfn "Finite original floor mesh: %s (GPL-2.0-only; the Stanford scan is NOT GPL)" floorRelative
printfn "Historical 1024x768 / 16-sample multi-jittered / two-bounce settings: %s" settingsPath
