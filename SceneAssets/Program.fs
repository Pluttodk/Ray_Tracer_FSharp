namespace Tracer.SceneAssets

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open Tracer.SceneFormat

[<CLIMutable>]
type AssetEntry =
    { Id: string
      Path: string
      Sha256: string
      License: string
      Authors: string
      Origin: string
      SourceUrl: string
      Recipe: string
      Smooth: bool
      Topology: Topology }

[<CLIMutable>]
type FileEntry =
    { Path: string
      Sha256: string }

[<CLIMutable>]
type TextureEntry =
    { Id: string
      Path: string
      Sha256: string
      Width: int
      Height: int
      License: string
      Authors: string
      Origin: string
      SourceUrl: string
      Recipe: string
      Transfer: string }

[<CLIMutable>]
type AssetManifest =
    { SchemaVersion: int
      Generator: string
      Seed: int
      License: string
      Coordinates: string
      TextureConvention: string
      TimingBoundary: string
      Sources: FileEntry array
      Meshes: AssetEntry array
      Textures: TextureEntry array
      Scenes: FileEntry array }

module Program =
    let private seed = 2026
    let private utf8 = UTF8Encoding(false)
    let private meshDirectory = "artifacts/scene-assets/meshes"
    let private textureDirectory = "artifacts/scene-assets/textures"
    let private sceneDirectory = "benchmarks/scenes"
    let private manifestPath = $"{sceneDirectory}/provenance.json"
    let private sourcePaths =
        [| "SceneAssets/SceneAssets.fsproj"; "SceneAssets/packages.lock.json"; "SceneAssets/Geometry.fs"; "SceneAssets/Meshes.fs"
           "SceneAssets/Textures.fs"; "SceneAssets/Scenes.fs"; "SceneAssets/Checks.fs"; "SceneAssets/Program.fs"; "SceneFormat/SceneFormat.fs"
           "RayTracer.ImageIO/RayTracer.ImageIO.fsproj"; "RayTracer.ImageIO/RgbImage.fs"; "RayTracer.ImageIO/packages.lock.json"; "global.json"; "LICENSE" |]

    let private fileEntry path = { Path = path; Sha256 = SceneFiles.hashFile path }

    let private textDiffers path text = not (File.Exists path) || File.ReadAllText path <> text

    let private writeText path text =
        if textDiffers path text then File.WriteAllText(path, text, utf8)

    let private writeBytes path bytes =
        if not (File.Exists path) || File.ReadAllBytes path <> bytes then File.WriteAllBytes(path, bytes)

    let private validateScenes (meshes: Mesh array) scenes =
        let topology =
            meshes |> Array.map (fun mesh -> mesh.Id, Meshes.validate mesh) |> Map.ofArray
        for scene in scenes do
            SceneFiles.validate scene |> ignore
            for variant in SceneFiles.materialVariants do
                SceneFiles.applyMaterialVariant variant scene |> SceneFiles.validate |> ignore
            for mesh in scene.Meshes do
                if mesh.Closed && not topology.[mesh.Id].Closed then
                    failwith $"{scene.Id}: {mesh.Id} is declared closed but fails the solid topology check."
            let subjectIds = Set.ofArray scene.Subjects
            for instance in scene.Objects do
                if subjectIds.Contains instance.Id && not topology.[instance.Mesh].Closed then
                    failwith $"{scene.Id}: glass subject {instance.Id} is not closed."
        let required = Set.ofList [ "chair"; "roman-bust"; "space-sentinel"; "sky-arena" ]
        let actual = scenes |> Array.map (fun scene -> scene.Id) |> Set.ofArray
        if not (Set.isSubset required actual) then failwith "The four required benchmark scene IDs are missing."

    let private generate () =
        Directory.CreateDirectory meshDirectory |> ignore
        Directory.CreateDirectory textureDirectory |> ignore
        Directory.CreateDirectory sceneDirectory |> ignore
        let meshes = Meshes.library()
        let scenes = Scenes.all meshes
        validateScenes meshes scenes
        let entries =
            meshes |> Array.map (fun mesh ->
                let path = $"{meshDirectory}/{mesh.Id}.ply"
                writeText path (Meshes.plyText mesh)
                // Validate the rounded, serialized coordinates, not only the in-memory construction.
                let topology = Meshes.readPly mesh.Id mesh.Recipe mesh.Smooth path |> Meshes.validate
                { Id = mesh.Id; Path = path; Sha256 = SceneFiles.hashFile path
                  License = "GPL-2.0-only"; Authors = "Ray_Tracer_FSharp contributors (procedural design with Copilot assistance)"
                  Origin = "Original geometry authored in this repository; generated locally; no external models or scans"
                  SourceUrl = ""; Recipe = mesh.Recipe; Smooth = mesh.Smooth; Topology = topology })
        let textures =
            Textures.library() |> Array.map (fun texture ->
                let path = $"{textureDirectory}/{texture.Id}.png"
                writeBytes path (Textures.pngBytes texture)
                Textures.validateFile texture path
                { Id = texture.Id; Path = path; Sha256 = SceneFiles.hashFile path; Width = texture.Width; Height = texture.Height
                  License = "GPL-2.0-only"; Authors = "Ray_Tracer_FSharp contributors (procedural design with Copilot assistance)"
                  Origin = "Original deterministic F# texture recipe; no images, scans, fonts or external texture samples"
                  SourceUrl = ""; Recipe = texture.Recipe
                  Transfer = "RGB8 gamma-2-encoded linear modulation mask; diffuse/ambient material colour multiplies decoded texel; top-down PNG rows" })
        let sceneFiles =
            scenes |> Array.map (fun scene ->
                let path = $"{sceneDirectory}/{scene.Id}.json"
                if textDiffers path (JsonSerializer.Serialize(scene, SceneFiles.jsonOptions)) then
                    SceneFiles.save path scene
                fileEntry path)
        let manifest =
            { SchemaVersion = 1; Generator = "SceneAssets/1"; Seed = seed; License = "GPL-2.0-only"
              Coordinates = "Right-handed; +Y up; subjects face +Z; scene units are arbitrary consistent design units; transforms are row-major affine."
              TextureConvention = "PLY vertices store x y z nx ny nz u v, with u/v in [0,1]; indexed seams are shared to preserve closed topology. Reject non-finite UVs, clamp each component to [0,1], then nearest sample x=min(width-1,int(u*width)), y=min(height-1,int((1-v)*height)); v=0 is bottom and v=1 is top. PNG masks are gamma-2 encoded; decoded texels multiply authored base and ambient colours in every engine, never specular/reflection colours."
              TimingBoundary = "Run this generator and verify all hashes before benchmarking. Asset generation and validation are excluded from render timing."
              Sources = sourcePaths |> Array.map fileEntry
              Meshes = entries; Textures = textures; Scenes = sceneFiles }
        writeText manifestPath (JsonSerializer.Serialize(manifest, SceneFiles.jsonOptions) + "\n")
        printfn "Generated %d original closed meshes, %d textures and %d scenes (seed %d)." entries.Length textures.Length scenes.Length seed
        for scene in scenes do
            let perMesh = entries |> Array.map (fun mesh -> mesh.Id, mesh.Topology.Triangles) |> Map.ofArray
            let triangles = scene.Objects |> Array.sumBy (fun instance -> perMesh.[instance.Mesh])
            printfn "  %-15s %3d instances, %3d subjects, %7d instanced triangles" scene.Id scene.Objects.Length scene.Subjects.Length triangles
        printfn "Provenance: %s" manifestPath

    let private verify () =
        let manifest = JsonSerializer.Deserialize<AssetManifest>(File.ReadAllText manifestPath, SceneFiles.jsonOptions)
        if manifest.SchemaVersion <> 1 || manifest.Seed <> seed || manifest.Generator <> "SceneAssets/1" || manifest.License <> "GPL-2.0-only" then
            failwith "Unexpected scene asset manifest version, seed, generator or license."
        if (manifest.Sources |> Array.map _.Path) <> sourcePaths then failwith "Manifest source inventory differs from this generator."
        for source in manifest.Sources do
            if SceneFiles.hashFile source.Path <> source.Sha256 then failwith $"Generator/source digest changed: {source.Path}. Regenerate before benchmarking."
        let regenerated = Meshes.library()
        let regeneratedTextures = Textures.library()
        let regeneratedScenes = Scenes.all regenerated
        if (manifest.Meshes |> Array.map _.Id) <> (regenerated |> Array.map _.Id)
           || (manifest.Textures |> Array.map _.Id) <> (regeneratedTextures |> Array.map _.Id)
           || (manifest.Scenes |> Array.map _.Path) <> (regeneratedScenes |> Array.map (fun scene -> $"{sceneDirectory}/{scene.Id}.json")) then
            failwith "Manifest asset inventory differs from this generator."
        let meshes =
            manifest.Meshes |> Array.map (fun entry ->
                if entry.Path <> $"{meshDirectory}/{entry.Id}.ply" || entry.License <> "GPL-2.0-only" || entry.SourceUrl <> "" then
                    failwith $"Unexpected mesh path or provenance: {entry.Id}."
                if SceneFiles.hashFile entry.Path <> entry.Sha256 then failwith $"Asset hash mismatch: {entry.Path}."
                let mesh = Meshes.readPly entry.Id entry.Recipe entry.Smooth entry.Path
                let topology = Meshes.validate mesh
                if topology <> entry.Topology then failwith $"Topology metadata mismatch: {entry.Path}."
                mesh)
        let textures = regeneratedTextures |> Array.map (fun texture -> texture.Id, texture) |> Map.ofArray
        for entry in manifest.Textures do
            if entry.Path <> $"{textureDirectory}/{entry.Id}.png" || entry.License <> "GPL-2.0-only" || entry.SourceUrl <> "" then
                failwith $"Unexpected texture path or provenance: {entry.Id}."
            if SceneFiles.hashFile entry.Path <> entry.Sha256 then failwith $"Texture hash mismatch: {entry.Path}."
            let texture = textures.[entry.Id]
            if texture.Width <> entry.Width || texture.Height <> entry.Height then failwith $"Texture dimensions mismatch: {entry.Path}."
            Textures.validateFile texture entry.Path
        let scenes =
            manifest.Scenes |> Array.map (fun entry ->
                if SceneFiles.hashFile entry.Path <> entry.Sha256 then failwith $"Scene hash mismatch: {entry.Path}."
                let scene = SceneFiles.load entry.Path
                for mesh in scene.Meshes do
                    let path = SceneFiles.resolveAsset entry.Path mesh.Path
                    if not (manifest.Meshes |> Array.exists (fun asset -> Path.GetFullPath asset.Path = path)) then
                        failwith $"Scene mesh is not an original manifested asset: {mesh.Path}."
                for material in scene.Materials do
                    if not (String.IsNullOrEmpty material.Texture) then
                        let path = SceneFiles.resolveAsset entry.Path material.Texture
                        if not (manifest.Textures |> Array.exists (fun asset -> Path.GetFullPath asset.Path = path)) then
                            failwith $"Scene texture is not an original manifested asset: {material.Texture}."
                scene)
        validateScenes meshes scenes
        Checks.run meshes scenes
        for mesh in regenerated do
            let path = $"{meshDirectory}/{mesh.Id}.ply"
            if File.ReadAllText path <> Meshes.plyText mesh then failwith $"Deterministic regeneration mismatch: {path}."
        for scene in regeneratedScenes do
            let path = $"{sceneDirectory}/{scene.Id}.json"
            if File.ReadAllText path <> JsonSerializer.Serialize(scene, SceneFiles.jsonOptions) then
                failwith $"Deterministic scene regeneration mismatch: {path}."
        printfn "Verified %d mesh hashes, closed oriented manifold topology, %d PNGs, %d scenes and all six material variants." meshes.Length textures.Count scenes.Length
        printfn "Regenerated scene, mesh and texture bytes exactly match the saved JSON/PLY/PNG files."

    [<EntryPoint>]
    let main args =
        CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
        CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
        try
            if not (File.Exists "SceneFormat/SceneFormat.fsproj") then
                failwith "Run SceneAssets from the repository root."
            match args with
            | [||] | [| "generate" |] -> generate(); verify(); 0
            | [| "verify" |] -> verify(); 0
            | [| "--help" |] | [| "-h" |] ->
                printfn "SceneAssets [generate|verify]\nRun from the repository root. Offline; fixed seed 2026; no downloaded assets."
                0
            | _ -> eprintfn "Usage: SceneAssets [generate|verify]"; 2
        with error ->
            eprintfn "Scene asset preparation failed: %s" error.Message
            1
