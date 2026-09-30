#r "../benchmarks/legacy/bin/Release/net10.0/FParsecCS.dll"
#r "../benchmarks/legacy/bin/Release/net10.0/FParsec.dll"
#r "../benchmarks/legacy/bin/Release/net10.0/LegacyRayTracer.dll"
#r "bin/Release/net10.0/SceneFormat.dll"
#r "bin/Release/net10.0/SceneAssets.dll"

open System
open System.IO
open System.Text.Json
open Tracer.SceneAssets
open Tracer.SceneFormat

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let manifest =
    JsonSerializer.Deserialize<AssetManifest>(
        File.ReadAllText(Path.Combine(root, "benchmarks/scenes/provenance.json")),
        SceneFiles.jsonOptions)

for entry in manifest.Meshes do
    let path = Path.Combine(root, entry.Path)
    let vertices, faces = Tracer.Basics.PLYParser.parsePLY path
    let expected = Meshes.readPly entry.Id entry.Recipe entry.Smooth path
    if vertices.Length <> entry.Topology.Vertices || faces.Length <> entry.Topology.Triangles then
        failwith $"Frozen reader count mismatch for {entry.Id}."
    for index in 0 .. vertices.Length - 1 do
        let vertex, reference = vertices.[index], expected.Vertices.[index]
        if not (Double.IsFinite vertex.x && Double.IsFinite vertex.y && Double.IsFinite vertex.z) then
            failwith $"Frozen reader produced a non-finite vertex for {entry.Id}."
        for property in [ vertex.nx; vertex.ny; vertex.nz; vertex.u; vertex.v ] do
            match property with
            | Some value when Double.IsFinite value -> ()
            | _ -> failwith $"Frozen reader lost a normal/UV property for {entry.Id}."
        let actualValues = [| vertex.x; vertex.y; vertex.z; vertex.nx.Value; vertex.ny.Value; vertex.nz.Value; vertex.u.Value; vertex.v.Value |]
        let expectedValues =
            [| reference.Position.X; reference.Position.Y; reference.Position.Z
               reference.Normal.X; reference.Normal.Y; reference.Normal.Z; reference.U; reference.V |]
        if not (Array.forall2 (fun actual expected -> abs (actual - expected) < 1e-12) actualValues expectedValues) then
            failwith $"Frozen reader changed a vertex/normal/UV value for {entry.Id}."
    for index in 0 .. faces.Length - 1 do
        match faces.[index], expected.Triangles.[index] with
        | [ 3; a; b; c ], (x, y, z) when a = x && b = y && c = z -> ()
        | _ -> failwith $"Frozen reader did not preserve a triangulated face for {entry.Id}."

printfn "Frozen legacy PLY reader accepted all %d generated meshes with normals, UVs and triangle counts intact." manifest.Meshes.Length
