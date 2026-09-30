#r "System.Formats.Tar"
#load "../SceneFormat/SceneFormat.fs"

open System
open System.Formats.Tar
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.Json
open Tracer.SceneFormat

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let archivePath = Path.Combine(root, "artifacts/downloads/stanford-dragon-recon.tar.gz")
let meshPath = Path.Combine(root, "artifacts/scene-assets/stanford/dragon_recon/dragon_vrip.ply")
let archiveHash = "74ac1d90989c9b1732edee82d57e9ce71452144cf4355f108d8c9c616d28d02f"
let meshHash = "fea87ff48f2aba22fb53e7b67c3ff3f7b8c2a3b3a0653af62c48bba67c6d5744"
let source = "https://graphics.stanford.edu/pub/3Dscanrep/dragon/dragon_recon.tar.gz"
let terms = "https://graphics.stanford.edu/data/3Dscanrep/"

let checkHash path expected =
    if SceneFiles.hashFile path <> expected then
        invalidOp $"Checksum mismatch: {path}. Refusing to use a changed or incomplete asset."

let atomicWrite (path: string) (write: Stream -> unit) =
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    let temporary = path + "." + Guid.NewGuid().ToString("N") + ".tmp"
    try
        do
            use output = new FileStream(temporary, FileMode.CreateNew, FileAccess.Write, FileShare.None)
            write output
        File.Move(temporary, path, false)
    finally
        if File.Exists temporary then File.Delete temporary

if not (File.Exists meshPath) then
    if not (File.Exists archivePath) then
        printfn "Downloading the Stanford Dragon for attributed noncommercial rendering research."
        use client = new HttpClient(Timeout = TimeSpan.FromMinutes 5.)
        use response = client.GetAsync(source, HttpCompletionOption.ResponseHeadersRead).GetAwaiter().GetResult()
        response.EnsureSuccessStatusCode() |> ignore
        use input = response.Content.ReadAsStream()
        atomicWrite archivePath (fun output -> input.CopyTo output)
    checkHash archivePath archiveHash
    use archive = File.OpenRead archivePath
    use decompressed = new GZipStream(archive, CompressionMode.Decompress)
    use reader = new TarReader(decompressed)
    let mutable found = false
    let mutable finished = false
    while not finished do
        let entry = reader.GetNextEntry()
        if isNull entry then finished <- true
        elif entry.Name = "dragon_recon/dragon_vrip.ply" then
            if entry.EntryType <> TarEntryType.RegularFile && entry.EntryType <> TarEntryType.V7RegularFile then
                invalidOp "Expected a regular PLY file in the pinned Stanford archive."
            if entry.Length <= 0L || entry.Length > 128L * 1024L * 1024L then
                invalidOp "Unexpected Stanford Dragon mesh size."
            if found then invalidOp "Duplicate mesh entry in the Stanford archive."
            atomicWrite meshPath (fun output -> entry.DataStream.CopyTo output)
            found <- true
    if not found then invalidOp "The Stanford archive did not contain dragon_recon/dragon_vrip.ply."

checkHash meshPath meshHash
let alias = Path.Combine(root, "ply/dragon.ply")
if File.Exists alias then checkHash alias meshHash
else
    Directory.CreateDirectory(Path.GetDirectoryName alias) |> ignore
    File.Copy(meshPath, alias, false)

let provenance =
    {| asset = "Stanford Dragon, original supplied dragon_vrip.ply reconstruction"
       credit = "Stanford University Computer Graphics Laboratory"
       sourceUrl = source
       termsUrl = terms
       archiveSha256 = archiveHash
       meshSha256 = meshHash
       vertices = 437645
       triangles = 871414
       closed = false
       modifications = "None. Byte-identical source PLY; transforms are applied only by the renderer."
       permittedUse = "Attributed noncommercial research; free redistribution under Stanford's stated terms."
       restriction = "Commercial use requires Stanford's permission. This asset is not licensed under the renderer's GPL."
       renderUse = "Static gold-material scene only; no deformation, destructive simulation, or Boolean operations." |}
File.WriteAllText(Path.Combine(Path.GetDirectoryName meshPath, "provenance.json"),
                  JsonSerializer.Serialize(provenance, SceneFiles.jsonOptions))
printfn "Gold Dragon ready: %s" alias
printfn "437645 vertices, 871414 triangles; SHA256 %s" meshHash
printfn "Credit: Stanford University Computer Graphics Laboratory. Terms: %s" terms
