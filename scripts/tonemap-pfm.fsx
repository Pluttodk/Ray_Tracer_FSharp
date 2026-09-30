// Re-encodes a retained linear PFM to PNG at a chosen exposure and transfer.
//
// Renders keep unclamped linear radiance alongside the PNG, so exposure and
// tone mapping can be revisited without re-tracing - a 63-minute render does
// not need repeating to try a different exposure. Uses the renderer's own
// Colour.ToDisplayColor, so the result is identical to rendering with those
// settings.
//
// Usage: dotnet fsi scripts/tonemap-pfm.fsx -- <in.pfm> <out.png> <exposure> [transfer]

#r "../RayTracer/bin/Release/net10.0/Basics.dll"
#r "../RayTracer.ImageIO/bin/Release/net10.0/RayTracer.ImageIO.dll"
// RgbImage.SavePng needs its native encoder alongside it; fsi does not
// resolve a project's transitive package references on its own.
#r "../BenchmarkRunner/bin/Release/net10.0/StbImageWriteSharp.dll"
#r "../BenchmarkRunner/bin/Release/net10.0/StbImageSharp.dll"

open System
open System.IO
open Tracer.Basics
open Tracer.Imaging

let args = fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--")
if args.Length < 3 then
    eprintfn "Usage: dotnet fsi scripts/tonemap-pfm.fsx -- <in.pfm> <out.png> <exposure> [transfer]"
    exit 2

let source = args.[0]
let destination = args.[1]
let exposure = Double.Parse(args.[2], Globalization.CultureInfo.InvariantCulture)
let transfer = if args.Length > 3 then args.[3] else "aces"
if not (Double.IsFinite exposure) || exposure <= 0. then failwith "Exposure must be finite and positive."

/// Standard RGB PFM: "PF", dimensions, negative scale for little-endian,
/// float32 triples, bottom row first.
let readPfm (path: string) =
    use stream = File.OpenRead path
    let readLine () =
        let builder = Text.StringBuilder()
        let mutable next = stream.ReadByte()
        while next <> int '\n' && next <> -1 do
            builder.Append(char next) |> ignore
            next <- stream.ReadByte()
        builder.ToString().Trim()
    if readLine () <> "PF" then failwith "Not a colour PFM."
    let parts = (readLine ()).Split(' ')
    let width, height = int parts.[0], int parts.[1]
    let scale = Double.Parse(readLine (), Globalization.CultureInfo.InvariantCulture)
    if scale >= 0. then failwith "Only little-endian PFM is supported."
    let bytes = Array.zeroCreate<byte> (width * height * 3 * 4)
    let mutable read = 0
    while read < bytes.Length do
        let got = stream.Read(bytes, read, bytes.Length - read)
        if got <= 0 then failwith "Truncated PFM."
        read <- read + got
    let values = Array.zeroCreate<float32> (width * height * 3)
    Buffer.BlockCopy(bytes, 0, values, 0, bytes.Length)
    width, height, values

let width, height, values = readPfm source
use image = new RgbImage(width, height)
let pixels = image.Pixels
for y in 0 .. height - 1 do
    // PFM stores the bottom row first; RgbImage is top-down.
    let sourceRow = height - 1 - y
    for x in 0 .. width - 1 do
        let s = (sourceRow * width + x) * 3
        let d = (y * width + x) * 3
        let clamp (v: float32) = let f = float v in if Double.IsFinite f && f > 0. then f else 0.
        let colour =
            Colour(clamp values.[s] * exposure, clamp values.[s + 1] * exposure, clamp values.[s + 2] * exposure)
                .ToDisplayColor transfer
        pixels.[d] <- colour.R
        pixels.[d + 1] <- colour.G
        pixels.[d + 2] <- colour.B
image.SavePng destination
printfn "wrote %s  (%dx%d, exposure %.2f, transfer %s)" destination width height exposure transfer
