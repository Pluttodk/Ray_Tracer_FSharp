#I "bin/Release/net10.0"
#r "FParsecCS.dll"
#r "FParsec.dll"
#r "StbImageSharp.dll"
#r "StbImageWriteSharp.dll"
#r "RayTracer.ImageIO.dll"
#r "LegacyRayTracer.dll"

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open Tracer.API
open Tracer.Basics
open Tracer.Basics.Render
open Tracer.Basics.Transformation
open Tracer.Imaging

let require condition message = if not condition then failwith message
let hashFile path = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes path)).ToLowerInvariant()
let output =
    match fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--") with
    | [||] -> Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../../artifacts/foundation-validation/legacy-proof"))
    | [| path |] -> Path.GetFullPath path
    | _ -> invalidArg "arguments" "Usage: dotnet fsi benchmarks/legacy/proof.fsx -- [OUTPUT_DIRECTORY]"

Directory.CreateDirectory output |> ignore
let colour = mkColour 0.65 0.22 0.08
let material = mkMatteMaterial colour 0.25 colour 0.75
let texture = mkMatTexture material
let sphere = mkSphere (mkPoint 0. 0.25 0.) 0.7 texture
let light = mkLight (mkPoint -2. 3. 4.) (mkColour 1. 1. 1.) 1.
let scene = mkScene [sphere] [light] (mkAmbientLight (mkColour 1. 1. 1.) 0.2) 0
let camera = mkPinholeCamera (mkPoint 0. 0. 3.) (mkPoint 0. 0. 0.) (mkVector 0. 1. 0.) 1. 2. 2. 32 32 (mkRegularSampler 1)

let analytic = SphereShape(Point.Zero, 1., texture)
let hit = analytic.hitFunction (Ray(Point(0., 0., 3.), Vector(0., 0., -1.)))
require (hit.DidHit && abs (hit.Time - 2.) < 1e-12) "Frozen sphere hit probe failed."
require (Vector.DivideByInt(Vector(2., 4., 6.), 2).GetCoord = (1., 2., 3.)) "Compiler-only vector annotation changed division."
require (Colour(0.25, 1., 0.).ToColor.R = 127uy) "Frozen gamma-2 output conversion changed."

let forward: QuickMatrix =
    { Pos1x1 = 2.; Pos1x2 = 1.; Pos1x3 = 0.5; Pos1x4 = 3.
      Pos2x1 = 0.; Pos2x2 = 4.; Pos2x3 = 1.; Pos2x4 = -2.
      Pos3x1 = 0.; Pos3x2 = 0.; Pos3x3 = 0.5; Pos3x4 = 1.
      Pos4x1 = 0.; Pos4x2 = 0.; Pos4x3 = 0.; Pos4x4 = 1. }
let inverse: QuickMatrix =
    { Pos1x1 = 0.5; Pos1x2 = -0.125; Pos1x3 = -0.25; Pos1x4 = -1.5
      Pos2x1 = 0.; Pos2x2 = 0.25; Pos2x3 = -0.5; Pos2x4 = 1.
      Pos3x1 = 0.; Pos3x2 = 0.; Pos3x3 = 2.; Pos3x4 = -2.
      Pos4x1 = 0.; Pos4x2 = 0.; Pos4x3 = 0.; Pos4x4 = 1. }
let affine = mkTransformation(forward, inverse)
require (getMatrix affine = forward && getInvMatrix affine = inverse) "Affine input constructor changed the supplied matrix pair."
let transformed = transformPoint(Point(1., 2., 3.), getMatrix affine)
require (transformed.GetCoord = (8.5, 9., 2.5)) "Typed affine16 forward transform failed."
require ((transformPoint(transformed, getInvMatrix affine)).GetCoord = (1., 2., 3.))
    "Typed shared inverse did not round-trip the affine16 point."

let renderer = Render(scene, camera)
let film = renderer.RenderCaptured
let image, linear = film.Image, film.LinearRgb
try
    require (film.Width = 32 && film.Height = 32 && linear.Length = 32 * 32 * 3) "Legacy linear capture has the wrong dimensions."
    require (linear |> Array.forall Double.IsFinite)
        "Legacy proof produced non-finite pixels."
    require (linear |> Array.exists (fun value -> value > 0.)) "Legacy proof did not render its sphere."
    require ([film.BuildMs; film.TraceMs; film.OutputPreparationMs] |> List.forall (fun value -> Double.IsFinite value && value >= 0.))
        "Legacy capture returned invalid phase durations."
    require (film.PhaseNotes.Length > 0) "Legacy capture did not disclose its coarse timing boundaries."
    for i in 0 .. 32 * 32 - 1 do
        let colour = Colour(linear.[i * 3], linear.[i * 3 + 1], linear.[i * 3 + 2])
        require (image.GetPixel(i % 32, i / 32).ToArgb() = colour.ToColor.ToArgb())
            "Legacy PNG orientation or gamma differs from captured original Colour."
    let imagePath = Path.Combine(output, "legacy-port.png")
    image.SavePng imagePath
    use decoded = RgbImage.Load imagePath
    require (decoded.Pixels = image.Pixels) "Legacy PNG round-trip did not preserve the image."

    let linearPath = Path.Combine(output, "legacy-port.rgb64")
    do
        use stream = File.Create linearPath
        use writer = new BinaryWriter(stream)
        for value in linear do writer.Write value
    let details =
        {| name = "legacy-port"
           purpose = "Tiny portability proof, not a shared benchmark scene or performance measurement."
           sourceCommit = "066c59eba68d222d63b373b7e25d408796eb1d12"
           portabilityManifestSha256 = hashFile (Path.Combine(__SOURCE_DIRECTORY__, "portability.json"))
           runtime = System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription
           width = 32
           height = 32
           samplesPerPixel = 1
           imageTransfer = "Original Colour.ToColor: sqrt channel * 255, truncation and clamp"
           linearFormat = "Top-left row-major RGB float64 little endian, before original Colour.ToColor"
           buildMs = film.BuildMs
           traceMs = film.TraceMs
           outputPreparationMs = film.OutputPreparationMs
           phaseNotes = film.PhaseNotes
           affine16VisibilityProbe = "Typed original QuickMatrix record and mkTransformation preserve the supplied forward/inverse pair; no reflection."
           imageSha256 = hashFile imagePath
           linearSha256 = hashFile linearPath
           analyticSphereHitTime = hit.Time |}
    File.WriteAllText(Path.Combine(output, "proof.json"), JsonSerializer.Serialize(details, JsonSerializerOptions(WriteIndented = true)))
    printfn "legacy-port proof passed: 32x32 PNG, original linear pixels, sphere hit t=%.1f; %s" hit.Time output
finally
    renderer.Clean image

require (List.isEmpty Tracer.Basics.Acceleration.listOfAccel) "Original global cleanup policy did not run."

let compatibilityRenderer = Render(scene, camera)
let compatibilityImage, compatibilityColours = compatibilityRenderer.RenderWithLinear
try
    use expectedImage = RgbImage.Load(Path.Combine(output, "legacy-port.png"))
    require (compatibilityImage.Pixels = expectedImage.Pixels) "RenderWithLinear compatibility image changed."
    for i = 0 to compatibilityColours.Length - 1 do
        let colour = compatibilityColours.[i]
        require ((colour.R, colour.G, colour.B) = (linear.[i * 3], linear.[i * 3 + 1], linear.[i * 3 + 2]))
            "Coarse timing/output packing changed original averaged colors."
finally
    compatibilityRenderer.Clean compatibilityImage
printfn "Legacy RenderCaptured timing/layout and RenderWithLinear compatibility passed."
