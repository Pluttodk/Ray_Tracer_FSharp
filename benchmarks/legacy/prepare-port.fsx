open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Xml.Linq

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../.."))
let legacy = Path.Combine(root, "benchmarks/legacy")
let original = Path.Combine(legacy, "original/RayTracer")
let utf8 = UTF8Encoding(false)
let hash (bytes: byte[]) = Convert.ToHexString(SHA256.HashData bytes).ToLowerInvariant()
let fileHash path = File.ReadAllBytes path |> hash
let relative path = Path.GetRelativePath(root, path).Replace('\\', '/')

let checkOnly =
    match fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--") with
    | [||] -> false
    | [| "--check" |] -> true
    | _ -> invalidArg "arguments" "Usage: dotnet fsi benchmarks/legacy/prepare-port.fsx -- [--check]"

for line in File.ReadLines(Path.Combine(legacy, "source.sha256")) do
    if not (String.IsNullOrWhiteSpace line) then
        let expected = line.Substring(0, 64)
        let path = Path.Combine(root, line.Substring(66))
        if fileHash path <> expected then failwithf "Frozen source checksum mismatch: %s" (relative path)

let replace (expected: string) (replacement: string) (source: string) =
    if not (source.Contains(expected, StringComparison.Ordinal)) then
        failwithf "Expected portability source fragment was not found: %s" expected
    source.Replace(expected, replacement, StringComparison.Ordinal)

let capturedFilmType = """
type LegacyRenderFilm =
    { Width: int
      Height: int
      LinearRgb: float[]
      Image: RgbImage
      BuildMs: float
      TraceMs: float
      OutputPreparationMs: float
      PhaseNotes: string[] }

"""

let renderOutput = """
    member this.ShowImageOnScreen (_:RgbImage) : unit =
        raise (PlatformNotSupportedException("Window preview is unavailable in legacy-port. Use renderToFile to save a PNG."))

    member this.SaveImage (renderedImage:RgbImage, filepath:string) =
        renderedImage.SavePng(filepath)

    member private this.RenderPixels (captureLinear: bool) =
        let watch = Stopwatch.StartNew()
        let accel = this.PreProcessing
        let buildMs = watch.Elapsed.TotalMilliseconds

        watch.Restart()
        let renderedImage = new RgbImage(camera.ResX, camera.ResY)
        let pixel = renderedImage.Pixels
        let linear : Colour[] =
            if captureLinear then Array.zeroCreate (camera.ResX * camera.ResY)
            else Array.empty
        let bufferSetupMs = watch.Elapsed.TotalMilliseconds

        watch.Restart()
        Parallel.For(0, camera.ResY * camera.ResX, fun xy ->
            let y = xy / camera.ResX
            let coordsX = xy % camera.ResX
            let rays = camera.CreateRays coordsX y
            let cols = Array.map (fun ray -> (this.Cast accel ray)) rays
            let colour = (Array.fold (+) Colour.Black cols)/float cols.Length

            let color = colour.ToColor

            pixel.[xy * 3] <- color.R
            pixel.[xy * 3 + 1] <- color.G
            pixel.[xy * 3 + 2] <- color.B
            if captureLinear then
                linear.[(camera.ResY - 1 - y) * camera.ResX + coordsX] <- colour

            ) |> ignore
        let traceMs = watch.Elapsed.TotalMilliseconds

        watch.Restart()
        renderedImage.FlipVertical()
        let outputPreparationMs = bufferSetupMs + watch.Elapsed.TotalMilliseconds

        renderedImage, linear, buildMs, traceMs, outputPreparationMs

    member this.RenderParallel =
        let image, _, _, _, _ = this.RenderPixels false
        image

    member this.RenderWithLinear =
        let image, linear, _, _, _ = this.RenderPixels true
        image, linear

    member this.RenderCaptured : LegacyRenderFilm =
        let image, colours, buildMs, traceMs, outputPreparationMs = this.RenderPixels true
        let watch = Stopwatch.StartNew()
        let linear = Array.zeroCreate<float> (colours.Length * 3)
        for i = 0 to colours.Length - 1 do
            linear.[i * 3] <- colours.[i].R
            linear.[i * 3 + 1] <- colours.[i].G
            linear.[i * 3 + 2] <- colours.[i].B
        { Width = image.Width
          Height = image.Height
          LinearRgb = linear
          Image = image
          BuildMs = buildMs
          TraceMs = traceMs
          OutputPreparationMs = outputPreparationMs + watch.Elapsed.TotalMilliseconds
          PhaseNotes =
              [| "BuildMs measures the original single PreProcessing call. Renderer construction/bound partitioning is outside this capture and must be timed by the caller."
                 "TraceMs includes the original Parallel.For, camera samples, tracing, averaging, original per-pixel Colour.ToColor/RGB8 writes, and optional raw Colour capture; it is not a pure ray-tracing-only measurement."
                 "OutputPreparationMs covers RGB/raw-buffer setup, the original final vertical flip, and packing original Colour values into top-left interleaved RGB float64. PNG/PFM encoding and original cleanup are outside capture." |] }

    member this.Clean (image:RgbImage) =
        image.Dispose()
        Acceleration.listOfAccel <- []
        GC.Collect()

    member this.RenderToFile filename =
        let image = this.RenderParallel
        this.SaveImage(image, filename)
        this.Clean image

    member this.RenderToScreen : unit =
        raise (PlatformNotSupportedException("Window preview is unavailable in legacy-port. Use renderToFile to save a PNG."))
"""

let adaptRender (source: string) =
    let marker = "    member this.ShowImageOnScreen"
    let boundary = source.IndexOf(marker, StringComparison.Ordinal)
    if boundary < 0 then failwith "Original Render.fs image-I/O boundary was not found."
    source.Substring(0, boundary)
    |> replace "open System.Drawing\n" ""
    |> replace "open System.Windows.Forms\n" ""
    |> replace "open System.Runtime.InteropServices\n" ""
    |> replace "open System.Drawing.Imaging\n" ""
    |> replace "open System.Threading\n" "open System.Threading\nopen Tracer.Imaging\nopen System.Diagnostics\n"
    |> replace "type Render(scene : Scene, camera : Camera) ="
        (capturedFilmType.TrimStart('\n') + "type Render(scene : Scene, camera : Camera) =")
    |> fun prefix -> prefix + renderOutput.TrimStart('\n')

let adaptations =
    [ "Sampling/Sampling.fsi",
      "Unchanged signature copied alongside its adapted implementation because the F# compiler pairs signatures by physical path.",
      id
      "Sampling/Sampling.fs",
      "Headless diagnostic visualizers only: Bitmap becomes owned RGB8 image; sampling, RNG, locks, mappings and main dispatch are unchanged.",
      (replace "open System.Drawing\n" "open System.Drawing\nopen Tracer.Imaging\n"
       >> replace "let img = new Bitmap(size, size)" "use img = new RgbImage(size, size)"
       >> replace "(img:Bitmap)" "(img:RgbImage)"
       >> replace "sampleMethod fileName =" "sampleMethod (fileName:string) ="
       >> replace "(sampler:Sampler) fileName =" "(sampler:Sampler) (fileName:string) ="
       >> replace "(sampler:Sampler) e fileName above =" "(sampler:Sampler) e (fileName:string) above ="
       >> replace "img.Save(fileName)" "img.SavePng(fileName)")
      "Core/Vector.fs",
      "Compiler inference only: declare DivideByInt's Vector result so the later overloaded division member resolves identically.",
      replace "static member DivideByInt(a: Vector, s: int) =" "static member DivideByInt(a: Vector, s: int) : Vector ="
      "Core/Foundation.fs",
      "Compiler name resolution only: remove unused System.Numerics import that shadows the renderer's Vector type on modern .NET.",
      replace "open System.Numerics\n" ""
      "Transformation/Transformation.fsi",
      "Input API visibility only: expose the original immutable QuickMatrix record fields and existing mkTransformation pair constructor for a shared affine16 matrix/inverse input. No transformation or inverse mathematics change.",
      (replace "[<Sealed>]\ntype QuickMatrix =\n"
          ("type QuickMatrix =\n"
           + "    { Pos1x1: float; Pos1x2: float; Pos1x3: float; Pos1x4: float\n"
           + "      Pos2x1: float; Pos2x2: float; Pos2x3: float; Pos2x4: float\n"
           + "      Pos3x1: float; Pos3x2: float; Pos3x3: float; Pos3x4: float\n"
           + "      Pos4x1: float; Pos4x2: float; Pos4x3: float; Pos4x4: float }\n")
       >> replace "type Transformation\n"
           "type Transformation\n\nval mkTransformation : QuickMatrix * QuickMatrix -> Transformation\n")
      "Transformation/Transformation.fs",
      "Byte-identical implementation copy beside the visibility-adapted signature, required for F# signature pairing. All original transformation, inverse, and normal mathematics are retained.",
      id
      "Acceleration/RegularGrids.fs",
      "Compiler token spacing only: separate comparison operators from identifiers/numeric literals; no grid algorithm changes.",
      (replace "x<0." "x < 0." >> replace "x>float(b)" "x > float(b)")
      "Render.fs",
      "Headless RGB8 PNG/unsupported preview boundary, optional linear output capture and coarse Stopwatch phase boundaries only. Original Cast/PreProcessing, camera calls, array averaging, Parallel.For index order, Colour.ToColor, Y flip, acceleration reset and forced GC are retained.",
      adaptRender ]

type Adaptation =
    { original: string
      adapted: string
      originalSha256: string
      adaptedSha256: string
      reason: string }

let adaptedFiles =
    adaptations |> List.map (fun (path, reason, transform) ->
        let sourcePath = Path.Combine(original, path)
        let sourceBytes = File.ReadAllBytes sourcePath
        let source = File.ReadAllText sourcePath
        let newline = if source.Contains "\r\n" then "\r\n" else "\n"
        let transformed = transform (source.Replace("\r\n", "\n"))
        let text = if newline = "\r\n" then transformed.Replace("\n", "\r\n") else transformed
        let hasBom = sourceBytes.Length >= 3 && sourceBytes.[0..2] = [| 0xEFuy; 0xBBuy; 0xBFuy |]
        let encoding = UTF8Encoding(hasBom)
        let bytes = Array.append (encoding.GetPreamble()) (encoding.GetBytes text)
        let destination = Path.Combine(legacy, "adapted", path)
        if checkOnly then
            if not (File.Exists destination) || File.ReadAllBytes destination <> bytes then
                failwithf "Unexplained adapted source change: %s. Review and regenerate with prepare-port.fsx." (relative destination)
        else
            Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
            File.WriteAllBytes(destination, bytes)
        { original = relative sourcePath
          adapted = relative destination
          originalSha256 = hash sourceBytes
          adaptedSha256 = hash bytes
          reason = reason })

let compileFiles (project: string) =
    let document = XDocument.Load project
    document.Descendants()
    |> Seq.filter (fun element -> element.Name.LocalName = "Compile")
    |> Seq.map (fun element -> element.Attribute(XName.Get "Include").Value.Replace('\\', '/'))
    |> Seq.toList

let sourceOrder = compileFiles (Path.Combine(original, "RayTracer.fsproj"))
let adaptationPaths = adaptations |> List.map (fun (path, _, _) -> path) |> Set.ofList
let expectedOrder =
    sourceOrder |> List.map (fun path ->
        if Set.contains path adaptationPaths then "adapted/" + path
        else "original/RayTracer/" + path)
let project = Path.Combine(legacy, "LegacyRayTracer.fsproj")
if compileFiles project <> expectedOrder then
    failwith "Legacy Compile inputs/order differ from the authoritative original project."

let diffFiles first second =
    let start = ProcessStartInfo("git")
    start.WorkingDirectory <- root
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    for argument in [ "--no-pager"; "diff"; "--no-index"; "--no-ext-diff"; "--no-color"; "--"; relative first; relative second ] do
        start.ArgumentList.Add argument
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEnd()
    let error = child.StandardError.ReadToEnd()
    child.WaitForExit()
    if child.ExitCode > 1 then failwithf "Could not generate portability diff: %s" error
    output

let diff =
    [ yield diffFiles (Path.Combine(original, "RayTracer.fsproj")) project
      for item in adaptedFiles do
          yield diffFiles (Path.Combine(root, item.original)) (Path.Combine(root, item.adapted)) ]
    |> String.concat "\n"

let manifest =
    {| name = "legacy-port"
       sourceCommit = "066c59eba68d222d63b373b7e25d408796eb1d12"
       targetFramework = "net10.0"
       project = relative project
       projectSha256 = fileHash project
       adaptations = adaptedFiles
       compileInputs = expectedOrder
       sharedProject = "RayTracer.ImageIO/RayTracer.ImageIO.fsproj"
       retainedBehavior = [| "Original mathematics"; "Original RNG and shared sampler state"; "Original accelerators and global cache"; "Original per-light recursion"; "Original Colour.ToColor gamma-2 approximation"; "Original vertical flip"; "Original parallel pixel loop"; "Original cleanup and forced GC" |] |}
    |> fun value -> JsonSerializer.Serialize(value, JsonSerializerOptions(WriteIndented = true)) + "\n"

for name, content in [ "portability.diff", diff; "portability.json", manifest ] do
    let destination = Path.Combine(legacy, name)
    if checkOnly then
        if not (File.Exists destination) || File.ReadAllText destination <> content then
            failwithf "%s is stale or modified; review and regenerate with prepare-port.fsx." (relative destination)
    else File.WriteAllText(destination, content, utf8)

printfn "legacy-port: frozen hashes, %d compile inputs and %d documented adaptations verified." sourceOrder.Length adaptedFiles.Length
