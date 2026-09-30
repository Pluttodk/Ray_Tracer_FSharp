namespace Tracer.Benchmarks.Reporting

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open Tracer.SceneFormat

[<CLIMutable>]
type HashedFile = { Path: string; Sha256: string; Bytes: int64 }

[<CLIMutable>]
type ProcessResult =
    { Status: string
      ExitCode: Nullable<int>
      ElapsedMs: float
      Stdout: string
      Stderr: string
      Error: string }

[<CLIMutable>]
type RunMetadata =
    { SchemaVersion: int
      CreatedUtc: string
      Repository: string
      GitCommit: string
      GitStatus: string
      SourceDigest: string
      Sources: HashedFile array
      Dotnet: string
      Sdk: string
      OperatingSystem: string
      Architecture: string
      Cpu: string
      GpuProbe: string
      RuntimeEnvironment: string array
      Dependencies: HashedFile array
      DependencyDescriptions: string array
      Notes: string array }

[<CLIMutable>]
type CaseSpec =
    { Preset: string
      Scene: string
      ScenePath: string
      Material: string
      Engine: string
      Worker: string
      Acceleration: string
      Settings: RenderSettings
      InputFiles: HashedFile array
      ContextFingerprint: string
      Fingerprint: string
      Repeats: int
      Warmups: int }

[<CLIMutable>]
type Trial =
    { SchemaVersion: int
      Case: CaseSpec
      Repeat: int
      Warmup: bool
      Attempt: int
      StartedUtc: string
      Status: string
      Error: string
      ExitCode: Nullable<int>
      ColdMs: Nullable<float>
      Metrics: RenderMetrics
      OutputPath: string
      LinearPath: string
      MetricsPath: string
      WorkerMetadataPath: string
      StdoutPath: string
      StderrPath: string
      TrialPath: string
      Files: HashedFile array }

[<CLIMutable>]
type Distribution =
    { Count: int
      Median: float
      Min: float
      Max: float
      P25: float
      P75: float
      Mad: float }

[<CLIMutable>]
type Measurement = { Name: string; Unit: string; Distribution: Distribution }

[<CLIMutable>]
type CaseSummary =
    { Preset: string
      Scene: string
      Material: string
      Engine: string
      Fingerprint: string
      Settings: RenderSettings
      Required: int
      Completed: int
      Failed: int
      Status: string
      Measurements: Measurement array
      Errors: string array
      Representative: Trial }

[<CLIMutable>]
type ImageDifference =
    { Preset: string
      Scene: string
      Material: string
      ReferenceEngine: string
      ComparedEngine: string
      ReferencePath: string
      ComparedPath: string
      DiffPath: string
      Status: string
      Error: string
      Mae: Nullable<float>
      Rmse: Nullable<float>
      MaxAbsolute: Nullable<float>
      RelativeRmse: Nullable<float>
      DisplayScale: float }

[<CLIMutable>]
type RunManifest =
    { SchemaVersion: int
      ContextFingerprint: string
      Metadata: RunMetadata
      Cases: CaseSpec array
      TimeoutSeconds: float
      RequestedMeasuredTrials: int
      Status: string }

module Data =
    let invariant = CultureInfo.InvariantCulture
    let json = SceneFiles.jsonOptions
    let nullRecord<'T> : 'T = Unchecked.defaultof<'T>
    let serialize value = JsonSerializer.Serialize(value, json)
    let deserialize<'T> (path: string) =
        use stream = File.OpenRead path
        JsonSerializer.Deserialize<'T>(stream, json)
    let ensureParent (path: string) =
        let parent = Path.GetDirectoryName(Path.GetFullPath path)
        Directory.CreateDirectory parent |> ignore
    let writeText (path: string) (text: string) =
        ensureParent path
        let staging = path + ".writing"
        try
            File.WriteAllText(staging, text, UTF8Encoding(false))
            File.Move(staging, path, true)
        finally
            if File.Exists staging then File.Delete staging
    let writeJson path value = writeText path (serialize value)
    let persistTrial (trial: Trial) =
        if trial.Attempt <= 0 || String.IsNullOrWhiteSpace trial.OutputPath || String.IsNullOrWhiteSpace trial.TrialPath then
            invalidArg (nameof trial) "Only a started trial with concrete artifact paths can be persisted."
        writeJson (Path.Combine(Path.GetDirectoryName trial.OutputPath, "trial.json")) trial
        writeJson trial.TrialPath trial
    let invalidateTrial reason (trial: Trial) =
        if trial.Status <> "success" || String.IsNullOrWhiteSpace reason then
            invalidArg (nameof trial) "Invalidation requires a successful trial and a diagnostic reason."
        let invalidated = { trial with Status = "incomplete"; Error = reason }
        persistTrial invalidated
        invalidated
    let hashText (value: string) = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes value)).ToLowerInvariant()
    let hashFile path =
        let info = FileInfo path
        if info.Exists then { Path = path; Sha256 = SceneFiles.hashFile path; Bytes = info.Length }
        else { Path = path; Sha256 = "missing"; Bytes = -1L }
    let digestFiles (files: HashedFile array) =
        files
        |> Array.sortBy (fun file -> file.Path)
        |> Array.map (fun file -> file.Path + "\000" + file.Sha256 + "\000" + string file.Bytes)
        |> String.concat "\n"
        |> hashText
    let filesIntact (files: HashedFile array) =
        not (isNull files) && files.Length >= 3
        && files |> Array.forall (fun file ->
            let current = hashFile file.Path
            current.Bytes > 0L && current.Bytes = file.Bytes && current.Sha256 = file.Sha256)
    let nullable value = Nullable value
    let finite value = Double.IsFinite value && value >= 0.
    let nullFloat = Nullable<float>()
    let timestamp () = DateTimeOffset.UtcNow.ToString("O", invariant)
    let engineFamily = function
        | "modern-cpu" -> "cpu"
        | "modern-gpu" -> "gpu"
        | name -> name

    let runProcess (workingDirectory: string) (executable: string) (arguments: string seq)
                   (environment: (string * string) seq) (timeoutSeconds: float) (cancellation: CancellationToken) =
        use proc = new Process()
        proc.StartInfo <- ProcessStartInfo(executable, WorkingDirectory = workingDirectory, UseShellExecute = false,
                                          RedirectStandardOutput = true, RedirectStandardError = true, CreateNoWindow = true)
        for argument in arguments do proc.StartInfo.ArgumentList.Add argument
        for key, value in environment do proc.StartInfo.Environment.[key] <- value
        let clock = Stopwatch.StartNew()
        let mutable started = false
        try
            cancellation.ThrowIfCancellationRequested()
            started <- proc.Start()
            if not started then failwith $"Could not start {executable}."
            let stdout = proc.StandardOutput.ReadToEndAsync()
            let stderr = proc.StandardError.ReadToEndAsync()
            use deadline = new CancellationTokenSource(TimeSpan.FromSeconds timeoutSeconds)
            use linked = CancellationTokenSource.CreateLinkedTokenSource(cancellation, deadline.Token)
            let mutable status = "exited"
            let mutable error = ""
            try proc.WaitForExitAsync(linked.Token).GetAwaiter().GetResult()
            with :? OperationCanceledException ->
                status <- if cancellation.IsCancellationRequested then "cancelled" else "timeout"
                error <- if status = "timeout" then $"""Worker exceeded the {timeoutSeconds.ToString("R", invariant)} second process timeout."""
                         else "Benchmark cancelled."
                if not proc.HasExited then proc.Kill(entireProcessTree = true)
                proc.WaitForExit()
            clock.Stop()
            { Status = status; ExitCode = Nullable proc.ExitCode; ElapsedMs = clock.Elapsed.TotalMilliseconds
              Stdout = stdout.GetAwaiter().GetResult(); Stderr = stderr.GetAwaiter().GetResult(); Error = error }
        with ex ->
            if started then
                try
                    if not proc.HasExited then proc.Kill(entireProcessTree = true)
                    proc.WaitForExit()
                with _ -> ()
            { Status = (if cancellation.IsCancellationRequested then "cancelled" else "start-failure")
              ExitCode = Nullable(); ElapsedMs = clock.Elapsed.TotalMilliseconds; Stdout = ""; Stderr = ""; Error = ex.Message }

    let percentile quantile (sorted: float array) =
        if sorted.Length = 0 then invalidArg (nameof sorted) "At least one value is required."
        let index = quantile * float (sorted.Length - 1)
        let lo, hi = int (floor index), int (ceil index)
        sorted.[lo] + (sorted.[hi] - sorted.[lo]) * (index - float lo)
    let distribution values =
        let sorted = values |> Array.sort
        if sorted.Length = 0 then nullRecord<Distribution>
        else
            let median = percentile 0.5 sorted
            { Count = sorted.Length; Median = median; Min = sorted.[0]; Max = sorted.[sorted.Length - 1]
              P25 = percentile 0.25 sorted; P75 = percentile 0.75 sorted
              Mad = sorted |> Array.map (fun value -> abs (value - median)) |> Array.sort |> percentile 0.5 }
    let csvCell (value: string) = "\"" + (if isNull value then "" else value.Replace("\"", "\"\"")) + "\""
    let csv rows = rows |> Seq.map (Seq.map csvCell >> String.concat ",") |> String.concat "\n"
    let number (value: float) = value.ToString("R", invariant)
    let optionalNumber (value: Nullable<float>) = if value.HasValue then number value.Value else ""

    let summarize (trials: Trial array) =
        trials
        |> Array.filter (fun trial -> not trial.Warmup)
        |> Array.groupBy (fun trial -> trial.Case.Fingerprint)
        |> Array.map (fun (_, trials) ->
            let first = trials.[0].Case
            let good = trials |> Array.filter (fun trial -> trial.Status = "success")
            let values name unit chooser =
                { Name = name; Unit = unit
                  Distribution = good |> Array.choose (fun trial ->
                      let value: Nullable<float> = chooser trial
                      if value.HasValue && finite value.Value then Some value.Value else None) |> distribution }
            let phase selector trial = Nullable(selector trial.Metrics.Timings)
            let optionalPhase selector trial = selector trial.Metrics.Timings
            { Preset = first.Preset; Scene = first.Scene; Material = first.Material; Engine = first.Engine
              Fingerprint = first.Fingerprint; Settings = first.Settings; Required = first.Repeats; Completed = good.Length
              Failed = trials.Length - good.Length; Status = if good.Length = first.Repeats then "complete" else "incomplete"
              Measurements =
                [| values "cold" "ms" (fun trial -> trial.ColdMs)
                   values "worker-total" "ms" (phase (fun phases -> phases.TotalMs))
                   values "load" "ms" (phase (fun phases -> phases.LoadMs))
                   values "build" "ms" (optionalPhase (fun phases -> phases.BuildMs))
                   values "compile" "ms" (optionalPhase (fun phases -> phases.CompileMs))
                   values "upload" "ms" (optionalPhase (fun phases -> phases.UploadMs))
                   values "trace-synchronized" "ms" (phase (fun phases -> phases.TraceMs))
                   values "download" "ms" (optionalPhase (fun phases -> phases.DownloadMs))
                   values "encode" "ms" (phase (fun phases -> phases.EncodeMs))
                   values "cleanup" "ms" (phase (fun phases -> phases.CleanupMs))
                   values "allocated" "bytes" (fun trial -> if trial.Metrics.AllocatedBytes.HasValue then Nullable(float trial.Metrics.AllocatedBytes.Value) else Nullable())
                   values "peak-working-set" "bytes" (fun trial -> Nullable(float trial.Metrics.PeakWorkingSetBytes))
                   values "peak-device" "bytes" (fun trial -> if trial.Metrics.PeakDeviceBytes.HasValue then Nullable(float trial.Metrics.PeakDeviceBytes.Value) else Nullable()) |]
              Errors = trials |> Array.filter (fun trial -> trial.Status <> "success") |> Array.map (fun trial -> $"repeat {trial.Repeat}: {trial.Status}: {trial.Error}")
              Representative = if good.Length = 0 then nullRecord<Trial> else good |> Array.minBy (fun trial -> trial.Repeat) })
        |> Array.sortBy (fun summary -> summary.Preset, summary.Scene, summary.Material, summary.Engine)

module LinearImage =
    type Film = { Width: int; Height: int; Pixels: float32 array }

    let read path =
        use input = File.OpenRead path
        use reader = new BinaryReader(input, Encoding.ASCII, true)
        let line () =
            let bytes = ResizeArray<byte>()
            let mutable finished = false
            while not finished do
                let value = input.ReadByte()
                if value < 0 then raise (EndOfStreamException "Truncated PFM header.")
                elif value = 10 then finished <- true
                elif value <> 13 then bytes.Add(byte value)
                if bytes.Count > 4096 then raise (InvalidDataException "PFM header line is too long.")
            Encoding.ASCII.GetString(bytes.ToArray())
        if line () <> "PF" then raise (InvalidDataException "Linear output must be RGB PFM (PF).")
        let dimensions = (line ()).Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
        if dimensions.Length <> 2 then raise (InvalidDataException "Invalid PFM dimensions.")
        let width, height = Int32.Parse(dimensions.[0], Data.invariant), Int32.Parse(dimensions.[1], Data.invariant)
        if width <= 0 || height <= 0 || int64 width * int64 height > int64 Int32.MaxValue / 3L then
            raise (InvalidDataException "Invalid PFM dimensions.")
        let scale = Double.Parse(line (), Data.invariant)
        if not (Double.IsFinite scale) || scale = 0. then raise (InvalidDataException "Invalid PFM scale.")
        let count = width * height * 3
        if input.Length - input.Position <> int64 count * 4L then raise (InvalidDataException "PFM payload length does not match its dimensions.")
        let values = Array.zeroCreate<float32> count
        for sourceY = 0 to height - 1 do
            let target = (height - 1 - sourceY) * width * 3
            for x = 0 to width * 3 - 1 do
                let bits = reader.ReadInt32()
                let bits = if scale < 0. then bits else System.Buffers.Binary.BinaryPrimitives.ReverseEndianness bits
                values.[target + x] <- BitConverter.Int32BitsToSingle bits * float32 (abs scale)
        { Width = width; Height = height; Pixels = values }

    let write path film =
        if film.Width <= 0 || film.Height <= 0 || int64 film.Pixels.Length <> int64 film.Width * int64 film.Height * 3L then
            invalidArg (nameof film) "Invalid film dimensions."
        Data.ensureParent path
        use output = File.Create path
        use writer = new BinaryWriter(output, Encoding.ASCII, true)
        writer.Write(Encoding.ASCII.GetBytes($"PF\n{film.Width} {film.Height}\n-1.0\n"))
        for y = film.Height - 1 downto 0 do
            for x = 0 to film.Width * 3 - 1 do writer.Write(film.Pixels.[y * film.Width * 3 + x])

    let private crcTable =
        Array.init 256 (fun index ->
            let mutable value = uint32 index
            for _ = 0 to 7 do value <- if value &&& 1u = 1u then 0xedb88320u ^^^ (value >>> 1) else value >>> 1
            value)
    let private bigEndian (value: uint32) =
        [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]
    let writePng path width height (rgb: byte array) =
        if width <= 0 || height <= 0 || int64 rgb.Length <> int64 width * int64 height * 3L then
            invalidArg (nameof rgb) "Expected packed top-down RGB8 pixels."
        Data.ensureParent path
        use output = File.Create path
        output.Write([| 137uy; 80uy; 78uy; 71uy; 13uy; 10uy; 26uy; 10uy |])
        let chunk (kind: string) (data: byte array) =
            let tag = Encoding.ASCII.GetBytes kind
            output.Write(bigEndian (uint32 data.Length))
            output.Write tag
            output.Write data
            let mutable crc = 0xffffffffu
            for buffer in [| tag; data |] do
                for value in buffer do crc <- crcTable.[int ((crc ^^^ uint32 value) &&& 255u)] ^^^ (crc >>> 8)
            output.Write(bigEndian (crc ^^^ 0xffffffffu))
        chunk "IHDR" (Array.concat [| bigEndian (uint32 width); bigEndian (uint32 height); [| 8uy; 2uy; 0uy; 0uy; 0uy |] |])
        use compressed = new MemoryStream()
        do
            use zlib = new ZLibStream(compressed, CompressionLevel.Optimal, true)
            for y = 0 to height - 1 do
                zlib.WriteByte 0uy
                zlib.Write(rgb, y * width * 3, width * 3)
        chunk "IDAT" (compressed.ToArray())
        chunk "IEND" Array.empty

    let pngDimensions path =
        use input = File.OpenRead path
        use reader = new BinaryReader(input, Encoding.ASCII, true)
        if reader.ReadBytes 8 <> [| 137uy; 80uy; 78uy; 71uy; 13uy; 10uy; 26uy; 10uy |] then
            raise (InvalidDataException "Invalid PNG output signature.")
        let integer (bytes: byte array) offset = System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(offset, 4))
        let mutable width, height, channels, chunks, finished = 0, 0, 0, 0, false
        use compressed = new MemoryStream()
        while not finished do
            let lengthBytes = reader.ReadBytes 4
            if lengthBytes.Length <> 4 then raise (EndOfStreamException "PNG is missing its IEND chunk.")
            let length = integer lengthBytes 0
            if length < 0 || int64 length + 8L > input.Length - input.Position then raise (InvalidDataException "Truncated PNG chunk.")
            let kind = reader.ReadBytes 4
            let payload = reader.ReadBytes length
            let expected = System.Buffers.Binary.BinaryPrimitives.ReverseEndianness(reader.ReadUInt32())
            let mutable crc = 0xffffffffu
            for buffer in [| kind; payload |] do
                for value in buffer do crc <- crcTable.[int ((crc ^^^ uint32 value) &&& 255u)] ^^^ (crc >>> 8)
            if crc ^^^ 0xffffffffu <> expected then raise (InvalidDataException "PNG chunk checksum mismatch.")
            let name = Encoding.ASCII.GetString kind
            if chunks = 0 && name <> "IHDR" then raise (InvalidDataException "PNG must begin with IHDR.")
            match name with
            | "IHDR" ->
                if chunks <> 0 || payload.Length <> 13 then raise (InvalidDataException "Invalid PNG IHDR.")
                width <- integer payload 0
                height <- integer payload 4
                channels <- if payload.[9] = 2uy then 3 elif payload.[9] = 6uy then 4 else 0
                if width <= 0 || height <= 0 || channels = 0 || payload.[8] <> 8uy || payload.[10..12] <> [| 0uy; 0uy; 0uy |]
                   || int64 width * int64 channels + 1L > int64 Array.MaxLength then
                    raise (InvalidDataException "Worker PNG must be noninterlaced RGB8/RGBA8 with valid dimensions.")
            | "IDAT" -> compressed.Write payload
            | "IEND" ->
                if length <> 0 || compressed.Length = 0L || input.Position <> input.Length then raise (InvalidDataException "Invalid PNG end/payload.")
                finished <- true
            | _ -> ()
            chunks <- chunks + 1
        compressed.Position <- 0L
        use zlib = new ZLibStream(compressed, CompressionMode.Decompress, true)
        let row = Array.zeroCreate<byte> (width * channels + 1)
        for _ = 1 to height do
            zlib.ReadExactly row
            if row.[0] > 4uy then raise (InvalidDataException "Invalid PNG scanline filter.")
        if zlib.ReadByte() <> -1 then raise (InvalidDataException "PNG decompressed payload exceeds its dimensions.")
        width, height

    let compare reference compared =
        if reference.Width <> compared.Width || reference.Height <> compared.Height then
            raise (InvalidDataException "Cannot compare films with different dimensions.")
        let mutable sumAbs, sumSquared, referenceSquared, maximum = 0., 0., 0., 0.
        let difference = Array.zeroCreate<byte> reference.Pixels.Length
        for i = 0 to reference.Pixels.Length - 1 do
            let a, b = float reference.Pixels.[i], float compared.Pixels.[i]
            if not (Double.IsFinite a && Double.IsFinite b) then raise (InvalidDataException "Cannot compare non-finite linear pixels.")
            let delta = abs (a - b)
            sumAbs <- sumAbs + delta
            sumSquared <- sumSquared + delta * delta
            referenceSquared <- referenceSquared + a * a
            maximum <- max maximum delta
            difference.[i] <- byte (int (min 1. (sqrt (delta * 4.)) * 255.))
        let count = float reference.Pixels.Length
        let relative = if referenceSquared > 0. then Nullable(sqrt (sumSquared / referenceSquared))
                       elif sumSquared = 0. then Nullable 0. else Nullable()
        sumAbs / count, sqrt (sumSquared / count), maximum, relative, difference
