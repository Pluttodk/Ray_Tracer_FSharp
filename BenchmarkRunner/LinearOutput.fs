namespace Tracer.Benchmarks

open System
open System.Globalization
open System.IO
open System.Text

module LinearOutput =
    /// Writes top-down RGB64 values as bottom-up little-endian RGB32 PFM, without transfer or clamping.
    let writePfm (path: string) (width: int) (height: int) (topDownRgb: float array) =
        if String.IsNullOrWhiteSpace path then invalidArg (nameof path) "A PFM output path is required."
        if width <= 0 then invalidArg (nameof width) "PFM width must be positive."
        if height <= 0 then invalidArg (nameof height) "PFM height must be positive."
        let pixelCount = int64 width * int64 height
        if pixelCount > int64 Array.MaxLength / 3L then
            invalidArg "dimensions" "PFM dimensions exceed the supported packed RGB array size."
        if isNull topDownRgb then nullArg (nameof topDownRgb)
        if topDownRgb.LongLength <> pixelCount * 3L then
            invalidArg (nameof topDownRgb) "PFM data must contain exactly width * height * 3 components."
        for index = 0 to topDownRgb.Length - 1 do
            let value = topDownRgb.[index]
            if not (Double.IsFinite value && Single.IsFinite(float32 value)) then
                invalidArg (nameof topDownRgb) $"Component {index} cannot be stored as finite float32 PFM data."

        let fullPath = Path.GetFullPath path
        Directory.CreateDirectory(Path.GetDirectoryName fullPath) |> ignore
        use output = File.Create fullPath
        use writer = new BinaryWriter(output, Encoding.ASCII, true)
        let dimensions = width.ToString(CultureInfo.InvariantCulture) + " " + height.ToString(CultureInfo.InvariantCulture)
        writer.Write(Encoding.ASCII.GetBytes("PF\n" + dimensions + "\n-1.0\n"))
        for y = height - 1 downto 0 do
            let start = y * width * 3
            for offset = 0 to width * 3 - 1 do
                writer.Write(float32 topDownRgb.[start + offset])
