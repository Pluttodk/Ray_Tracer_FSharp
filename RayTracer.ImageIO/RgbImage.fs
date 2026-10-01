namespace Tracer.Imaging

open System
open System.Drawing
open System.IO

module private PixelStorage =
    let length width height =
        if width <= 0 then invalidArg (nameof width) "Image width must be positive."
        if height <= 0 then invalidArg (nameof height) "Image height must be positive."
        let count = int64 width * int64 height
        if count > int64 Array.MaxLength / 3L then invalidArg "dimensions" "The RGB image is too large."
        int (count * 3L)

/// Owns tightly packed RGB8 pixels in top-to-bottom row order. No transfer function is applied.
[<Sealed>]
type RgbImage private (width: int, height: int, pixels: byte[]) =
    let mutable data = pixels
    let mutable disposed = false

    let requirePixels () =
        if disposed then raise (ObjectDisposedException(nameof RgbImage))
        data

    let offset x y =
        if x < 0 || x >= width then invalidArg (nameof x) "Pixel X is outside the image."
        if y < 0 || y >= height then invalidArg (nameof y) "Pixel Y is outside the image."
        (y * width + x) * 3

    new(width: int, height: int) =
        new RgbImage(width, height, Array.zeroCreate (PixelStorage.length width height))

    member _.Width = width
    member _.Height = height

    /// The image-owned writable RGB array. Do not mutate it during encoding or after disposal.
    member _.Pixels = requirePixels ()

    member _.GetPixel(x: int, y: int) =
        let bytes = requirePixels ()
        let index = offset x y
        Color.FromArgb(int bytes.[index], int bytes.[index + 1], int bytes.[index + 2])

    member _.SetPixel(x: int, y: int, colour: Color) =
        let bytes = requirePixels ()
        let index = offset x y
        bytes.[index] <- colour.R
        bytes.[index + 1] <- colour.G
        bytes.[index + 2] <- colour.B

    member _.FlipVertical() =
        let bytes = requirePixels ()
        let stride = width * 3
        let row = Array.zeroCreate<byte> stride
        for y in 0 .. height / 2 - 1 do
            let top = y * stride
            let bottom = (height - 1 - y) * stride
            Array.Copy(bytes, top, row, 0, stride)
            Array.Copy(bytes, bottom, bytes, top, stride)
            Array.Copy(row, 0, bytes, bottom, stride)

    /// Encodes PNG without closing the caller-owned stream.
    member _.SavePng(stream: Stream) =
        let bytes = requirePixels ()
        ArgumentNullException.ThrowIfNull stream
        if not stream.CanWrite then invalidArg (nameof stream) "The output stream is not writable."
        let writer = StbImageWriteSharp.ImageWriter()
        writer.WritePng(bytes, width, height, StbImageWriteSharp.ColorComponents.RedGreenBlue, stream)

    member this.SavePng(path: string) =
        requirePixels () |> ignore
        use stream = File.Create path
        this.SavePng stream

    member _.Dispose() =
        if not disposed then
            disposed <- true
            data <- Array.empty

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    /// Copies input pixels; the caller retains ownership of the source array.
    static member FromPixels(width: int, height: int, pixels: byte[]) =
        ArgumentNullException.ThrowIfNull pixels
        if pixels.Length <> PixelStorage.length width height then
            invalidArg (nameof pixels) "The pixel array must contain width * height * 3 RGB bytes."
        new RgbImage(width, height, Array.copy pixels)

    /// Decodes PNG/JPEG into RGB8 without closing the caller-owned stream.
    static member Load(stream: Stream) =
        ArgumentNullException.ThrowIfNull stream
        if not stream.CanRead then invalidArg (nameof stream) "The input stream is not readable."
        let result = StbImageSharp.ImageResult.FromStream(stream, StbImageSharp.ColorComponents.RedGreenBlue)
        if result.Data.Length <> PixelStorage.length result.Width result.Height then
            raise (InvalidDataException("The decoded image has an invalid RGB pixel count."))
        new RgbImage(result.Width, result.Height, result.Data)

    static member Load(path: string) =
        use stream = File.OpenRead path
        RgbImage.Load stream

    /// Decodes PNG/JPEG into RGB8 plus a separate alpha plane (width * height bytes, top-to-bottom rows),
    /// without closing the caller-owned stream. The plane is empty when the source has no alpha channel,
    /// which means fully opaque.
    static member LoadWithAlpha(stream: Stream) : struct (RgbImage * byte[]) =
        ArgumentNullException.ThrowIfNull stream
        if not stream.CanRead then invalidArg (nameof stream) "The input stream is not readable."
        let result = StbImageSharp.ImageResult.FromStream(stream, StbImageSharp.ColorComponents.RedGreenBlueAlpha)
        let rgbLength = PixelStorage.length result.Width result.Height
        let count = rgbLength / 3
        if result.Data.Length <> count * 4 then
            raise (InvalidDataException("The decoded image has an invalid RGBA pixel count."))
        let source = result.Data
        let rgb = Array.zeroCreate<byte> rgbLength
        let hasAlpha =
            result.SourceComp = StbImageSharp.ColorComponents.RedGreenBlueAlpha
            || result.SourceComp = StbImageSharp.ColorComponents.GreyAlpha
        let alpha = if hasAlpha then Array.zeroCreate<byte> count else Array.empty
        for i in 0 .. count - 1 do
            rgb.[3 * i] <- source.[4 * i]
            rgb.[3 * i + 1] <- source.[4 * i + 1]
            rgb.[3 * i + 2] <- source.[4 * i + 2]
            if hasAlpha then alpha.[i] <- source.[4 * i + 3]
        struct (new RgbImage(result.Width, result.Height, rgb), alpha)
