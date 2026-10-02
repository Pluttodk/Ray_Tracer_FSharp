module ImageIOTests

open System
open System.Drawing
open System.IO
open Assert
open Tracer.Imaging

type private FailingWriteStream() =
    inherit MemoryStream()
    override _.Write(_: byte[], _: int, _: int) =
        raise (IOException("Deliberate output failure."))

let allTest () =
    let source = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 31uy; 63uy; 127uy |]
    let expected = Array.copy source
    use image = RgbImage.FromPixels(2, 2, source)
    source.[0] <- 0uy
    Assert.Equal(expected, image.Pixels, "image owns a copy of input RGB bytes")
    Assert.Equal(Color.Red.ToArgb(), image.GetPixel(0, 0).ToArgb(), "RGB channel order is explicit")

    use png = new MemoryStream()
    image.SavePng png
    Assert.True(png.CanWrite, "PNG encoding leaves caller stream open")
    Assert.Equal([| 137uy; 80uy; 78uy; 71uy; 13uy; 10uy; 26uy; 10uy |], png.ToArray().[0..7], "PNG signature")
    png.Position <- 0L
    use decoded = RgbImage.Load png
    Assert.True(png.CanRead, "PNG decoding leaves caller stream open")
    Assert.Equal((2, 2), (decoded.Width, decoded.Height), "PNG dimensions")
    Assert.Equal(expected, decoded.Pixels, "PNG round trip preserves bytes and vertical orientation")

    image.FlipVertical()
    Assert.Equal(Color.Blue.ToArgb(), image.GetPixel(0, 0).ToArgb(), "vertical flip places bottom row first")
    image.FlipVertical()
    Assert.Equal(expected, image.Pixels, "double vertical flip restores pixels")
    use odd = new RgbImage(1, 3)
    odd.SetPixel(0, 1, Color.Lime)
    odd.FlipVertical()
    Assert.Equal(Color.Lime.ToArgb(), odd.GetPixel(0, 1).ToArgb(), "odd-height flip retains center row")

    use jpeg = new MemoryStream()
    let writer = StbImageWriteSharp.ImageWriter()
    writer.WriteJpg([| 90uy; 140uy; 210uy |], 1, 1, StbImageWriteSharp.ColorComponents.RedGreenBlue, jpeg, 100)
    jpeg.Position <- 0L
    use decodedJpeg = RgbImage.Load jpeg
    Assert.Equal((1, 1), (decodedJpeg.Width, decodedJpeg.Height), "JPEG dimensions")
    let jpegColour = decodedJpeg.GetPixel(0, 0)
    Assert.True(abs (int jpegColour.R - 90) <= 3 && abs (int jpegColour.G - 140) <= 3 && abs (int jpegColour.B - 210) <= 3,
                "JPEG decoding retains RGB within lossy codec tolerance")

    Assert.Throws<ArgumentException>((fun () -> new RgbImage(0, 1) |> ignore), "reject zero width")
    Assert.Throws<ArgumentException>((fun () -> new RgbImage(Int32.MaxValue, Int32.MaxValue) |> ignore), "reject overflowing dimensions")
    Assert.Throws<ArgumentException>((fun () -> RgbImage.FromPixels(2, 2, [| 0uy |]) |> ignore), "reject wrong pixel count")
    Assert.Throws<ArgumentException>((fun () -> image.GetPixel(-1, 0) |> ignore), "reject out-of-bounds pixel")
    use invalid = new MemoryStream([| 1uy; 2uy; 3uy |])
    Assert.Throws<Exception>((fun () -> RgbImage.Load invalid |> ignore), "malformed image errors propagate")
    use failing = new FailingWriteStream()
    Assert.Throws<IOException>((fun () -> image.SavePng failing), "encoding write failures propagate")
    let disposed = new RgbImage(1, 1)
    disposed.Dispose()
    disposed.Dispose()
    Assert.Throws<ObjectDisposedException>((fun () -> disposed.GetPixel(0, 0) |> ignore), "disposed image rejects pixel access")

    // A scratch file in the temp directory: deterministic CI builds map __SOURCE_DIRECTORY__ to "/_/".
    let directory = Path.Combine(Path.GetTempPath(), "raytracer-image-io-tests")
    Directory.CreateDirectory directory |> ignore
    let path = Path.Combine(directory, Guid.NewGuid().ToString("N") + ".png")
    try
        File.WriteAllBytes(path, Array.zeroCreate<byte> 4096)
        image.SavePng path
        Assert.Equal(png.ToArray(), File.ReadAllBytes path, "PNG file output truncates existing files")
        use loaded = RgbImage.Load path
        use exclusive = new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
        Assert.Equal(expected, loaded.Pixels, "file decoder closes its input stream")
    finally
        File.Delete path
