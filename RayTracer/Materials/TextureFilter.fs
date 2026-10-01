namespace Tracer.Basics

open System

/// How texture coordinates outside [0,1) map back into the image (the glTF sampler wrap modes).
type WrapMode =
    | Repeat
    | ClampToEdge
    | MirroredRepeat

/// A linear-light image for filtered lookups. Row 0 is the top of the image, which is where glTF
/// texture coordinate t = 0 points.
///
/// Texels are stored either as floats (`Data`) or, for 8-bit sources, as the original bytes (`Bytes`)
/// decoded through a per-channel table (`Lut`) on lookup. A 4K RGB texture is 50 MB as bytes against
/// 200 MB as floats, and Sponza's 70-odd textures were most of the renderer's memory. Both forms read
/// back the same float32 values.
[<NoEquality; NoComparison>]
type FilterImage =
    { Width: int
      Height: int
      /// Values per texel, 1 to 4.
      Channels: int
      /// Row-major, `Channels` values per texel. Empty when the image is byte-encoded.
      Data: float32[]
      /// Row-major, `Channels` bytes per texel. Empty unless the image is byte-encoded.
      Bytes: byte[]
      /// 256 decoded values per channel: channel c's byte b reads as Lut.[256 * c + b].
      Lut: float32[] }

/// Bilinear and nearest texture lookups.
///
/// Texel centres sit at ((x + 0.5) / width, (y + 0.5) / height), so a lookup exactly at a centre returns
/// that texel unfiltered, and a lookup between centres blends the four nearest texels.
module TextureFilter =
    let create (width: int) (height: int) (channels: int) (data: float32[]) =
        if width <= 0 || height <= 0 then invalidArg (nameof width) "A filter image needs a positive size."
        if channels < 1 || channels > 4 then invalidArg (nameof channels) "A filter image has 1 to 4 channels."
        if data.Length <> width * height * channels then invalidArg (nameof data) "Image data does not match its size."
        { Width = width; Height = height; Channels = channels; Data = data; Bytes = [||]; Lut = [||] }

    /// An 8-bit image kept as bytes. `lut` holds 256 values per channel (channel c's byte b decodes to
    /// lut.[256 * c + b]), so lookups return exactly what `create` with the decoded floats would.
    let createEncoded (width: int) (height: int) (channels: int) (bytes: byte[]) (lut: float32[]) =
        if width <= 0 || height <= 0 then invalidArg (nameof width) "A filter image needs a positive size."
        if channels < 1 || channels > 4 then invalidArg (nameof channels) "A filter image has 1 to 4 channels."
        if bytes.Length <> width * height * channels then invalidArg (nameof bytes) "Image data does not match its size."
        if lut.Length <> 256 * channels then invalidArg (nameof lut) "Give 256 decoded values per channel."
        { Width = width; Height = height; Channels = channels; Data = [||]; Bytes = bytes; Lut = lut }

    /// Map an integer texel index into [0, n).
    let wrapIndex (mode: WrapMode) (i: int) (n: int) =
        match mode with
        | ClampToEdge -> if i < 0 then 0 elif i >= n then n - 1 else i
        | Repeat -> let r = i % n in if r < 0 then r + n else r
        | MirroredRepeat ->
            let period = 2 * n
            let r = let m = i % period in if m < 0 then m + period else m
            if r < n then r else period - 1 - r

    let inline private channel (image: FilterImage) x y c =
        let index = (y * image.Width + x) * image.Channels + c
        if image.Bytes.Length > 0 then float image.Lut.[(c <<< 8) + int image.Bytes.[index]]
        else float image.Data.[index]

    /// The texel as RGBA; missing channels read as the first channel (grey) and alpha as 1.
    let texel (image: FilterImage) (x: int) (y: int) =
        match image.Channels with
        | 1 -> let v = channel image x y 0 in struct (v, v, v, 1.)
        | 2 -> let v = channel image x y 0 in struct (v, v, v, channel image x y 1)
        | 3 -> struct (channel image x y 0, channel image x y 1, channel image x y 2, 1.)
        | _ -> struct (channel image x y 0, channel image x y 1, channel image x y 2, channel image x y 3)

    let private finite (v: float) = if Double.IsFinite v then v else 0.

    /// Nearest-texel lookup at texture coordinates (s, t).
    let nearest (image: FilterImage) (wrapS: WrapMode) (wrapT: WrapMode) (s: float) (t: float) =
        let x = wrapIndex wrapS (int (floor (finite s * float image.Width))) image.Width
        let y = wrapIndex wrapT (int (floor (finite t * float image.Height))) image.Height
        texel image x y

    /// Bilinear lookup at texture coordinates (s, t).
    let bilinear (image: FilterImage) (wrapS: WrapMode) (wrapT: WrapMode) (s: float) (t: float) =
        let fx = finite s * float image.Width - 0.5
        let fy = finite t * float image.Height - 0.5
        let x0f = floor fx
        let y0f = floor fy
        let dx = fx - x0f
        let dy = fy - y0f
        let x0 = int x0f
        let y0 = int y0f
        let xa = wrapIndex wrapS x0 image.Width
        let xb = wrapIndex wrapS (x0 + 1) image.Width
        let ya = wrapIndex wrapT y0 image.Height
        let yb = wrapIndex wrapT (y0 + 1) image.Height
        let struct (r00, g00, b00, a00) = texel image xa ya
        let struct (r10, g10, b10, a10) = texel image xb ya
        let struct (r01, g01, b01, a01) = texel image xa yb
        let struct (r11, g11, b11, a11) = texel image xb yb
        // Nested lerps rather than four weights: a uniform neighbourhood then returns its value exactly.
        let inline mix a b c d =
            let top = a + (b - a) * dx
            let bottom = c + (d - c) * dx
            top + (bottom - top) * dy
        struct (mix r00 r10 r01 r11, mix g00 g10 g01 g11, mix b00 b10 b01 b11, mix a00 a10 a01 a11)

    /// The sRGB transfer function's inverse, for colour textures stored in sRGB.
    let srgbToLinear (v: float) =
        if v <= 0.04045 then v / 12.92 else Math.Pow((v + 0.055) / 1.055, 2.4)
