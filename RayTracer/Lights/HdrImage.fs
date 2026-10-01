namespace Tracer.Basics

open System
open System.IO
open System.Text
open Tracer.Basics.Sampling

/// A floating-point RGB image, row 0 at the top, three floats per pixel.
type HdrImage =
    { Width: int
      Height: int
      Pixels: float32[] }

/// Radiance RGBE (.hdr) files: reading (flat, old-style run-length and adaptive run-length
/// scanlines) and writing (flat or run-length).
///
/// Decoding follows Blender's reader: a pixel (r, g, b, e) with e > 0 is
/// ((r, g, b) + 0.5) * 2^(e - 136), and e = 0 is black. The EXPOSURE header is ignored, as Blender
/// and three.js do, so radiance values match what Cycles renders with the same file.
[<RequireQualifiedAccess>]
module HdrImage =
    let luminance (r: float32) (g: float32) (b: float32) = 0.2126 * float r + 0.7152 * float g + 0.0722 * float b

    let private fail message = raise (InvalidDataException("Radiance HDR: " + message))

    let private decodePixel (r: byte) (g: byte) (b: byte) (e: byte) (pixels: float32[]) offset =
        if e = 0uy then
            pixels.[offset] <- 0.f; pixels.[offset + 1] <- 0.f; pixels.[offset + 2] <- 0.f
        else
            let f = Math.ScaleB(1., int e - 136)
            pixels.[offset] <- float32 ((float r + 0.5) * f)
            pixels.[offset + 1] <- float32 ((float g + 0.5) * f)
            pixels.[offset + 2] <- float32 ((float b + 0.5) * f)

    /// Decodes the bytes of a .hdr file.
    let decode (bytes: byte[]) : HdrImage =
        let mutable pos = 0
        let readLine () =
            let start = pos
            while pos < bytes.Length && bytes.[pos] <> 10uy do pos <- pos + 1
            if pos >= bytes.Length then fail "truncated header."
            let line = Encoding.ASCII.GetString(bytes, start, pos - start).TrimEnd('\r')
            pos <- pos + 1
            line
        let magic = readLine ()
        if not (magic.StartsWith "#?") then fail "missing #? signature."
        let mutable format = ""
        let mutable reading = true
        while reading do
            let line = readLine ()
            if line.Length = 0 then reading <- false
            elif line.StartsWith "FORMAT=" then format <- line.Substring 7
        if format <> "" && format <> "32-bit_rle_rgbe" then fail (sprintf "unsupported format %s." format)
        let resolution = (readLine ()).Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        if resolution.Length <> 4 || resolution.[2] <> "+X" || (resolution.[0] <> "-Y" && resolution.[0] <> "+Y") then
            fail (sprintf "unsupported orientation %s." (String.Join(" ", resolution)))
        let height = int resolution.[1]
        let width = int resolution.[3]
        if width < 1 || height < 1 then fail "invalid resolution."
        let bottomUp = resolution.[0] = "+Y"
        let pixels = Array.zeroCreate<float32> (width * height * 3)
        let scan = Array.zeroCreate<byte> (width * 4)
        let need n = if pos + n > bytes.Length then fail "truncated pixel data."
        for row in 0 .. height - 1 do
            need 4
            if width >= 8 && width < 32768 && bytes.[pos] = 2uy && bytes.[pos + 1] = 2uy && bytes.[pos + 2] &&& 0x80uy = 0uy then
                // Adaptive run-length: each of the four components is run-length coded in turn.
                if (int bytes.[pos + 2] <<< 8 ||| int bytes.[pos + 3]) <> width then fail "scanline width mismatch."
                pos <- pos + 4
                for channel in 0 .. 3 do
                    let mutable x = 0
                    while x < width do
                        need 1
                        let count = int bytes.[pos]
                        pos <- pos + 1
                        if count > 128 then
                            let run = count - 128
                            need 1
                            if x + run > width then fail "run overflows the scanline."
                            let value = bytes.[pos]
                            pos <- pos + 1
                            for k in 0 .. run - 1 do scan.[(x + k) * 4 + channel] <- value
                            x <- x + run
                        else
                            if count = 0 || x + count > width then fail "bad literal run."
                            need count
                            for k in 0 .. count - 1 do scan.[(x + k) * 4 + channel] <- bytes.[pos + k]
                            pos <- pos + count
                            x <- x + count
            else
                // Flat pixels, possibly with old-style runs (1, 1, 1, n repeats the previous pixel).
                let mutable x = 0
                let mutable shift = 0
                while x < width do
                    need 4
                    let r, g, b, e = bytes.[pos], bytes.[pos + 1], bytes.[pos + 2], bytes.[pos + 3]
                    pos <- pos + 4
                    if r = 1uy && g = 1uy && b = 1uy && x > 0 then
                        let run = int e <<< shift
                        if x + run > width then fail "old-style run overflows the scanline."
                        for k in 0 .. run - 1 do
                            for c in 0 .. 3 do scan.[(x + k) * 4 + c] <- scan.[(x - 1) * 4 + c]
                        x <- x + run
                        shift <- shift + 8
                    else
                        scan.[x * 4] <- r; scan.[x * 4 + 1] <- g; scan.[x * 4 + 2] <- b; scan.[x * 4 + 3] <- e
                        x <- x + 1
                        shift <- 0
            let target = if bottomUp then height - 1 - row else row
            for x in 0 .. width - 1 do
                decodePixel scan.[x * 4] scan.[x * 4 + 1] scan.[x * 4 + 2] scan.[x * 4 + 3] pixels ((target * width + x) * 3)
        { Width = width; Height = height; Pixels = pixels }

    let load (path: string) = decode (File.ReadAllBytes path)

    /// RGBE encoding of one pixel (Ward's: the largest component's mantissa in [128, 256)).
    let encodePixel (r: float) (g: float) (b: float) =
        let m = max r (max g b)
        if not (m > 1e-32) || not (Double.IsFinite m) then struct (0uy, 0uy, 0uy, 0uy)
        else
            let exponent = int (Math.Floor(Math.Log2 m)) + 1
            let scale = Math.ScaleB(256., -exponent)
            let q v = byte (min 255. (max 0. (Math.Floor(v * scale))))
            struct (q r, q g, q b, byte (exponent + 128))

    /// Encodes an image as a .hdr file, with adaptive run-length scanlines or flat ones.
    let encode (image: HdrImage) (runLength: bool) : byte[] =
        use stream = new MemoryStream()
        let header = sprintf "#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y %d +X %d\n" image.Height image.Width
        let headerBytes = Encoding.ASCII.GetBytes header
        stream.Write(headerBytes, 0, headerBytes.Length)
        let scan = Array.zeroCreate<byte> (image.Width * 4)
        for row in 0 .. image.Height - 1 do
            for x in 0 .. image.Width - 1 do
                let o = (row * image.Width + x) * 3
                let struct (r, g, b, e) = encodePixel (float image.Pixels.[o]) (float image.Pixels.[o + 1]) (float image.Pixels.[o + 2])
                scan.[x * 4] <- r; scan.[x * 4 + 1] <- g; scan.[x * 4 + 2] <- b; scan.[x * 4 + 3] <- e
            if runLength && image.Width >= 8 && image.Width < 32768 then
                stream.WriteByte 2uy; stream.WriteByte 2uy
                stream.WriteByte(byte (image.Width >>> 8)); stream.WriteByte(byte (image.Width &&& 0xFF))
                for channel in 0 .. 3 do
                    let value x = scan.[x * 4 + channel]
                    let mutable x = 0
                    while x < image.Width do
                        // A run of at least three equal bytes is coded as a run, anything else literally.
                        let mutable run = 1
                        while x + run < image.Width && run < 127 && value (x + run) = value x do run <- run + 1
                        if run >= 3 then
                            stream.WriteByte(byte (128 + run)); stream.WriteByte(value x)
                            x <- x + run
                        else
                            let start = x
                            let mutable count = 0
                            let mutable stop = false
                            while not stop && x < image.Width && count < 128 do
                                let mutable ahead = 1
                                while x + ahead < image.Width && ahead < 3 && value (x + ahead) = value x do ahead <- ahead + 1
                                if ahead >= 3 then stop <- true
                                else
                                    x <- x + 1
                                    count <- count + 1
                            stream.WriteByte(byte count)
                            for k in 0 .. count - 1 do stream.WriteByte(value (start + k))
            else stream.Write(scan, 0, scan.Length)
        stream.ToArray()

    let save (path: string) (image: HdrImage) (runLength: bool) = File.WriteAllBytes(path, encode image runLength)

/// A sun found in an HDR environment and taken out of it (see `HdrEnvironment.extractSun`).
type HdrSun =
    { /// World-space direction towards the sun (rotation applied).
      Direction: Vector
      /// Irradiance at normal incidence removed from the map, render units (before any intensity scale).
      Irradiance: Colour
      /// Pixels that were clamped.
      Pixels: int
      /// Luminance the clamped pixels were reduced to.
      Threshold: float
      /// Share of the map's total luminous power (over the sphere) that the sun carried.
      PowerFraction: float }

/// Environment lighting from an equirectangular (lat-long) HDR image.
///
/// Mapping convention (identical to three.js and glTF viewers, and to Blender's Environment
/// Texture node in a glTF-imported scene, verified by rendering a probe image in Cycles):
/// with +Y up, the lookup direction d = (x, y, z) reads image column u = 0.5 + atan2(z, x) / (2 pi)
/// and row v = acos(y) / pi measured from the top. So the image centre (u = 0.5) lies towards +X,
/// u = 0.25 towards -Z, u = 0.75 towards +Z and the left/right seam towards -X. In Blender's Z-up
/// frame (glTF importer: Blender (x, y, z) = glTF (x, -z, y)) that is the centre towards +X and
/// u = 0.25 towards +Y.
///
/// `rotation` (radians) rotates the lookup about +Y: d is replaced by R_y(rotation) d, with
/// R_y(a) (x, y, z) = (x cos a + z sin a, y, -x sin a + z cos a). That is exactly a Blender Mapping
/// node of type Point with Rotation Z = `rotation` feeding the Environment Texture's vector, so
/// increasing it turns the environment clockwise seen from above (towards -azimuth).
[<RequireQualifiedAccess>]
module HdrEnvironment =
    let private luminanceOf (c: Colour) = 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B

    /// The lookup direction for a world direction.
    let rotate (rotation: float) (d: Vector) =
        if rotation = 0. then d
        else
            let c, s = Math.Cos rotation, Math.Sin rotation
            Vector(d.X * c + d.Z * s, d.Y, -d.X * s + d.Z * c)

    /// The world direction whose lookup is `d` (inverse of `rotate`).
    let unrotate (rotation: float) (d: Vector) = rotate -rotation d

    /// Continuous image coordinates (column, row from the top, in [0, 1)) of a lookup direction.
    let uv (d: Vector) =
        let d = d.Normalise
        let u = 0.5 + Math.Atan2(d.Z, d.X) / (2. * Math.PI)
        let v = Math.Acos(max -1. (min 1. d.Y)) / Math.PI
        struct ((if u >= 1. then u - 1. else u), v)

    /// The lookup direction at image coordinates (u, v), v from the top.
    let direction (u: float) (v: float) =
        let polar = v * Math.PI
        let phi = (u - 0.5) * 2. * Math.PI
        let s = Math.Sin polar
        Vector(s * Math.Cos phi, Math.Cos polar, s * Math.Sin phi)

    /// Bilinear radiance lookup (wrapping horizontally, clamped vertically) for a world direction.
    let sample (image: HdrImage) (rotation: float) (worldDirection: Vector) =
        let struct (u, v) = uv (rotate rotation worldDirection)
        let x = u * float image.Width - 0.5
        let y = v * float image.Height - 0.5
        let x0 = int (Math.Floor x)
        let y0 = int (Math.Floor y)
        let fx = x - float x0
        let fy = y - float y0
        let wrap i = ((i % image.Width) + image.Width) % image.Width
        let clampRow j = max 0 (min (image.Height - 1) j)
        let xa, xb = wrap x0, wrap (x0 + 1)
        let ya, yb = clampRow y0, clampRow (y0 + 1)
        let p = image.Pixels
        let inline at xx yy c = float p.[(yy * image.Width + xx) * 3 + c]
        let channel c =
            (1. - fy) * ((1. - fx) * at xa ya c + fx * at xb ya c) + fy * ((1. - fx) * at xa yb c + fx * at xb yb c)
        Colour(max 0. (channel 0), max 0. (channel 1), max 0. (channel 2))

    /// Solid angle of a pixel in row `row`.
    let private pixelSolidAngle (image: HdrImage) row =
        let c0 = Math.Cos(Math.PI * float row / float image.Height)
        let c1 = Math.Cos(Math.PI * float (row + 1) / float image.Height)
        2. * Math.PI / float image.Width * (c0 - c1)

    /// An importance table tabulated straight from the pixels: every pixel's luminance times its
    /// solid angle lands in the table cell containing its centre (so even a sun a few pixels across
    /// keeps its full weight), and each cell is raised to at least a quarter of its brightest
    /// neighbour so the bilinear skirt around a bright spot is never sampled with a tiny density.
    /// The width is capped at the image width; the height is half the width.
    let distribution (image: HdrImage) (rotation: float) (tableWidth: int) =
        let width = max 2 (min tableWidth image.Width)
        let height = max 1 (width / 2)
        let sums = Array.zeroCreate<float> (width * height)
        for row in 0 .. image.Height - 1 do
            let omega = pixelSolidAngle image row
            let v = (float row + 0.5) / float image.Height
            for column in 0 .. image.Width - 1 do
                let o = (row * image.Width + column) * 3
                let l = HdrImage.luminance image.Pixels.[o] image.Pixels.[o + 1] image.Pixels.[o + 2]
                if l > 0. && Double.IsFinite l then
                    let d = unrotate rotation (direction ((float column + 0.5) / float image.Width) v)
                    let polar = Math.Acos(max -1. (min 1. d.Y))
                    let j = min (height - 1) (max 0 (int (polar / Math.PI * float height)))
                    let azimuth = Math.Atan2(d.X, d.Z)
                    let a = (if azimuth < 0. then azimuth + 2. * Math.PI else azimuth) / (2. * Math.PI)
                    let i = min (width - 1) (max 0 (int (a * float width)))
                    sums.[j * width + i] <- sums.[j * width + i] + l * omega
        // Convert to mean luminance per steradian for the neighbourhood rule, then back to weights.
        let cosineAt j = Math.Cos(Math.PI * float j / float height)
        let cellOmega = Array.init height (fun j -> 2. * Math.PI / float width * (cosineAt j - cosineAt (j + 1)))
        let density = Array.init (width * height) (fun k -> sums.[k] / cellOmega.[k / width])
        let weights =
            Array.init (width * height) (fun k ->
                let j, i = k / width, k % width
                let mutable neighbour = 0.
                for dj in -1 .. 1 do
                    let jj = j + dj
                    if jj >= 0 && jj < height then
                        for di in -1 .. 1 do
                            let ii = ((i + di) % width + width) % width
                            neighbour <- max neighbour density.[jj * width + ii]
                max density.[k] (0.25 * neighbour) * cellOmega.[j])
        let mean = Array.average weights
        let floor = if mean > 0. then 1e-3 * mean else 1.
        EnvironmentDistribution(width, height, weights |> Array.map (fun w -> max w floor))

    /// Finds the sun (the brightest pixel and the connected region around it, within
    /// `maxAngleDegrees`, brighter than `threshold`), clamps that region's luminance to the threshold,
    /// and returns the clamped image with the removed energy as a sun. `threshold` None picks a
    /// level automatically: the larger of 1/1000 of the peak and 50 times the map's median.
    let extractSun (image: HdrImage) (rotation: float) (threshold: float option) (maxAngleDegrees: float) =
        let w, h = image.Width, image.Height
        let lum = Array.init (w * h) (fun k -> HdrImage.luminance image.Pixels.[k * 3] image.Pixels.[k * 3 + 1] image.Pixels.[k * 3 + 2])
        let peakIndex = lum |> Array.mapi (fun k l -> struct (k, l)) |> Array.maxBy (fun struct (_, l) -> l) |> fun struct (k, _) -> k
        let peak = lum.[peakIndex]
        let median =
            let sorted = Array.copy lum
            Array.sortInPlace sorted
            sorted.[sorted.Length / 2]
        let level = match threshold with Some t -> t | None -> max (peak / 1000.) (50. * median)
        let pixelDirection k = direction ((float (k % w) + 0.5) / float w) ((float (k / w) + 0.5) / float h)
        let peakDirection = pixelDirection peakIndex
        let cosLimit = Math.Cos(maxAngleDegrees * Math.PI / 180.)
        let pixels = Array.copy image.Pixels
        let visited = Collections.Generic.HashSet<int>()
        let stack = Collections.Generic.Stack<int>()
        let mutable removedR, removedG, removedB = 0., 0., 0.
        let mutable sx, sy, sz = 0., 0., 0.
        let mutable count = 0
        if peak > level then
            stack.Push peakIndex
            visited.Add peakIndex |> ignore
        while stack.Count > 0 do
            let k = stack.Pop()
            let l = lum.[k]
            let o = k * 3
            let scale = level / l
            let omega = pixelSolidAngle image (k / w)
            let r, g, b = float pixels.[o], float pixels.[o + 1], float pixels.[o + 2]
            pixels.[o] <- float32 (r * scale); pixels.[o + 1] <- float32 (g * scale); pixels.[o + 2] <- float32 (b * scale)
            let dr, dg, db = r * (1. - scale) * omega, g * (1. - scale) * omega, b * (1. - scale) * omega
            removedR <- removedR + dr; removedG <- removedG + dg; removedB <- removedB + db
            let weight = 0.2126 * dr + 0.7152 * dg + 0.0722 * db
            let d = pixelDirection k
            sx <- sx + weight * d.X; sy <- sy + weight * d.Y; sz <- sz + weight * d.Z
            count <- count + 1
            let x, y = k % w, k / w
            for dy in -1 .. 1 do
                for dx in -1 .. 1 do
                    let yy = y + dy
                    if yy >= 0 && yy < h then
                        let n = yy * w + ((x + dx) % w + w) % w
                        if lum.[n] > level && not (visited.Contains n) && pixelDirection n * peakDirection >= cosLimit then
                            visited.Add n |> ignore
                            stack.Push n
        let mutable total = 0.
        for row in 0 .. h - 1 do
            let omega = pixelSolidAngle image row
            for column in 0 .. w - 1 do total <- total + lum.[row * w + column] * omega
        let irradiance = Colour(removedR, removedG, removedB)
        let lookupDirection = if count > 0 then Vector(sx, sy, sz).Normalise else peakDirection
        { Width = w; Height = h; Pixels = pixels },
        { Direction = unrotate rotation lookupDirection; Irradiance = irradiance; Pixels = count; Threshold = level
          PowerFraction = if total > 0. then luminanceOf irradiance / total else 0. }

    /// An importance-sampled environment light lighting with `lighting` and showing `visible` to
    /// camera rays and specular chains (pass the original map there when the sun was extracted
    /// into a DirectionalLight, as Sky.fs does with its disc). Radiance is scaled by `intensity`.
    /// `samplesPerAxis`^2 is the classic integrator's per-hit sample count; `tableWidth` the width
    /// of the importance table (1024 suits a 4k map).
    let light (lighting: HdrImage) (visible: HdrImage option) (rotation: float) (intensity: float)
              (samplesPerAxis: int) (tableWidth: int) =
        if not (Double.IsFinite intensity) || intensity < 0. then invalidArg (nameof intensity) "Intensity must be finite and non-negative."
        let radiance d = sample lighting rotation d * intensity
        let seen =
            match visible with
            | Some image -> fun d -> sample image rotation d * intensity
            | None -> radiance
        let table = distribution lighting rotation tableWidth
        let texture =
            Textures.mkTexture (fun u v ->
                // EnvironmentLight's own (u, v) layout, for consumers that read the texture.
                let polar = (1. - v) * Math.PI
                let azimuth = 2. * Math.PI * u
                let d = Vector(sin polar * sin azimuth, cos polar, sin polar * cos azimuth)
                EmissiveMaterial(radiance d, 1.) :> Material)
        // Many sample sets: the classic integrator picks one per hit, and few sets correlate pixels.
        EnvironmentLight(1e6, texture, multiJittered samplesPerAxis 127, Some radiance, Some seen, tableWidth, Some table)

    /// The sun extracted from the map as a DirectionalLight, scaled by `intensity`.
    let sunLight (sun: HdrSun) (intensity: float) =
        DirectionalLight(sun.Irradiance * intensity, 1., sun.Direction)
