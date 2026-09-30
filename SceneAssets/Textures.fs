namespace Tracer.SceneAssets

open System
open System.IO
open Tracer.Imaging

type BakedTexture =
    { Id: string
      Recipe: string
      Width: int
      Height: int
      Pixels: byte array }

module Textures =
    let private tau = 2. * Math.PI
    let private seed = 2026u

    let private hash x y =
        let mutable value = uint32 x * 0x9e3779b9u ^^^ uint32 y * 0x85ebca6bu ^^^ seed
        value <- (value ^^^ (value >>> 16)) * 0x7feb352du
        value <- (value ^^^ (value >>> 15)) * 0x846ca68bu
        value <- value ^^^ (value >>> 16)
        float (value &&& 0x00ffffffu) / float 0x01000000u

    let private noise periodX periodY u v =
        let x, y = u * float periodX, v * float periodY
        let ix, iy = int (floor x), int (floor y)
        let smooth t = t * t * (3. - 2. * t)
        let tx, ty = smooth (x - floor x), smooth (y - floor y)
        let at x y = hash ((x % periodX + periodX) % periodX) ((y % periodY + periodY) % periodY)
        let mix a b t = a + (b - a) * t
        mix (mix (at ix iy) (at (ix + 1) iy) tx) (mix (at ix (iy + 1)) (at (ix + 1) (iy + 1)) tx) ty

    let private fbm u v =
        0.57 * noise 4 4 u v + 0.27 * noise 8 8 u v + 0.11 * noise 16 16 u v + 0.05 * noise 32 32 u v

    let private bake id recipe width height pixel =
        let encode value = byte (int (sqrt (max 0. (min 1. value)) * 255. + 0.5))
        let pixels = Array.zeroCreate<byte> (width * height * 3)
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let u, v = (float x + 0.5) / float width, 1. - (float y + 0.5) / float height
                let r, g, b = pixel u v x y
                let offset = (y * width + x) * 3
                pixels.[offset] <- encode r
                pixels.[offset + 1] <- encode g
                pixels.[offset + 2] <- encode b
        { Id = id; Recipe = recipe; Width = width; Height = height; Pixels = pixels }

    let library () =
        [| bake "wood-grain"
               "Original walnut modulation: long warped growth bands, fine pore noise and periodic lattice noise; no photographic input"
               512 512 (fun u v x y ->
                   let flow = tau * 17. * u + 5.0 * noise 5 2 u v + 0.65 * sin (tau * v)
                   let growth = 0.5 + 0.5 * sin flow
                   let fine = 0.5 + 0.5 * sin (flow * 3. + noise 19 3 u v)
                   let pores = hash (x / 2) (y / 9)
                   let shade = 0.53 + 0.32 * growth ** 0.55 + 0.10 * fine + 0.05 * pores
                   shade, shade * 0.97, shade * 0.93)
           bake "wood-endgrain"
               "Original cut-wood modulation: perturbed concentric growth rings and tiny deterministic pores"
               512 512 (fun u v x y ->
                   let dx, dy = u - 0.43, v - 0.54
                   let radius = sqrt (dx * dx + dy * dy)
                   let rings = 0.5 + 0.5 * sin (radius * 137. + 3.8 * fbm u v)
                   let shade = 0.56 + 0.38 * rings ** 0.60 + 0.06 * hash x y
                   shade, shade * 0.98, shade * 0.95)
           bake "marble-veins"
               "Original stone modulation: warped narrow mineral veins over low-frequency cloudy value noise"
               512 512 (fun u v _ _ ->
                   let phase = tau * (2. * u + v) + 8. * noise 4 4 u v + 2.7 * noise 8 8 u v
                   let distance = sin phase / 0.11
                   let vein = exp (-(distance * distance))
                   let shade = 0.88 + 0.10 * fbm u v - 0.29 * vein
                   shade * 0.99, shade, shade * 0.985)
           bake "woven-fabric"
               "Original textile modulation: alternating over/under threads, yarn micro-variation and periodic 64-thread weave"
               512 512 (fun u v x y ->
                   let warp, weft = 0.5 + 0.5 * cos (tau * 64. * u), 0.5 + 0.5 * cos (tau * 64. * v)
                   let over = (int (u * 64.) + int (v * 64.)) % 2 = 0
                   let thread = if over then 0.70 * warp + 0.30 * weft else 0.30 * warp + 0.70 * weft
                   let shade = 0.68 + 0.24 * thread + 0.05 * noise 16 16 u v + 0.03 * hash x y
                   shade, shade, shade)
           bake "basalt"
               "Original rock modulation: three-scale tileable mineral noise and fine angular-looking sinusoidal fissures"
               512 512 (fun u v x y ->
                   let phase = tau * (5. * u - 3. * v) + 6. * noise 8 8 u v
                   let crack = max 0. (1. - abs (sin phase) * 24.)
                   let shade = 0.62 + 0.33 * fbm u v + 0.05 * hash x y - 0.13 * crack
                   shade * 0.97, shade * 0.985, shade)
           bake "brushed-metal"
               "Original subtle alloy modulation: periodic directional brushing and broad low-contrast finish variation"
               512 512 (fun u v x _ ->
                   let brush = 0.5 + 0.5 * sin (tau * 181. * u + 0.22 * sin (tau * 3. * v))
                   let shade = 0.84 + 0.055 * brush + 0.075 * noise 6 3 u v + 0.03 * hash x 26
                   shade, shade, shade) |]

    let pngBytes texture =
        use image = RgbImage.FromPixels(texture.Width, texture.Height, texture.Pixels)
        use stream = new MemoryStream()
        image.SavePng stream
        stream.ToArray()

    let validateFile (texture: BakedTexture) (path: string) =
        use decoded = RgbImage.Load path
        if decoded.Width <> texture.Width || decoded.Height <> texture.Height || decoded.Pixels <> texture.Pixels then
            failwith $"Texture round-trip does not preserve RGB bytes: {path}."
        if File.ReadAllBytes path <> pngBytes texture then
            failwith $"Texture encoding is not byte-stable: {path}."
