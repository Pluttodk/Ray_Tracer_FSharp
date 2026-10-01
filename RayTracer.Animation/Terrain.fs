namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Basics.Textures

/// Procedural mountains: a ridged fractal heightfield turned into a triangle mesh.
module Terrain =
    /// Deterministic lattice hash in [0, 1).
    let private hash (x: int) (z: int) (seed: int) =
        let mutable h = uint32 x * 374761393u + uint32 z * 668265263u + uint32 seed * 2246822519u
        h <- (h ^^^ (h >>> 13)) * 1274126177u
        h <- h ^^^ (h >>> 16)
        float h / 4294967296.

    /// Smooth value noise in [0, 1).
    let valueNoise (x: float) (z: float) seed =
        let x0, z0 = int (floor x), int (floor z)
        let fx, fz = x - floor x, z - floor z
        let s t = t * t * t * (t * (t * 6. - 15.) + 10.)
        let sx, sz = s fx, s fz
        let a, b = hash x0 z0 seed, hash (x0 + 1) z0 seed
        let c, d = hash x0 (z0 + 1) seed, hash (x0 + 1) (z0 + 1) seed
        let top = a + sx * (b - a)
        let bottom = c + sx * (d - c)
        top + sz * (bottom - top)

    /// Ridged multifractal: sharp crests where the noise folds, with detail concentrated on the ridges.
    let ridged (x: float) (z: float) octaves seed =
        let mutable sum, amplitude, frequency, weight, norm = 0., 1., 1., 1., 0.
        for octave in 0 .. octaves - 1 do
            let n = 1. - abs (2. * valueNoise (x * frequency) (z * frequency) (seed + octave * 101) - 1.)
            let n = n * n * weight
            weight <- min 1. (n * 1.8)
            sum <- sum + n * amplitude
            norm <- norm + amplitude
            amplitude <- amplitude * 0.5
            frequency <- frequency * 2.03
        sum / norm

    type Peak = { X: float; Z: float; Height: float; Radius: float }

    type Settings =
        { /// Side length of the square terrain, centred on the origin.
          Size: float
          /// Vertices per side.
          Resolution: int
          /// Height of the ridged background relief.
          Relief: float
          /// Horizontal scale of the relief features.
          FeatureSize: float
          /// Individual mountains added on top of the relief.
          Peaks: Peak list
          /// Altitude above which gentle slopes hold snow.
          SnowLine: float
          Seed: int }

    /// Terrain height at (x, z).
    let height (settings: Settings) (x: float) (z: float) =
        let f = settings.FeatureSize
        let relief = settings.Relief * ridged (x / f) (z / f) 7 settings.Seed
        let massifs =
            settings.Peaks |> List.sumBy (fun p ->
                let d2 = ((x - p.X) * (x - p.X) + (z - p.Z) * (z - p.Z)) / (p.Radius * p.Radius)
                let shape = exp (-d2 * 1.6)
                // Break up the cone with ridges that grow towards the summit.
                p.Height * shape * (0.7 + 0.45 * ridged (x / (0.35 * f) + 17.) (z / (0.35 * f) - 5.) 6 (settings.Seed + 7)))
        relief + massifs

    let private smoothstep a b x = let t = max 0. (min 1. ((x - a) / (b - a))) in t * t * (3. - 2. * t)
    let private mix (a: Colour) (b: Colour) w = a * (1. - w) + b * w

    /// Ground colour from altitude and steepness: forest low down, meadow above it, rock on steep or high
    /// ground, snow on high and gentle slopes. Blended smoothly so no seams follow the mesh.
    let colourAt (settings: Settings) altitude slope =
        let line = settings.SnowLine
        let forest, meadow = Colour(0.05, 0.12, 0.045), Colour(0.2, 0.24, 0.09)
        let rock, snow = Colour(0.3, 0.27, 0.24), Colour(0.88, 0.89, 0.93)
        let grass = mix meadow forest (1. - smoothstep (0.2 * line) (0.5 * line) altitude)
        let rocky = max (smoothstep 0.33 0.55 slope) (smoothstep (0.65 * line) (0.92 * line) altitude)
        let snowy = smoothstep (0.9 * line) (1.05 * line) altitude * (1. - smoothstep 0.42 0.62 slope)
        mix (mix grass rock rocky) snow snowy

    /// Height and slope are stored in the vertex UVs (u = altitude / (1.5 x snow line), v = steepness), so the
    /// texture classifies each shading point from a 64 x 64 table of prebuilt materials (no allocation per hit).
    let private texture (settings: Settings) =
        let size = 64
        let table =
            Array2D.init size size (fun i j ->
                let altitude = (float i + 0.5) / float size * settings.SnowLine * 1.5
                let slope = (float j + 0.5) / float size
                let c = colourAt settings altitude slope
                MatteMaterial(c, 0.3, c, 0.85) :> Material)
        mkTexture (fun u v ->
            let i = max 0 (min (size - 1) (int (u * float size)))
            let j = max 0 (min (size - 1) (int (v * float size)))
            table.[i, j])
        |> markOpaque

    /// Distant ranges: the normal colouring, slightly desaturated and with a matte finish. Haze and
    /// distance come from the scene's atmosphere, not from the texture.
    let farTexture (settings: Settings) =
        let tones =
            Array2D.init 32 32 (fun i j ->
                let altitude = (float i + 0.5) / 32. * settings.SnowLine * 1.5
                let slope = (float j + 0.5) / 32.
                let c = colourAt settings altitude slope
                let grey = (c.R + c.G + c.B) / 3.
                let c = c * 0.8 + Colour(grey, grey, grey) * 0.2
                MatteMaterial(c, 0.3, c, 0.85) :> Material)
        mkTexture (fun u v -> tones.[max 0 (min 31 (int (u * 32.))), max 0 (min 31 (int (v * 32.)))])
        |> markOpaque

    /// A terrain mesh over [-Size/2, Size/2]^2 for any height function, shaded with `texture`.
    let buildWith (settings: Settings) (heightAt: float -> float -> float) (texture: Texture) =
        let n = settings.Resolution
        if n < 2 then invalidArg (nameof settings) "The terrain needs at least 2 vertices per side."
        let step = settings.Size / float (n - 1)
        let coordinate i = -settings.Size / 2. + float i * step
        let heights = Array2D.init n n (fun i j -> heightAt (coordinate i) (coordinate j))
        let positions = Array.init (n * n) (fun k -> let i, j = k % n, k / n in Point(coordinate i, heights.[i, j], coordinate j))
        let at i j = heights.[max 0 (min (n - 1) i), max 0 (min (n - 1) j)]
        let normals =
            Array.init (n * n) (fun k ->
                let i, j = k % n, k / n
                Vector(at (i - 1) j - at (i + 1) j, 2. * step, at i (j - 1) - at i (j + 1)).Normalise)
        let uvs =
            Array.init (n * n) (fun k ->
                let p = positions.[k]
                let dither = 0.06 * (valueNoise (p.X / 7.) (p.Z / 7.) 99 - 0.5)
                (p.Y / (settings.SnowLine * 1.5) + dither), 1. - normals.[k].Y)
        let triangles =
            [| for j in 0 .. n - 2 do
                 for i in 0 .. n - 2 do
                     let a = j * n + i
                     let b, c, d = a + 1, a + n, a + n + 1
                     yield! [ a; c; b; b; c; d ] |]
        (TriangleMesh.fromArrays positions normals uvs triangles true).toShape texture

    /// The terrain as a mesh shape centred on the origin (y up).
    let build (settings: Settings) = buildWith settings (height settings) (texture settings)
