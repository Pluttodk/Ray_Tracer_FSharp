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

    // --- d: procedural surface ---
    /// Three octaves of value noise in [0, 1), centred on 0.5.
    let private fbm (x: float) (z: float) seed =
        (valueNoise x z seed * 4. + valueNoise (x * 2.1) (z * 2.1) (seed + 1) * 2. + valueNoise (x * 4.3) (z * 4.3) (seed + 2)) / 7.

    /// Procedural ground at a world position: noise-modulated rock strata, snow on high or gentle ground with an
    /// irregular edge, forest and meadow low down. Returns the colour and a specular coefficient (snow glints).
    /// `steepness` is 1 - normal.Y.
    let surfaceAt (settings: Settings) (x: float) (z: float) altitude steepness =
        let line = settings.SnowLine
        let seed = settings.Seed
        let broad = fbm (x / 90.) (z / 90.) (seed + 31)
        let medium = fbm (x / 24.) (z / 24.) (seed + 41)
        let fine = fbm (x / 5.) (z / 5.) (seed + 51)
        // Rock: tilted strata with wandering bands, two tones, and fine grain.
        let phase = altitude / 5. + 14. * (fbm (x / 70.) (z / 70.) (seed + 61) - 0.5) + 6. * (medium - 0.5) + 2. * (fine - 0.5)
        let band = 0.5 + 0.5 * sin (phase * 2. * Math.PI / 2.3) * (0.4 + 1.2 * (broad - 0.3))
        let rockWarm, rockCool = Colour(0.34, 0.28, 0.22), Colour(0.27, 0.245, 0.22)
        let rock = mix rockCool rockWarm (smoothstep 0.25 0.75 (0.35 * band + 0.65 * broad)) * (0.75 + 0.5 * fine)
        // Vegetation: forest darkens with altitude and thins into meadow; patchy, and desaturated far from the peaks.
        let patch = smoothstep 0.35 0.65 (fbm (x / 55.) (z / 55.) (seed + 71))
        let forest = mix (Colour(0.035, 0.085, 0.03)) (Colour(0.07, 0.12, 0.04)) patch * (0.75 + 0.5 * fine)
        let meadow = mix (Colour(0.2, 0.22, 0.085)) (Colour(0.27, 0.25, 0.11)) broad * (0.85 + 0.3 * fine)
        let green = mix meadow forest (1. - smoothstep (0.22 * line) (0.55 * line + 20. * (medium - 0.5)) altitude)
        let distance = smoothstep 500. 900. (sqrt (x * x + z * z))
        let grey = green.R * 0.3 + green.G * 0.59 + green.B * 0.11
        let green = mix green (Colour(grey, grey, grey) * 0.85) (0.45 * distance)
        let rocky = max (smoothstep 0.3 0.5 (steepness + 0.06 * (medium - 0.5))) (smoothstep (0.62 * line) (0.9 * line) (altitude + 25. * (broad - 0.5)))
        let ground = mix green rock rocky
        // Snow: the line wanders with noise and the edge is narrow; steep faces shed it, slightly less up high.
        let snowLine = line * (0.8 + 0.22 * (broad - 0.5) + 0.12 * (medium - 0.5))
        let slopeLimit = 0.46 + 0.3 * (medium - 0.5) + 0.15 * (altitude / line - 1.)
        let cover = smoothstep (snowLine - 0.025 * line) (snowLine + 0.025 * line) altitude
        let shed = 1. - smoothstep (slopeLimit - 0.04) (slopeLimit + 0.04) (steepness + 0.08 * (fine - 0.5))
        let snowy = cover * shed
        let snow = Colour(0.86, 0.88, 0.93) * (0.93 + 0.14 * fine)
        mix ground snow snowy, 0.02 + 0.2 * snowy

    /// Bilinear sample of a per-vertex grid at a world position.
    let private bilinear (settings: Settings) (get: int -> int -> float) (x: float) (z: float) =
        let n = settings.Resolution
        let step = settings.Size / float (n - 1)
        let gx = max 0. (min (float n - 1.001) ((x + settings.Size / 2.) / step))
        let gz = max 0. (min (float n - 1.001) ((z + settings.Size / 2.) / step))
        let i, j = int gx, int gz
        let fx, fz = gx - float i, gz - float j
        let top = get i j * (1. - fx) + get (i + 1) j * fx
        let bottom = get i (j + 1) * (1. - fx) + get (i + 1) (j + 1) * fx
        top * (1. - fz) + bottom * fz

    /// Distant ranges seen through haze: pale, blue-shifted and low in contrast, so they recede.
    let hazeTexture (settings: Settings) (haze: Colour) =
        let tones =
            Array2D.init 32 32 (fun i j ->
                let altitude = (float i + 0.5) / 32. * settings.SnowLine * 1.5
                let slope = (float j + 0.5) / 32.
                let c = colourAt settings altitude slope * 0.6 + haze * 0.4
                MatteMaterial(c, 0.35, c, 0.8) :> Material)
        mkTexture (fun u v -> tones.[max 0 (min 31 (int (u * 32.))), max 0 (min 31 (int (v * 32.)))])
        |> markOpaque

    // --- d: shared grid builder, smooth normals, position-keyed procedural texture ---
    /// Heights sampled on the vertex grid, indexed [i, j] with i along x and j along z.
    let heightGrid (settings: Settings) (heightAt: float -> float -> float) =
        let n = settings.Resolution
        if n < 2 then invalidArg (nameof settings) "The terrain needs at least 2 vertices per side."
        let step = settings.Size / float (n - 1)
        let coordinate i = -settings.Size / 2. + float i * step
        Array2D.init n n (fun i j -> heightAt (coordinate i) (coordinate j))

    /// Smooth unit vertex normals from central differences of the grid (one-sided at the border), indexed j * n + i.
    /// Vertices shared by neighbouring triangles carry one normal, so shading is continuous across the mesh.
    let gridNormals (step: float) (heights: float[,]) =
        let n = Array2D.length1 heights
        let at i j = heights.[max 0 (min (n - 1) i), max 0 (min (n - 1) j)]
        Array.init (n * n) (fun k ->
            let i, j = k % n, k / n
            Vector(at (i - 1) j - at (i + 1) j, 2. * step, at i (j - 1) - at i (j + 1)).Normalise)

    let private assemble (settings: Settings) (heights: float[,]) (normals: Vector[]) (uvOf: Point -> Vector -> float * float) (texture: Texture) =
        let n = settings.Resolution
        let step = settings.Size / float (n - 1)
        let coordinate i = -settings.Size / 2. + float i * step
        let positions = Array.init (n * n) (fun k -> let i, j = k % n, k / n in Point(coordinate i, heights.[i, j], coordinate j))
        let uvs = Array.init (n * n) (fun k -> uvOf positions.[k] normals.[k])
        let triangles =
            [| for j in 0 .. n - 2 do
                 for i in 0 .. n - 2 do
                     let a = j * n + i
                     let b, c, d = a + 1, a + n, a + n + 1
                     // Split each cell along the diagonal that follows the terrain's curvature, avoiding zig-zag ridges.
                     if abs (heights.[i, j] - heights.[i + 1, j + 1]) < abs (heights.[i + 1, j] - heights.[i, j + 1])
                     then yield! [ a; c; d; a; d; b ]
                     else yield! [ a; c; b; b; c; d ] |]
        (TriangleMesh.fromArrays positions normals uvs triangles true).toShape texture

    /// A terrain mesh over [-Size/2, Size/2]^2 for any height function, shaded with `texture` whose UVs are
    /// (altitude / (1.5 x snow line), steepness).
    let buildWith (settings: Settings) (heightAt: float -> float -> float) (texture: Texture) =
        let heights = heightGrid settings heightAt
        let normals = gridNormals (settings.Size / float (settings.Resolution - 1)) heights
        let uvOf (p: Point) (normal: Vector) =
            let dither = 0.06 * (valueNoise (p.X / 7.) (p.Z / 7.) 99 - 0.5)
            (p.Y / (settings.SnowLine * 1.5) + dither), 1. - normal.Y
        assemble settings heights normals uvOf texture

    /// The detailed terrain: UVs are the normalised world position, and the texture rebuilds the ground from the
    /// interpolated height and normal grids plus procedural noise, so detail is finer than the mesh.
    let private detailed (settings: Settings) =
        let heights = heightGrid settings (height settings)
        let step = settings.Size / float (settings.Resolution - 1)
        let normals = gridNormals step heights
        let n = settings.Resolution
        let altitudeAt x z = bilinear settings (fun i j -> heights.[i, j]) x z
        let normalY x z = bilinear settings (fun i j -> normals.[j * n + i].Y) x z
        let texture =
            mkTexture (fun u v ->
                let x, z = (u - 0.5) * settings.Size, (v - 0.5) * settings.Size
                let c, specular = surfaceAt settings x z (altitudeAt x z) (1. - normalY x z)
                PhongMaterial(c, 0.3, c, 0.85, Colour(1., 1., 1.), specular, 24) :> Material)
            |> markOpaque
        assemble settings heights normals (fun p _ -> p.X / settings.Size + 0.5, p.Z / settings.Size + 0.5) texture

    /// The terrain as a mesh shape centred on the origin (y up).
    let build (settings: Settings) = detailed settings
