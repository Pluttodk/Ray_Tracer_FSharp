#r "../RayTracer/bin/Release/net10.0/Basics.dll"
#load "../SceneFormat/SceneFormat.fs"

// Generates benchmarks/scenes/stress-hall.json: a deliberately heavy scene that
// stresses BOTH axes at once, which none of the existing benchmark scenes do.
//
//   geometry - a few million instanced triangles from the Stanford scans, far
//              past the 19k-60k of the authored scenes
//   shading  - area lights, rough metal, glass and glossy surfaces at depth,
//              so path length and lobe divergence matter too
//
// Placement is derived from each mesh's actual bounds via the project's own
// PLY parser rather than hand-tuned constants, so the layout stays correct if
// an asset is ever replaced.

open System
open System.IO
open Tracer.Basics
open Tracer.SceneFormat

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let scenePath = Path.Combine(root, "benchmarks/scenes/stress-hall.json")

/// Axis-aligned bounds of a PLY, used to normalize each asset to a common size.
let bounds (relative: string) =
    let full = Path.Combine(root, relative)
    let vertices, faces = PLYParser.parseIndexedPLY full
    let mutable lo = (infinity, infinity, infinity)
    let mutable hi = (-infinity, -infinity, -infinity)
    for v in vertices do
        let (lx, ly, lz) = lo
        let (hx, hy, hz) = hi
        lo <- (min lx v.x, min ly v.y, min lz v.z)
        hi <- (max hx v.x, max hy v.y, max hz v.z)
    lo, hi, faces.Length

/// Row-major affine: uniform scale, then rotation about Y, then translation.
/// Translation lives in the fourth column, matching the existing scene files.
let trs (scale: float) (yaw: float) (tx, ty, tz) =
    let c = cos yaw
    let s = sin yaw
    [| scale * c; 0.; scale * s; tx
       0.; scale; 0.; ty
       -scale * s; 0.; scale * c; tz
       0.; 0.; 0.; 1. |]

/// Scale and offset that put a mesh's base on y=0, centred on the origin, at a
/// requested height.
let fitting relative targetHeight =
    let (lx, ly, lz), (hx, hy, hz), triangles = bounds relative
    let height = max 1e-9 (hy - ly)
    let scale = targetHeight / height
    let centreX = (lx + hx) / 2.
    let centreZ = (lz + hz) / 2.
    scale, (centreX, ly, centreZ), triangles

let place relative targetHeight yaw (x, y, z) =
    let scale, (cx, cy, cz), _ = fitting relative targetHeight
    // Undo the mesh's own centre before applying the world placement.
    let c = cos yaw
    let s = sin yaw
    let ox = -scale * (c * cx + s * cz)
    let oy = -scale * cy
    let oz = -scale * (-s * cx + c * cz)
    trs scale yaw (x + ox, y + oy, z + oz)

let colour r g b : float array = [| r; g; b |]

/// `reflection` is the specular/reflection tint. Leaving it null makes the
/// scene format default it to WHITE, which puts a grey specular layer over the
/// diffuse colour and desaturates metals badly - a gold dragon renders grey.
/// Metals must therefore state their own reflection colour.
let materialWith id kind col reflection ambient diffuse specular exponent reflectivity gloss ior emission =
    { Id = id; Kind = kind; Colour = col
      AmbientColour = null; SpecularColour = null; ReflectionColour = reflection
      Ambient = ambient; Diffuse = diffuse; Specular = specular; Exponent = exponent
      Reflectivity = reflectivity; GlossExponent = gloss; Ior = ior
      Filter = colour 1. 1. 1.; Emission = emission; Texture = "" }

let material id kind col ambient diffuse specular exponent reflectivity gloss ior emission =
    materialWith id kind col null ambient diffuse specular exponent reflectivity gloss ior emission

let materials =
    [| material "floor" MaterialKind.Phong (colour 0.32 0.34 0.38) 0.05 0.7 0.2 24 0. 0 1.5 0.
       materialWith "gold" MaterialKind.Glossy (colour 1.0 0.78 0.34) (colour 1.0 0.80 0.36) 0.05 0.55 0.5 64 0.72 64 1.5 0.
       materialWith "copper" MaterialKind.Glossy (colour 0.95 0.54 0.40) (colour 0.96 0.58 0.44) 0.05 0.5 0.45 40 0.62 40 1.5 0.
       material "jade" MaterialKind.Phong (colour 0.24 0.62 0.44) 0.06 0.8 0.25 40 0. 0 1.5 0.
       material "ivory" MaterialKind.Matte (colour 0.86 0.83 0.74) 0.06 0.9 0. 0 0. 0 1.5 0.
       material "slate" MaterialKind.Matte (colour 0.28 0.30 0.33) 0.05 0.85 0. 0 0. 0 1.5 0.
       materialWith "chrome" MaterialKind.Mirror (colour 0.92 0.93 0.95) (colour 0.95 0.96 0.97) 0.04 0.25 0.6 80 0.9 0 1.5 0.
       material "glass" MaterialKind.Glass (colour 0.95 0.98 0.96) 0.02 0.1 0.4 90 0.1 0 1.52 0.
       material "lamp" MaterialKind.Emissive (colour 1.0 0.95 0.85) 0. 0. 0. 0 0. 0 1.5 3.2 |]

let dragonPly = "artifacts/scene-assets/stanford/dragon_recon/dragon_vrip.ply"
let meshes =
    [| { Id = "dragon"; Path = "../../" + dragonPly; Smooth = false; Closed = false }
       { Id = "armadillo"; Path = "../../ply/armadillo.ply"; Smooth = true; Closed = false }
       { Id = "horse"; Path = "../../ply/horse.ply"; Smooth = true; Closed = false }
       { Id = "bunny"; Path = "../../ply/bunny.ply"; Smooth = true; Closed = false }
       { Id = "porsche"; Path = "../../ply/porsche.ply"; Smooth = true; Closed = false }
       // Glass must be a closed, consistently oriented solid. The Stanford
       // scans are open surfaces, so the refracting objects are the generated
       // sphere instead - an open scan would not be valid glass.
       { Id = "sphere"; Path = "../../artifacts/scene-assets/meshes/sphere.ply"; Smooth = true; Closed = true }
       { Id = "slab"; Path = "../../artifacts/scene-assets/gold-dragon/staging-plane.ply"; Smooth = false; Closed = true } |]

let objects = ResizeArray<ObjectSpec>()
let subjects = ResizeArray<string>()
let add id mesh mat transform isSubject =
    objects.Add { Id = id; Mesh = mesh; Material = mat; Transform = transform }
    if isSubject then subjects.Add id

// Floor slab, scaled far past the visible area so it reads as an infinite plane.
add "floor" "slab" "floor" [| 4000.; 0.; 0.; 0.; 0.; 0.; 1.; -0.02; 0.; -4000.; 0.; 0.; 0.; 0.; 0.; 1. |] false

// Hero: the 871k-triangle dragon, centred.
add "dragon" "dragon" "gold" (place dragonPly 6.0 (Math.PI / 4.) (0., 0., 0.)) true

// Armadillos flanking, alternating metal and stone.
let armadilloSpots = [ -7.5, -3.0; 7.5, -3.0; -11.0, 4.0; 11.0, 4.0 ]
armadilloSpots |> List.iteri (fun i (x, z) ->
    let mat = if i % 2 = 0 then "copper" else "slate"
    add (sprintf "armadillo-%d" i) "armadillo" mat
        (place "ply/armadillo.ply" 5.0 (Math.PI + float i * 0.4) (x, 0., z)) true)

// Horses in a receding row, which is where instancing pays off.
for i in 0 .. 7 do
    let side = if i % 2 = 0 then -1.0 else 1.0
    let x = side * (14.0 + float (i / 2) * 3.0)
    let z = -8.0 + float (i / 2) * 7.0
    add (sprintf "horse-%d" i) "horse" (if i % 3 = 0 then "jade" else "ivory")
        (place "ply/horse.ply" 4.2 (side * Math.PI / 3.) (x, 0., z)) true

// Bunny grid in the foreground.
for i in 0 .. 15 do
    let col = i % 4
    let row = i / 4
    let x = -6.0 + float col * 4.0
    let z = 8.0 + float row * 3.5
    let mat = [| "ivory"; "jade"; "copper"; "slate" |].[(col + row) % 4]
    add (sprintf "bunny-%d" i) "bunny" mat
        (place "ply/bunny.ply" 2.2 (float i * 0.7) (x, 0., z)) true

// Chrome porsches along the sides.
for i in 0 .. 7 do
    let side = if i % 2 = 0 then -1.0 else 1.0
    let x = side * 19.0
    let z = -10.0 + float (i / 2) * 9.0
    add (sprintf "porsche-%d" i) "porsche" "chrome"
        (place "ply/porsche.ply" 2.0 (side * Math.PI / 2.) (x, 0., z)) true

// Floating glass spheres, so refraction and total internal reflection are
// exercised at depth.
for i in 0 .. 11 do
    let angle = float i / 12. * 2. * Math.PI
    let radius = 9.0 + 2.0 * float (i % 3)
    let x = radius * cos angle
    let z = radius * sin angle
    let y = 5.5 + 1.4 * float (i % 4)
    add (sprintf "glass-%d" i) "sphere" "glass"
        (place "artifacts/scene-assets/meshes/sphere.ply" 1.8 0. (x, y, z)) true

// Emission is front-face only, so a light rectangle that clips into frame
// shows up as a black quad. These sit well above the top of the view - at this
// camera the frame edge reaches roughly y=16 at the lights' depth - and their
// intensity accounts for the extra distance, since area-light sampling carries
// the 1/d^2 falloff.
let lights =
    [| { Id = "key"; Kind = LightKind.Rectangle; Position = [| -11.; 30.; 13. |]
         Direction = [| 0.35; -1.; -0.22 |]; Colour = colour 1.0 0.96 0.88; Intensity = 16.0; Size = [| 13.; 13. |] }
       { Id = "fill"; Kind = LightKind.Rectangle; Position = [| 15.; 26.; 18. |]
         Direction = [| -0.5; -1.; -0.28 |]; Colour = colour 0.72 0.82 1.0; Intensity = 8.0; Size = [| 11.; 11. |] }
       { Id = "rim"; Kind = LightKind.Point; Position = [| 0.; 9.; -22. |]
         Direction = [| 0.; 0.; 0. |]; Colour = colour 1.0 0.85 0.7; Intensity = 1.6; Size = [| 1.; 1. |] }
       { Id = "sky"; Kind = LightKind.Environment; Position = [| 0.; 0.; 0. |]
         Direction = [| 0.; 1.; 0. |]; Colour = colour 0.46 0.56 0.72; Intensity = 0.42; Size = [| 1000000.; 1000000. |] } |]

let scene =
    { SchemaVersion = 1
      Id = "stress-hall"
      Title = "Stress hall - multi-million-triangle path-tracing workload"
      Description =
        "Original composition instancing the Stanford scans already present in the repository. "
        + "Built to stress geometry and shading at the same time: a few million instanced triangles, "
        + "area lights, rough metal, glass and glossy surfaces at depth. Glass subjects are the generated "
        + "closed sphere; the Stanford scans are open surfaces and are never used as glass. "
        + "Stanford scans remain under Stanford's attributed noncommercial research terms and are not GPL-relicensed. "
        + "See https://graphics.stanford.edu/data/3Dscanrep/."
      Camera =
        // Wide enough to hold the full hall: the porsches sit at x = +-19, so a
        // narrower view silently crops a third of the geometry out of frame.
        { Position = [| 0.; 11.5; 44. |]; Target = [| 0.; 3.5; 1.0 |]; Up = [| 0.; 1.; 0. |]
          ViewDistance = 1.; ViewWidth = 2. * tan 0.42; ViewHeight = 2. * tan 0.42 * 0.75
          LensRadius = 0.; FocusDistance = 44. }
      AmbientColour = colour 0.5 0.56 0.66
      AmbientIntensity = 0.04
      MaxBounces = 8
      Materials = materials
      Meshes = meshes
      Objects = objects.ToArray()
      Lights = lights
      Subjects = subjects.ToArray() }

SceneFiles.save scenePath scene
printfn "Wrote %s" scenePath

// Report the actual instanced triangle count, which is the number that matters.
let triangleCounts =
    meshes
    |> Array.map (fun m ->
        let relative = m.Path.Replace("../../", "")
        let _, _, tris = bounds relative
        m.Id, tris)
    |> dict
let mutable total = 0
for o in objects do total <- total + triangleCounts.[o.Mesh]
printfn "objects=%d  subjects=%d  instanced triangles=%s"
    objects.Count subjects.Count (total.ToString("N0"))
