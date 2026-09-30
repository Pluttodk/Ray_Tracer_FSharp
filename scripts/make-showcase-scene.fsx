#r "../RayTracer/bin/Release/net10.0/Basics.dll"
#load "../SceneFormat/SceneFormat.fs"

// Generates benchmarks/scenes/showcase-gallery.json: a deliberately heavy scene that
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
let scenePath = Path.Combine(root, "benchmarks/scenes/showcase-gallery.json")

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
/// scene format default it to WHITE, which lays a grey specular over the
/// diffuse colour and desaturates metals badly. Metals state their own.
let materialWith id kind col reflection ambient diffuse specular exponent reflectivity gloss ior emission =
    { Id = id; Kind = kind; Colour = col
      AmbientColour = null; SpecularColour = null; ReflectionColour = reflection
      Ambient = ambient; Diffuse = diffuse; Specular = specular; Exponent = exponent
      Reflectivity = reflectivity; GlossExponent = gloss; Ior = ior
      Filter = colour 1. 1. 1.; Emission = emission; Texture = "" }

let material id kind col ambient diffuse specular exponent reflectivity gloss ior emission =
    materialWith id kind col null ambient diffuse specular exponent reflectivity gloss ior emission

let materials =
    [| material "floor" MaterialKind.Phong (colour 0.30 0.31 0.34) 0.04 0.72 0.18 28 0. 0 1.5 0.
       material "marble" MaterialKind.Phong (colour 0.88 0.86 0.80) 0.05 0.85 0.22 44 0. 0 1.5 0.
       material "ivory" MaterialKind.Matte (colour 0.86 0.82 0.72) 0.05 0.9 0. 0 0. 0 1.5 0.
       material "jade" MaterialKind.Phong (colour 0.16 0.55 0.38) 0.05 0.8 0.3 52 0. 0 1.5 0.
       material "slate" MaterialKind.Matte (colour 0.26 0.28 0.31) 0.05 0.85 0. 0 0. 0 1.5 0.
       materialWith "gold" MaterialKind.Glossy (colour 1.0 0.78 0.34) (colour 1.0 0.80 0.36) 0.04 0.5 0.5 72 0.78 72 1.5 0.
       materialWith "copper" MaterialKind.Glossy (colour 0.95 0.54 0.40) (colour 0.96 0.58 0.44) 0.04 0.5 0.45 48 0.66 48 1.5 0.
       materialWith "steel" MaterialKind.Glossy (colour 0.62 0.65 0.70) (colour 0.70 0.73 0.78) 0.04 0.4 0.5 90 0.72 90 1.5 0.
       materialWith "chrome" MaterialKind.Mirror (colour 0.92 0.93 0.95) (colour 0.95 0.96 0.97) 0.03 0.22 0.6 96 0.92 0 1.5 0.
       material "glass" MaterialKind.Glass (colour 0.96 0.98 0.97) 0.02 0.1 0.4 96 0.1 0 1.52 0. |]

let stanford = "artifacts/scene-assets/stanford"
let meshes =
    [| { Id = "statuette"; Path = "../../" + stanford + "/xyzrgb_statuette.ply"; Smooth = true; Closed = false }
       { Id = "xyzdragon"; Path = "../../" + stanford + "/xyzrgb_dragon.ply"; Smooth = true; Closed = false }
       { Id = "buddha"; Path = "../../" + stanford + "/happy_recon/happy_vrip.ply"; Smooth = true; Closed = false }
       { Id = "dragon"; Path = "../../" + stanford + "/dragon_recon/dragon_vrip.ply"; Smooth = false; Closed = false }
       { Id = "armadillo"; Path = "../../ply/armadillo.ply"; Smooth = true; Closed = false }
       { Id = "horse"; Path = "../../ply/horse.ply"; Smooth = true; Closed = false }
       { Id = "bunny"; Path = "../../ply/bunny.ply"; Smooth = true; Closed = false }
       // Glass must be a closed, consistently oriented solid; the Stanford scans
       // are open surfaces and are never used as glass.
       { Id = "sphere"; Path = "../../artifacts/scene-assets/meshes/sphere.ply"; Smooth = true; Closed = true }
       { Id = "slab"; Path = "../../artifacts/scene-assets/gold-dragon/staging-plane.ply"; Smooth = false; Closed = true } |]

let objects = ResizeArray<ObjectSpec>()
let subjects = ResizeArray<string>()
let add id mesh mat transform isSubject =
    objects.Add { Id = id; Mesh = mesh; Material = mat; Transform = transform }
    if isSubject then subjects.Add id

let statuettePly = stanford + "/xyzrgb_statuette.ply"
let xyzDragonPly = stanford + "/xyzrgb_dragon.ply"
let buddhaPly = stanford + "/happy_recon/happy_vrip.ply"
let dragonPly = stanford + "/dragon_recon/dragon_vrip.ply"

// Floor, scaled far past the visible area so it reads as an infinite plane.
add "floor" "slab" "floor" [| 4000.; 0.; 0.; 0.; 0.; 0.; 1.; -0.02; 0.; -4000.; 0.; 0.; 0.; 0.; 0.; 1. |] false

// Centrepiece: the 10M-triangle Thai statuette, on axis and tallest.
add "statuette" "statuette" "marble" (place statuettePly 13.0 (Math.PI * 0.08) (0., 0., -4.)) true

// The 7.2M-triangle XYZ dragon in gold, turned towards the camera.
add "xyzdragon" "xyzdragon" "gold" (place xyzDragonPly 7.0 (-Math.PI * 0.30) (-11.5, 0., 3.0)) true

// Happy Buddha in jade, mirroring it.
add "buddha" "buddha" "jade" (place buddhaPly 7.5 (Math.PI * 0.22) (11.0, 0., 2.5)) true

// The original Stanford dragon in copper, small and forward.
add "dragon" "dragon" "copper" (place dragonPly 3.4 (Math.PI * 0.75) (-4.5, 0., 9.5)) true

// Armadillos in steel, flanking the statuette.
[ -18.0, -2.0, 0.5; 18.0, -3.0, -0.4 ] |> List.iteri (fun i (x, z, yaw) ->
    add (sprintf "armadillo-%d" i) "armadillo" "steel"
        (place "ply/armadillo.ply" 6.0 (Math.PI + yaw) (x, 0., z)) true)

// A receding colonnade of horses, where instancing pays off.
for i in 0 .. 9 do
    let side = if i % 2 = 0 then -1.0 else 1.0
    let step = float (i / 2)
    add (sprintf "horse-%d" i) "horse" (if i % 3 = 0 then "ivory" else "slate")
        (place "ply/horse.ply" 4.6 (side * Math.PI / 2.4) (side * (19.0 + step * 1.2), 0., -9.0 + step * 7.0)) true

// Bunny grid in the foreground.
for i in 0 .. 17 do
    let col = i % 6
    let row = i / 6
    let x = -12.0 + float col * 4.8
    let z = 11.0 + float row * 3.6
    let mat = [| "ivory"; "jade"; "copper"; "slate"; "marble"; "steel" |].[(col + row) % 6]
    add (sprintf "bunny-%d" i) "bunny" mat
        (place "ply/bunny.ply" 2.4 (float i * 0.7) (x, 0., z)) true

// Chrome spheres and glass spheres, so mirror reflection, refraction and total
// internal reflection are all exercised at depth.
for i in 0 .. 13 do
    let angle = float i / 14. * 2. * Math.PI
    let radius = 15.0 + 3.0 * float (i % 3)
    let x = radius * cos angle
    let z = radius * sin angle - 1.0
    let y = 7.0 + 1.8 * float (i % 4)
    let mat = if i % 2 = 0 then "glass" else "chrome"
    add (sprintf "orb-%d" i) "sphere" mat
        (place "artifacts/scene-assets/meshes/sphere.ply" 2.2 0. (x, y, z)) true

// Emission is front-face only, so a light rectangle that clips into frame shows
// up as a black quad. These sit well above the top of the view.
let lights =
    [| { Id = "key"; Kind = LightKind.Rectangle; Position = [| -14.; 42.; 16. |]
         Direction = [| 0.32; -1.; -0.20 |]; Colour = colour 1.0 0.96 0.88; Intensity = 30.0; Size = [| 18.; 18. |] }
       { Id = "fill"; Kind = LightKind.Rectangle; Position = [| 19.; 34.; 22. |]
         Direction = [| -0.45; -1.; -0.26 |]; Colour = colour 0.74 0.83 1.0; Intensity = 14.0; Size = [| 15.; 15. |] }
       { Id = "rim"; Kind = LightKind.Point; Position = [| 0.; 13.; -30. |]
         Direction = [| 0.; 0.; 0. |]; Colour = colour 1.0 0.86 0.72; Intensity = 2.4; Size = [| 1.; 1. |] }
       { Id = "sky"; Kind = LightKind.Environment; Position = [| 0.; 0.; 0. |]
         Direction = [| 0.; 1.; 0. |]; Colour = colour 0.44 0.55 0.74; Intensity = 0.40; Size = [| 1000000.; 1000000. |] } |]

let scene =
    { SchemaVersion = 1
      Id = "showcase-gallery"
      Title = "Showcase gallery - high-resolution Stanford scans, path traced"
      Description =
        "Hero render. Thai Statuette (10.0M triangles), XYZ RGB Dragon (7.2M) and Happy Buddha (1.09M) "
        + "from the Stanford Large Geometric Models Archive, with the Stanford Dragon, Armadillo, Horse and "
        + "Bunny, under two rectangular area lights and a sky environment. Rough metal, rough dielectric, "
        + "mirror and glass. Glass subjects are the generated closed sphere; the Stanford scans are open "
        + "surfaces and are never used as glass. The scans remain under Stanford's attributed noncommercial "
        + "research terms and are not GPL-relicensed. See https://graphics.stanford.edu/data/3Dscanrep/."
      Camera =
        // Framed so the subjects fill the frame: a wider view left a third of
        // the image as empty sky and shrank the statuette to a detail.
        { Position = [| 0.; 10.5; 42. |]; Target = [| 0.; 6.4; -2. |]; Up = [| 0.; 1.; 0. |]
          ViewDistance = 1.; ViewWidth = 2. * tan 0.40; ViewHeight = 2. * tan 0.40 * 0.625
          LensRadius = 0.; FocusDistance = 44. }
      AmbientColour = colour 0.5 0.56 0.66
      AmbientIntensity = 0.03
      MaxBounces = 8
      Materials = materials
      Meshes = meshes
      Objects = objects.ToArray()
      Lights = lights
      Subjects = subjects.ToArray() }

SceneFiles.save scenePath scene
printfn "Wrote %s" scenePath

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
