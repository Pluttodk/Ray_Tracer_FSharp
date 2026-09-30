open TracerTestSuite
open Tracer.API
open System
open System.Diagnostics
open System.IO

let allTargets : Target list =
  List.concat 
    [
     Material.renderRegular;
     Material.renderMulti;
     Material.renderHigh;
     ThinLens.render;
     AreaLights.render;
     Shapes.render;
     Texture.render;
     Transparency.render;
     AffineTransformations.render true;
     AffineTransformations.render false;
     ImplicitSurfaces.render;
     Meshes.render Tracer.API.Acceleration.KDTree
     Light.render;
     CSG.render;
     AmbientOcclusion.render [1;2;4;8;16]
      //The test groups below is only needed for teams of 7 students.
      //Teams of 6 students can uncomment the lines below.
     Meshes.render Tracer.API.Acceleration.RegularGrid;
     Meshes.render Tracer.API.Acceleration.BVH;
     ]


let renderAll (toScreen : bool) : unit = 
  List.iter (Util.renderTarget toScreen) allTargets
let renderTests (toScreen : bool) (group : string) (tests : string list) : unit = 
  Util.renderTests toScreen allTargets group tests
let renderGroups (toScreen : bool) (groups : string list) : unit =
  Util.renderGroups toScreen allTargets groups



let smokeScene () =
    let white = mkColour 1. 1. 1.
    let red = mkColour 0.8 0.055 0.025
    let grey = mkColour 0.22 0.25 0.3
    let redMaterial = mkPhongMaterial red 0.12 red 0.8 white 0.35 48
    let mirror = mkMatteReflectiveMaterial grey 0.02 grey 0.08 white 0.88
    let floorMaterial = mkMatteReflectiveMaterial grey 0.1 grey 0.72 white 0.12
    let floorTexture = mkMatTexture floorMaterial
    let floor = mkBox (mkPoint -12. -0.2 -12.) (mkPoint 12. 0. 12.) floorTexture floorTexture floorTexture floorTexture floorTexture floorTexture
    let shapes =
        [ floor
          mkSphere (mkPoint -1.1 1. 0.) 1. (mkMatTexture redMaterial)
          mkSphere (mkPoint 1.15 0.85 0.2) 0.85 (mkMatTexture mirror)
          mkSphere (mkPoint 0.1 3.0 -1.2) 0.25 (mkMatTexture (mkEmissive (mkColour 0.1 0.9 0.85) 1.)) ]
    let lights =
        [ mkLight (mkPoint -3. 6. 5.) white 0.85
          mkLight (mkPoint 4. 3. 1.) (mkColour 0.55 0.7 1.) 0.35
          mkEnvironmentLight 10000. (mkMatTexture (mkEmissive (mkColour 0.12 0.18 0.28) 0.25)) (mkRegularSampler 1) ]
    { scene = mkScene shapes lights (mkAmbientLight white 0.3) 3
      camera = mkPinholeCamera (mkPoint 5. 3.5 7.) (mkPoint 0. 1.1 0.) (mkVector 0. 1. 0.) 4. 3.4 3.4 256 256 (mkRegularSampler 2) }

let renderDirect path createScene =
    let path = Path.GetFullPath path
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    let watch = Stopwatch.StartNew()
    let source: TracerTestSuite.Render = createScene()
    let loadMs = watch.Elapsed.TotalMilliseconds
    let options =
        { Tracer.Basics.RenderOptions.Default with
            Acceleration = Some Tracer.Basics.Acceleration.Acceleration.FlatBVH }
    let renderer = Tracer.Basics.Render.Render(source.scene, source.camera, options)
    let film = renderer.RenderLinear
    use image = film.ToImage options.Transfer
    image.SavePng path
    printfn "Rendered %dx%d: %s" film.Width film.Height path
    printfn "Scene %.1f ms; acceleration %.1f ms; tracing %.1f ms." loadMs film.BuildMilliseconds film.TraceMilliseconds

let usage () =
    printfn "Headless examples: --smoke [output.png] | --gold-dragon-preview [output.png] | --gold-dragon [output.png]"
    printfn "Historical suite: --list | --group <group> | --test <group> <name> | --all"
    printfn "Gold Dragon requires: dotnet fsi scripts/prepare-gold-dragon.fsx"
    printfn "The original Gold Dragon uses 1024x768, 16 camera/light/glossy samples, and two secondary bounces."

[<EntryPoint>]
let main argv =
    try
        match argv with
        | [||] | [| "--help" |] -> usage()
        | [| "--smoke" |] -> renderDirect "artifacts/smoke/cpu.png" smokeScene
        | [| "--smoke"; path |] -> renderDirect path smokeScene
        | [| "--gold-dragon-preview" |] -> renderDirect "artifacts/gold-dragon/preview.png" (Meshes.renderGoldDragonAt 1 256 192)
        | [| "--gold-dragon-preview"; path |] -> renderDirect path (Meshes.renderGoldDragonAt 1 256 192)
        | [| "--gold-dragon" |] -> renderDirect "artifacts/gold-dragon/original-settings.png" (Meshes.renderGoldDragon 4)
        | [| "--gold-dragon"; path |] -> renderDirect path (Meshes.renderGoldDragon 4)
        | [| "--list" |] -> allTargets |> List.iter (fun target -> printfn "%s / %s" target.group target.name)
        | [| "--all" |] | [| "--group"; _ |] | [| "--test"; _; _ |] ->
            Util.init()
            try
                match argv with
                | [| "--all" |] -> renderAll false
                | [| "--group"; group |] -> renderGroups false [group]
                | [| "--test"; group; name |] -> renderTests false group [name]
                | _ -> invalidOp "Invalid historical suite selection."
            finally Util.finalize()
        | _ -> invalidArg (nameof argv) "Unknown arguments. Use --help."
        0
    with error ->
        eprintfn "%s" (error.ToString())
        1
