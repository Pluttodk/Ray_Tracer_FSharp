namespace TracerTestSuite

open System.IO
open Tracer.API
open System
open System.Drawing
open System.Globalization
open Tracer.Imaging

type Render = 
  { scene : scene;
    camera : camera}

type Target =
  { render : unit -> Render;
    group : string;
    name : string }

module Util =
  let mkTarget (group : string) (render : unit -> Render, name : string) : Target = 
    {render = render; group = group; name = name}



  let degrees_to_radians (d : float) = d * Math.PI / 180.0

  let private configuredPath variable fallback =
    match Environment.GetEnvironmentVariable variable with
    | null | "" -> Path.GetFullPath fallback
    | value -> Path.GetFullPath value

  let private source_path =
    configuredPath "RAYTRACER_ASSET_ROOT" (Path.Combine(__SOURCE_DIRECTORY__, ".."))
  let private result_path =
    configuredPath "RAYTRACER_OUTPUT_ROOT" (Path.Combine(source_path, "result"))
  let private timings = Path.Combine(result_path, "runtime.csv")

  let resolveAssetPath (file: string) =
    if Path.IsPathRooted file then Path.GetFullPath file
    else
      let rec withoutParentPrefix (path: string) =
        if path.StartsWith("../", StringComparison.Ordinal) then withoutParentPrefix (path.Substring 3)
        elif path.StartsWith("./", StringComparison.Ordinal) then withoutParentPrefix (path.Substring 2)
        else path
      let relative = withoutParentPrefix (file.Replace('\\', '/'))
      let sourceRelative = Path.GetFullPath(Path.Combine(source_path, relative))
      if File.Exists sourceRelative then sourceRelative
      elif File.Exists file then Path.GetFullPath file
      else raise (FileNotFoundException("Texture asset was not found. Set RAYTRACER_ASSET_ROOT to the asset directory.", sourceRelative))

  let mutable private timings_wr: StreamWriter option = None

  let private writeTiming fields =
    match timings_wr with
    | None -> ()
    | Some writer ->
      let escaped = fields |> List.map (fun (value: string) -> "\"" + value.Replace("\"", "\"\"") + "\"")
      writer.WriteLine(String.concat "," escaped)
      writer.Flush()

  let private seconds (value: float) = value.ToString("R", CultureInfo.InvariantCulture)

  let init () = 
    timings_wr |> Option.iter (fun writer -> writer.Dispose())
    Directory.CreateDirectory result_path |> ignore
    timings_wr <- Some (new StreamWriter(timings, false))
    writeTiming ["test name"; "construction"; "rendering"; "total"]


  let finalize () =
    timings_wr |> Option.iter (fun writer -> writer.Dispose())
    timings_wr <- None

  let render (renderIt : unit -> Render) : unit =
    let render = renderIt ()
    renderToScreen render.scene render.camera

  let setTimeout (seconds : int) : unit =
    if seconds >= 0 then
      raise (NotSupportedException("In-process example timeouts are unsafe. Use the benchmark CLI's process-isolated timeout option."))

  let renderTarget (toScreen : bool) (tgt : Target) : unit =
    try 
      let stopWatch = System.Diagnostics.Stopwatch.StartNew();
      let render = tgt.render();
      stopWatch.Stop();
      let timeConstruct = stopWatch.Elapsed.TotalMilliseconds / 1000.0
      if toScreen then 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        renderToScreen render.scene render.camera
        let timeRender = stopWatch.Elapsed.TotalMilliseconds / 1000.0
        printfn "Image rendered in %f seconds" timeRender
      else 
        let path = if tgt.group = "" then result_path else Path.Combine(result_path, tgt.group)
        Directory.CreateDirectory path |> ignore
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let s = Path.Combine(path, tgt.name + ".png")
        printf "Rendering file %s" s;
        renderToFile render.scene render.camera s
        stopWatch.Stop()
        let timeRender = stopWatch.Elapsed.TotalSeconds
        printfn " in %f seconds" timeRender
        writeTiming [tgt.group + "/" + tgt.name; seconds timeConstruct; seconds timeRender; seconds (timeConstruct + timeRender)]
    with | e -> 
      eprintfn "rendering of %s/%s failed: %s" tgt.group tgt.name (e.ToString())
      if not toScreen then
        writeTiming [tgt.group + "/" + tgt.name; "crashed"; e.ToString(); "crashed"]
      reraise()

  let renderGroups (toScreen : bool) (targets : Target list) (groups : string list) : unit =
    for group in groups do
      match List.filter (fun tgt -> tgt.group = group) targets with
      | [] -> failwith ("cannot find group " + group)
      | tgts -> List.iter (renderTarget toScreen) tgts

  let renderTests (toScreen : bool) (targets : Target list) (group : string) (tests : string list) : unit =
    match List.filter (fun tgt -> tgt.group = group) targets with
    | [] -> failwith ("cannot find group " + group)
    | tgts -> 
      for name in tests do
        match List.tryFind (fun tgt -> tgt.name = name) tgts with
        | None -> failwith ("cannot find test " + name + " in group " + group)
        | Some tgt -> renderTarget toScreen tgt


  let mkMatte c k = mkMatteMaterial c k c k
  let mkPhong cd kd ks e = mkPhongMaterial cd kd cd kd cd ks e
  let mkMatteReflective cd kd cr kr = mkMatteReflectiveMaterial cd kd cd kd cr kr
  let mkPhongReflective cd kd cr kr ks e = mkPhongReflectiveMaterial cd kd cd kd cr kr cr ks e



  let private loadTexturePixels file =
    use image = RgbImage.Load(resolveAssetPath file)
    let width, height = image.Width, image.Height
    let pixels = Array.copy image.Pixels
    let getPixel x y =
      if x < 0 || x >= width || y < 0 || y >= height then
        invalidArg "coordinates" "Texture coordinates lie outside the image."
      let offset = (y * width + x) * 3
      Color.FromArgb(int pixels.[offset], int pixels.[offset + 1], int pixels.[offset + 2])
    width - 1, height - 1, getPixel

  let mkReflectiveTextureFromFile kr (tr : float -> float -> float * float) (file : string) =
    let width, height, getPixel = loadTexturePixels file
    let widthf = float width
    let heightf = float height
    let texture x y =
      let (x', y') = tr x y
      let x'', y'' = int (widthf * x'), int (heightf * y')
      let c = fromColor (getPixel x'' y'')
      mkMatteReflective c (1.0 - kr) c kr
    mkTexture texture

  let mkTextureFromFile (tr : float -> float -> float * float) (file : string) =
    let width, height, getPixel = loadTexturePixels file
    let widthf = float width
    let heightf = float height
    let texture x y =
      let (x', y') = tr x y
      let x'', y'' = int (widthf * x'), int (heightf * y')
      let c = getPixel x'' y''
      mkMatte (fromColor c) 1.0
    mkTexture texture


  let mkMonochrome f = mkColour f f f
  let mkMatteMonochrome f ka kd = mkMatTexture (mkMatteMaterial (mkMonochrome f) ka (mkMonochrome f) kd)

  let mkTexturedBox p1 p2 t = mkBox p1 p2 t t t t t t
  let mkTexturedCylinder p r h t = mkSolidCylinder p r h t t t

  let mkGridTexture square_size outline_size m1 m2 m3 =
      let outline_lower = outline_size / (2.0 * square_size)
      let outline_upper = 1.0 - outline_lower
      let inline is_outline (a : float) = 
         let adec = a % 1.0
         adec > outline_upper || adec < outline_lower
      let f = 
         fun (x : float) (y : float) -> 
              let xsquare = ((Math.Abs x) / square_size)
              let ysquare = ((Math.Abs y) / square_size)
              if is_outline xsquare || is_outline ysquare then 
                  m3
              else if (x < 0.0) <> (y > 0.0) then
                  if (int)(Math.Floor xsquare + Math.Floor ysquare) % 2 = 0 then
                      m1
                  else
                      m2
              else if (int)(Math.Floor xsquare + Math.Floor ysquare) % 2 = 0 then
                  m2
              else
                  m1

      mkTexture f

  let mkCheckeredTexture square_size m1 m2 = mkGridTexture square_size 0.0 m1 m2 m2