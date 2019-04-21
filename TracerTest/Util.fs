namespace TracerTestSuite

open System.IO
open Tracer.API
open System
open System.Drawing
open System.Threading

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

  let private source_path = "../../.."
  let private result_path = source_path + "/result"
  let private timings = result_path + "/runtime.csv"

  let mutable private timings_wr = null

  let init () = 
    Directory.CreateDirectory result_path |> ignore
    timings_wr <- new StreamWriter(timings, false)
    timings_wr.WriteLine("test name, construction, rendering, total")


  let finalize () = timings_wr.Close()

  let render (renderIt : unit -> Render) : unit =
    let render = renderIt ()
    renderToScreen render.scene render.camera

  let mutable private timeout = -1

  let setTimeout (seconds : int) : unit =
    timeout <- seconds

  let private runWithTimout (work : unit -> unit) (failure : unit -> unit) (success : unit -> unit) : unit =
    if timeout < 0
    then work (); success ()
    else
    let th = new Thread(new ThreadStart(work))
    th.Start()
    if th.Join(new TimeSpan(0, 0, timeout))
    then success ()
    else th.Abort(); failure ()

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
        let path = if tgt.group = "" then result_path else result_path + "/" + tgt.group
        Directory.CreateDirectory path |> ignore
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let s = path + "/" + tgt.name + ".png"
        printf "Rendering file %s" s;
        runWithTimout 
          (fun () -> renderToFile render.scene render.camera s) 
          (fun () -> 
            stopWatch.Stop();
            printfn " timeout!"
            timings_wr.WriteLine("{0}/{1}, {2}, timeout ({3}), timeout", tgt.group, tgt.name, timeConstruct, timeout)
            timings_wr.Flush())
          (fun () ->
            stopWatch.Stop();
            let timeRender = stopWatch.Elapsed.TotalMilliseconds / 1000.0
            printfn " in %f seconds" timeRender;
            timings_wr.WriteLine("{0}/{1}, {2}, {3}, {4}", tgt.group, tgt.name, timeConstruct, timeRender, timeConstruct+timeRender)
            timings_wr.Flush())
    with | e -> 
      printfn "rendering of %s/%s failed: %s" tgt.group tgt.name (e.ToString())
      if not toScreen then
        timings_wr.WriteLine("{0}/{1}, crashed, {2}", tgt.group, tgt.name, e.ToString())
        timings_wr.Flush()    

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



  let mkReflectiveTextureFromFile kr (tr : float -> float -> float * float) (file : string) =
    let img = new Bitmap(file)
    let width = img.Width - 1
    let height = img.Height - 1
    let widthf = float width
    let heightf = float height
    let texture x y =
      let (x', y') = tr x y
      let x'', y'' = int (widthf * x'), int (heightf * y')
      let c = fromColor (lock img (fun () -> img.GetPixel(x'',y'')))
      mkMatteReflective c (1.0 - kr) c kr
    mkTexture texture

  let mkTextureFromFile (tr : float -> float -> float * float) (file : string) =
    let img = new Bitmap(file)
    let width = img.Width - 1
    let height = img.Height - 1
    let widthf = float width
    let heightf = float height
    let texture x y =
      let (x', y') = tr x y
      let x'', y'' = int (widthf * x'), int (heightf * y')
      let c = lock img (fun () -> img.GetPixel(x'',y''))
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