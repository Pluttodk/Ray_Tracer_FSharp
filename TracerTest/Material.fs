namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module Material =

  let ccylinder p r h t = mkSolidCylinder p r h t t t

  let grid c1 c2 x y =
      let abs' s f = if f < 0.0 then 1.0 - (f * s) else f * s
      if (int (abs' 1.0 x) + int (abs' 1.0 y)) % 2 = 0
      then c1
      else c2

       
  let renderScenePointF ptex btex s1tex s2tex s3tex ctex al lights sampler scale =
    let p = transform (mkPlane ptex) (rotateX (System.Math.PI/2.0)) in
    let b = mkTexturedBox (mkPoint -35.0 0.0 -110.0) (mkPoint -25.0 60.0 65.0) btex in
    let c = mkTexturedCylinder (mkPoint 0.0 42.5 0.0) 22.0 85.0 ctex in
    let s1 = mkSphere (mkPoint -7.0 15.0 42.0) 16.0 s1tex in
    let s2 = mkSphere (mkPoint 34.0 12.0 13.0) 12.0 s2tex in
    let s3 = mkSphere (mkPoint 38.0 20.0 -24.0) 20.0 s3tex in
    { camera = mkPinholeCamera (mkPoint (scale * 100.0) (scale * 45.0) (scale * 100.0)) (mkPoint -10.0 35.0 0.0) (mkVector 0.0 1.0 0.0) 25.0 40.0 30.0 1024 768 sampler;
      scene = mkScene [p; b; c; s1; s2; s3] lights al 5}

 // let mkSample sampler =  mkMultiJitteredSampler sampler 83
  let elight c i sampler = 
      mkEnvironmentLight 1000000.0 (mkMatTexture (mkEmissive c i)) sampler
  let ambientLight () = mkAmbientLight (fromColor Color.White) 0.1
  let ambientOccluder c ai amin sampler = mkAmbientOccluder c ai amin sampler

  let cwhite = mkColour 1.0 1.0 1.0
  let blue = mkColour 0.4 0.5 1.0
  let red = mkColour 0.75 0.0 0.0
  let orange = mkColour 1.0 0.75 0.5
  let lightOrange = mkColour 1.0 1.0 0.3
  let lemon = mkColour 1.0 1.0 0.5
  let green = mkColour 0.35 0.75 0.55

  let scene sampler () = 
     renderScenePointF 
         (mkMatteMonochrome 0.5 0.2 0.7) (mkMatteMonochrome 0.5 0.2 0.95) (mkMatteMonochrome 0.75 0.2 0.5) 
         (mkMatteMonochrome 0.85 0.2 0.5) (mkMatteMonochrome 0.75 0.2 0.6) (mkMatteMonochrome 0.6 0.2 0.5) 
         (ambientLight ()) [] sampler 1.0
  let sceneEL sampler () = 
      renderScenePointF 
          (mkMatteMonochrome 0.5 0.2 0.7) (mkMatteMonochrome 0.5 0.2 0.95) (mkMatteMonochrome 0.75 0.2 0.5) 
          (mkMatteMonochrome 0.85 0.2 0.5) (mkMatteMonochrome 0.75 0.2 0.6) (mkMatteMonochrome 0.6 0.2 0.5) 
          (ambientLight ()) [elight (fromColor Color.White) 1.0 sampler] sampler 1.0
  let sceneAO sampler () = 
      renderScenePointF 
          (mkMatteMonochrome 0.5 0.2 0.7) (mkMatteMonochrome 0.5 0.2 0.95) (mkMatteMonochrome 0.75 0.2 0.5) 
          (mkMatteMonochrome 0.85 0.2 0.5) (mkMatteMonochrome 0.75 0.2 0.6) (mkMatteMonochrome 0.6 0.2 0.5) 
          (ambientOccluder cwhite 0.5 0.5 sampler) [] sampler 1.0
  let sceneAOEL sampler () = 
      renderScenePointF 
          (mkMatteMonochrome 0.5 0.2 0.7) (mkMatteMonochrome 0.5 0.2 0.95) (mkMatteMonochrome 0.75 0.2 0.5) 
          (mkMatteMonochrome 0.85 0.2 0.5) (mkMatteMonochrome 0.75 0.2 0.6) (mkMatteMonochrome 0.6 0.2 0.5) 
          (ambientOccluder cwhite 0.1 0.5 sampler) [elight (fromColor Color.White) 1.0 sampler] sampler 1.0

  let gridTexture () = mkGridTexture 20.0 2.0 (mkMatte (mkColour 1.0 0.75 0.5) 1.0) (mkMatte (fromColor Color.White) 1.0) (mkMatte (fromColor Color.Black) 1.0)
  let boxTexture sampler = mkMatTexture(mkMatteGlossyReflectiveMaterial blue 0.2 blue 0.3 blue 0.9 1000 sampler)
  let s1Texture () = mkMatTexture(mkPhongReflectiveMaterial orange 0.0 orange 0.0 orange 0.0 orange 0.9 20)
  let s2Texture () = mkMatTexture(mkMatteMaterial red 0.4 red 0.4)
  let s3Texture sampler = mkMatTexture(mkPhongGlossyReflectiveMaterial orange 0.0 orange 0.3 lightOrange 0.3 orange 0.9 100 100 sampler)
  let cTexture sampler = mkMatTexture(mkPhongGlossyReflectiveMaterial green 0.0 lightOrange 0.0 green 0.75 green 0.9 10 10 sampler)
  let sceneGlossy sampler () = 
      renderScenePointF 
          (gridTexture ()) (boxTexture sampler) (s1Texture ()) (s2Texture ()) (s3Texture sampler) (cTexture sampler) 
          (ambientLight ()) [elight lemon 0.85 sampler; mkLight (mkPoint 150.0 250.0 -150.0) cwhite 1.5] sampler 1.0


  let mkMatteTexture ka kd c = mkMatTexture(mkMatteMaterial c ka c kd)
  let sceneMatte sampler () =
      renderScenePointF (mkMatteTexture 0.2 0.8 (mkMonochrome 0.8)) (mkMatteTexture 0.2 0.8 blue)
          (mkMatteTexture 0.2 0.8 orange) (mkMatteTexture 0.2 0.8 red) (mkMatteTexture 0.2 0.8 lightOrange) 
          (mkMatteTexture 0.2 0.8 green) (ambientLight ()) [mkLight (mkPoint 150.0 250.0 -150.0) cwhite 1.5] sampler 1.0

  let mkPhongTexture c e = mkMatTexture(mkPhongMaterial c 0.2 c 0.5 cwhite 0.4 e)
  let scenePhongPoint sampler () =
      renderScenePointF (mkMatteTexture 0.2 0.8 (mkMonochrome 0.8)) (mkPhongTexture blue 4)
          (mkPhongTexture orange 1000) (mkPhongTexture red 100) (mkPhongTexture lightOrange 10) 
          (mkPhongTexture green 100) (ambientLight ()) [mkLight (mkPoint 200.0 25.0 75.0) cwhite 1.5] sampler 1.0

  let scenePhongDirectional sampler () =
      renderScenePointF (mkMatteTexture 0.2 0.8 (mkMonochrome 0.8)) (mkPhongTexture blue 4)
          (mkPhongTexture orange 1000) (mkPhongTexture red 100) (mkPhongTexture lightOrange 10) 
          (mkPhongTexture green 100) (ambientLight ()) [mkDirectionalLight (mkVector 200.0 25.0 75.0) cwhite 1.5] sampler 1.0

  let scenePhongPointFar sampler () =
      renderScenePointF (mkMatteTexture 0.2 0.8 (mkMonochrome 0.8)) (mkPhongTexture blue 4)
          (mkPhongTexture orange 1000) (mkPhongTexture red 100) (mkPhongTexture lightOrange 10) 
          (mkPhongTexture green 100) (ambientLight ()) [mkLight (mkPoint 200.0 25.0 75.0) cwhite 1.5] sampler 50.0

  let scenePhongDirectionalFar sampler () =
      renderScenePointF (mkMatteTexture 0.2 0.8 (mkMonochrome 0.8)) (mkPhongTexture blue 4)
          (mkPhongTexture orange 1000) (mkPhongTexture red 100) (mkPhongTexture lightOrange 10) 
          (mkPhongTexture green 100) (ambientLight ()) [mkDirectionalLight (mkVector 200.0 25.0 75.0) cwhite 1.5] sampler 50.0


  let mkPhongReflectiveTexture c e = mkMatTexture(mkPhongReflectiveMaterial c 0.2 c 0.2 cwhite 0.4 cwhite 0.7 e)
  let scenePhongReflective sampler () = 
      renderScenePointF 
         (gridTexture ()) (mkPhongReflectiveTexture blue 4)
              (mkPhongReflectiveTexture orange 1000) (mkPhongReflectiveTexture red 100) (mkPhongReflectiveTexture lightOrange 10) 
              (mkPhongReflectiveTexture green 100) (ambientLight ()) [mkLight (mkPoint 200.0 25.0 75.0) cwhite 1.5] sampler 1.0

  let mkMatteGlossyReflectiveTexture c e sampler = mkMatTexture(mkMatteGlossyReflectiveMaterial c 0.2 c 0.2 cwhite 0.7 e sampler)
  let sceneMatteGlossyReflective sampler () = 
      renderScenePointF 
         (gridTexture ()) (mkMatteGlossyReflectiveTexture blue 4 sampler)
              (mkMatteGlossyReflectiveTexture orange 1000 sampler) (mkMatteGlossyReflectiveTexture red 100 sampler) (mkMatteGlossyReflectiveTexture lightOrange 10 sampler) 
              (mkMatteGlossyReflectiveTexture green 100 sampler) (ambientLight ()) [mkLight (mkPoint 200.0 25.0 75.0) cwhite 1.5] sampler 1.0


  let renderWithSampler (gname : string) (sampler : unit -> sampler) : Target list = 
    List.map (fun (test, name) -> mkTarget gname ((fun _ -> test (sampler ()) ()), name))
      [(scene, "AmbientLight");
       (sceneEL, "EnvironmentLight");
       (sceneAO, "AmbientOcclusion");
       (sceneAOEL, "AmbientOclusionEnvironmentLight");
       (sceneMatte, "matte");
       (scenePhongReflective, "phongReflective");
       (scenePhongPoint, "phongPoint");
       (scenePhongDirectional, "phongDirectional");
       (scenePhongPointFar, "phongPointFar");
       (scenePhongDirectionalFar, "phongDirectionalFar");
       (sceneMatteGlossyReflective, "matteGlossyReflective")
       (sceneGlossy, "glossy")]

  let renderMulti : Target list = 
    renderWithSampler "material multi" (fun () -> mkMultiJitteredSampler 4 83)
  let renderHigh : Target list = 
    renderWithSampler "material high" (fun () -> mkMultiJitteredSampler 6 83)
  let renderRegular : Target list =   
    renderWithSampler "material regular" (fun () -> mkRegularSampler 4)
    