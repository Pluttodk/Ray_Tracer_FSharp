namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module AreaLights =

  let mkUnitBox t = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) t t t t t t
  let mkUnitCylinder t = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 t t t
  let cube () = mkUnitBox (mkMatTexture (mkPhong (fromColor Color.Red) 1.0 1.0 10))
  let sphere () = mkSphere (mkPoint 0.0 0.0 0.0) 1.3 (mkMatTexture (mkPhong (fromColor Color.Blue) 1.0 1.0 10))

  let cy () = transform (mkUnitCylinder (mkMatTexture (mkMatte (fromColor Color.Yellow) 0.02))) (scale 0.7 1.5 0.7)  in
  let cx () = transform (cy ()) (rotateX (Util.degrees_to_radians 90.0)) in
  let cz () = transform (cy ()) (rotateZ (Util.degrees_to_radians 90.0)) in 

  let cross () =
      union (cy ()) (union (cz ()) (cx ()))

  let lantern () = subtraction (intersection (cube ()) (sphere ())) (cross ())

  let black = mkColour 0.0 0.0 0.0
  let white = mkColour 1.0 1.0 1.0

  let renderPlaneGrid numSamples () =
      let lb = fromColor(Color.LightBlue)
      let ambientLight = mkAmbientLight (fromColor Color.White) 0.5 in
      let l = mkDirectionalLight (mkVector 0.0 1.0 0.0) (fromColor Color.White) 1.0
      let p1 = mkPlane (mkGridTexture 5.0 0.5 (mkMatte (mkColour 1.0 1.0 1.0) 1.0) (mkMatte (fromColor Color.White) 1.0) (mkMatte (fromColor Color.Black) 1.0))
      let p2 = transform (mkPlane (mkMatTexture(mkPhongGlossyReflectiveMaterial lb 0.2 lb 0.5    white 0.0 white 0.7 10000 10000 (mkMultiJitteredSampler numSamples 83))))
                         (rotateX (Math.PI / 2.0))
      { camera = mkPinholeCamera (mkPoint -5.0 1.0 20.0) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 1);
        scene = mkScene [p1; p2] [l] ambientLight 1} 



  let renderCheckerSphere numSamples () =
      let ambientLight = mkAmbientLight (fromColor Color.White) 0.0 in
      let s = mkSphere (mkPoint 0.0 -2.0 0.0) 0.5 (mkMatTexture(mkMatteReflectiveMaterial black 0.2 (mkColour 0.0 1.0 0.2) 0.1 (mkColour 0.75 0.75 1.0) 0.85))
      let p = transform (mkPlane (mkCheckeredTexture 1.0 (mkMatteMaterial (mkMonochrome 1.0) 0.25 (mkMonochrome 1.0) 0.75) (mkMatteMaterial (mkMonochrome 0.9) 0.25 (mkMonochrome 0.9) 0.75))) (mergeTransformations [rotateX (Math.PI / 2.0); translate 0.0 -2.75 0.0]) 
      let l = mkAreaLight (mkBaseRectangle (mkPoint -1.0 -0.5 -1.0) (mkPoint -1.0 -0.5 1.0) (mkPoint 1.0 -0.5 -1.0)) (mkEmissive (mkMonochrome 1.0) 3.0) (mkMultiJitteredSampler numSamples 83)
      { camera = mkPinholeCamera (mkPoint 30.0 13.0 20.0) (mkPoint 0.0 -2.0 0.0) (mkVector 0.0 1.0 0.0) 90.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83);
        scene = mkScene [p; s] [l] ambientLight 1} 

  let renderDiskAreaLight numSamples () =
    let bdisk = mkBaseDisk (mkPoint 0.0 0.0 0.0) 0.3
    let t = mergeTransformations [(rotateZ (System.Math.PI/2.0)); translate 0.0 1.8 -1.0]
    let light = transformLight (mkAreaLight bdisk (mkEmissive (fromColor Color.White) 25.0) (mkMultiJitteredSampler numSamples 83)) t
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.01 in
    let p = transform (mkPlane (mkMatTexture (mkPhong (fromColor Color.BlueViolet) 1.0 0.5 10))) (rotateX (System.Math.PI/2.0)) in
    let s = mkSphere (mkPoint 0.0 1.0 1.0) 0.5 (mkMatTexture (mkPhong (fromColor Color.Yellow) 1.0 0.5 10)) in
    { camera = mkPinholeCamera (mkPoint 0.0 2.0 3.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83);
      scene = mkScene [p; s] [light] ambientLight 5 }

  let renderSphereAreaLight numSamples () =
    let bsphere = mkBaseSphere (mkPoint 0.0 0.0 0.0) 0.3
    let t = mergeTransformations [translate 0.0 1.8 -1.0]
    let light = transformLight (mkAreaLight bsphere (mkEmissive (fromColor Color.White) 25.0) (mkMultiJitteredSampler numSamples 83)) t
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.01 in
    let p = transform (mkPlane (mkMatTexture (mkPhong (fromColor Color.BlueViolet) 1.0 0.5 10))) (rotateX (System.Math.PI/2.0)) in
    let s = mkSphere (mkPoint 0.0 1.0 1.0) 0.5 (mkMatTexture (mkPhong (fromColor Color.Yellow) 1.0 0.5 10)) in
    { camera = mkPinholeCamera (mkPoint 0.0 2.0 3.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83);
      scene = mkScene [p; s] [light] ambientLight 5 }

  let renderLanternAreaLight numSamples () =
    let bsphere = mkBaseSphere (mkPoint 0.0 0.0 0.0) 0.3
    let pointLight = mkLight (mkPoint 0.0 0.5 2.5) (fromColor Color.White) 1.0 in
    let t = mergeTransformations [translate 0.0 1.8 0.0]
    let light = transformLight (mkAreaLight bsphere (mkEmissive (fromColor Color.White) 25.0) (mkMultiJitteredSampler numSamples 83)) t
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let p = transform (mkPlane (mkMatTexture (mkPhong (fromColor Color.BlueViolet) 1.0 0.5 10))) (rotateX (System.Math.PI/2.0)) in
    let l = transform (lantern ()) t
    { camera = mkPinholeCamera (mkPoint 5.0 4.0 5.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83);
      scene = mkScene [l; p] [light] ambientLight 5 }

  let render : Target list = 
    List.map (mkTarget "areaLight") 
      [(renderDiskAreaLight 4, "diskAreaLight");
       (renderSphereAreaLight 4, "sphereAreaLight");
       (renderLanternAreaLight 4, "lanternAreaLight");
       (renderCheckerSphere 4, "checkerSphere");
       (renderPlaneGrid 4, "planeGrid")]