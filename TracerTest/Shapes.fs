namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module Shapes =

  let renderSphere () =
    let light1 = mkLight (mkPoint 4.0 4.0 4.0) (fromColor Color.White) 1.0 in
    let light2 = mkLight (mkPoint -4.0 -4.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 4.0 2.0 1024 512 (mkRegularSampler 1);
      scene = mkScene [sphere] [light1; light2] ambientLight 0}

  let renderReflectiveSpheres () =
    let light = mkLight (mkPoint 0.0 0.0 2.0) (mkColour 1. 1. 1.) 1.0
    let ambientLight = mkAmbientLight (mkColour 1. 1. 1.) 0.2
    let sphere = mkSphere (mkPoint -0.8 0.0 0.0) 0.7 (mkMatTexture (mkMatteReflective (mkColour 0. 0. 1.) 1.0 (fromColor Color.White) 0.7))
    let sphere2 = mkSphere (mkPoint 0.8 0.0 0.0) 0.7 (mkMatTexture (mkMatteReflective (mkColour 1. 0. 0.) 1.0 (fromColor Color.White) 0.7))
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 2.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 (mkRegularSampler 1);
      scene = mkScene [sphere;sphere2] [light] ambientLight 4}

  let renderHollowCylinder () =
    let light = mkLight (mkPoint 2.0 3.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let cylinder = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 2.0 1.0 (mkMatTexture (mkMatte (fromColor Color.Yellow) 1.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 -0.5) 18.0 4.0 4.0 500 500 (mkRegularSampler 1);
      scene = mkScene [cylinder] [light] ambientLight 0}

  let renderSolidCylinder () =
    let light = mkLight (mkPoint 2.0 3.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let cylinder = 
      mkSolidCylinder (mkPoint 0.0 0.0 0.0) 2.0 1.0 (mkMatTexture (mkMatte (fromColor Color.Yellow) 1.0))
        (mkMatTexture (mkMatte (fromColor Color.Red) 1.0)) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 -0.5) 18.0 4.0 4.0 500 500 (mkRegularSampler 1);
      scene = mkScene [cylinder] [light] ambientLight 0}
    
  let renderInsideSphere () =
    let light = mkLight (mkPoint 0.0 0.0 0.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Red) 1.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 (mkRegularSampler 1);
     scene = mkScene [sphere] [light] ambientLight 0}

  let sierpinskiGasket1 numSamples () =
      let pink = mkColour 0.168 0.171 0.009
      let red = mkColour 0.243 0.018 0.164
      let green = mkColour 0.094 0.243 0.029
      let blue = mkColour 0.094 0.1 0.243
      let white = mkColour 1.0 1.0 1.0
      let kd = 0.75
      let kr = 1.0
      let ks = 0.1
      let e = 20
      let s1 = mkSphere (mkPoint 0.0 1.414 0.0) 0.866 (mkMatTexture (mkPhongReflective pink kd white ks kr e))
      let s2 = mkSphere (mkPoint 0.866 0.0 -0.5) 0.866 (mkMatTexture (mkPhongReflective red kd white ks kr e))
      let s3 = mkSphere (mkPoint 0.0 0.0 1.0) 0.866 (mkMatTexture (mkPhongReflective green kd white ks kr e))
      let s4 = mkSphere (mkPoint -0.866 0.0 -0.5) 0.866 (mkMatTexture (mkPhongReflective blue kd white ks kr e))

      let ambient = mkAmbientLight (fromColor Color.White) 0.5
      let light = mkLight (mkPoint 0.0 20.0 20.0) (fromColor Color.White) 5.0

      { camera = mkPinholeCamera (mkPoint -35.0 25.0 35.0) (mkPoint 0.0 0.4 -0.2) (mkVector 0.0 1.0 0.0) 50.0 4.67 3.5 1024 768 (mkMultiJitteredSampler numSamples 83);
        scene = mkScene [s1; s2; s3; s4] [light] ambient 12 }

  let sierpinskiGasket2 numSamples () =
      let pink = mkColour 0.168 0.171 0.009
      let red = mkColour 0.243 0.018 0.164
      let green = mkColour 0.094 0.243 0.029
      let blue = mkColour 0.094 0.1 0.243
      let white = mkColour 1.0 1.0 1.0
      let kd = 0.75
      let kr = 1.0
      let ks = 0.1
      let e = 20
      let s1 = mkSphere (mkPoint 0.0 1.414 0.0) 0.866 (mkMatTexture (mkPhongReflective pink kd white ks kr e))
      let s2 = mkSphere (mkPoint 0.866 0.0 -0.5) 0.866 (mkMatTexture (mkPhongReflective red kd white ks kr e))
      let s3 = mkSphere (mkPoint 0.0 0.0 1.0) 0.866 (mkMatTexture (mkPhongReflective green kd white ks kr e))
      let s4 = mkSphere (mkPoint -0.866 0.0 -0.5) 0.866 (mkMatTexture (mkPhongReflective blue kd white ks kr e))

      let ambient = mkAmbientLight white 0.5

      { camera = mkPinholeCamera (mkPoint 0.0 -10.0 0.0) (mkPoint 0.0 1.0 0.0) (mkVector 0.0024 1.0 0.0075) 40.0 4.67 3.5 1024 768 (mkMultiJitteredSampler numSamples 83);
        scene = mkScene [s1; s2; s3; s4] [] ambient 12 }

  let render : Target list = 
    List.map (mkTarget "shapes") 
      [(renderSphere,"sphere");
       (renderReflectiveSpheres,"reflectiveSpheres");
       (renderHollowCylinder, "hollowCylinder");
       (renderSolidCylinder, "solidCylinder");
       (renderInsideSphere, "insideSphere");
       (sierpinskiGasket1 4, "sierpinskiGasket1");
       (sierpinskiGasket2 4, "sierpinskiGasket2")]