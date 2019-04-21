namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module Light =

  let renderSphere lightPos () =
    let light = mkLight (lightPos ()) (fromColor Color.White) 0.9 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 12.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 10.0 2.0 2.0 500 500 (mkRegularSampler 1);
      scene = mkScene [sphere] [light] ambientLight 0 }



  let renderSphereColour () =
    let lightLeft = mkLight (mkPoint -4.0 -2.0 2.0) (fromColor Color.Red) 0.8 in
    let lightRight = mkLight (mkPoint 4.0 -2.0 2.0) (fromColor Color.Green) 0.8 in
    let lightTop = mkLight (mkPoint 0.0 4.0 2.0) (fromColor Color.Blue) 0.8 in
    let lightBottom = mkLight (mkPoint 0.0 -4.0 2.0) (fromColor Color.White) 0.4 in
    
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.0 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Orange) 1.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 12.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 10.0 2.0 2.0 500 500 (mkRegularSampler 1);
      scene = mkScene [sphere] [lightLeft;lightRight;lightTop;lightBottom] ambientLight 0}

  
  let renderShadow () =
    let light = mkLight (mkPoint 1.5 -1.5 4.0) (fromColor Color.Cyan) 0.8 in
    let light2 = mkLight (mkPoint 0.0 0.0 4.0) (fromColor Color.Red) 0.6 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0)) in
    let redMat = mkMatte (fromColor Color.Red) 1.0
    let red = mkMatTexture redMat
    let cylinder = mkSolidCylinder (mkPoint 1.0 -1.0 2.0) 0.3 0.8 red red red in
    let greenMat = mkMatte (fromColor Color.Green) 1.0
    let checker x y =
        let abs' f = if f < 0.0 then 1.0 - (f*2.0) else f * 2.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then redMat
        else greenMat
    let plane =  transform (mkPlane (mkTexture checker)) 
                  (mergeTransformations [rotateY (Math.PI/4.0);rotateX (Math.PI/4.0);(translate -4.0 0.0 -4.0)])
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 28.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 10.0 2.0 2.0 500 500 (mkRegularSampler 1);
      scene = mkScene [sphere;cylinder;plane] [light;light2] ambientLight 0 }

  

  let render =
    List.map (Util.mkTarget "light")
      [(renderSphere (fun () -> mkPoint 0.0 0.0 4.0), "sphere front");
      (renderSphere (fun () -> mkPoint 0.0 4.0 0.0), "sphere top");
      (renderSphere (fun () -> mkPoint 0.0 0.0 40.0), "sphere front far");
      (renderSphere (fun () -> mkPoint 0.0 40.0 0.0), "sphere top far");
      (renderSphereColour, "sphere colour");
      (renderShadow, "shadow")]
