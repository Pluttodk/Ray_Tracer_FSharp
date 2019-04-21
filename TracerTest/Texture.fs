namespace TracerTestSuite

module Texture =

  open System
  open System.Drawing
  open Tracer.API
  open Util

  let renderEarth () =
    let texture = mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/earth.jpg"
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 texture) 
                  (mergeTransformations [rotateY (System.Math.PI*1.0);rotateX (Math.PI/4.0)])
    { camera = mkPinholeCamera (mkPoint 0.0 1.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [sphere] [light] ambientLight 3}

  let renderSphere () =
    let white = mkMatteReflective (fromColor Color.Red) 1.0 (fromColor Color.White) 0.5
    let black = mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.White) 0.5
    let checker x y =
        let abs' s f = if f < 0.0 then 1.0 - (f * s) else f * s
        if (int (abs' 64.0 x) + int (abs' 32.0 y)) % 2 = 0
        then white
        else black
    let texture = mkTexture checker
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 texture) (rotateX (Math.PI/4.0))
    { camera = mkPinholeCamera (mkPoint 0.0 1.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [sphere] [light] ambientLight 3}



  let renderCylinder () =
    let white = mkMatte (fromColor Color.Red) 1.0
    let black = mkMatte (fromColor Color.Green) 1.0
    let checker x y =
        let abs' f = if f < 0.0 then 1.0 - (f*64.0) else f * 64.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then white
        else black
    let cbase = mkSolidCylinder  (mkPoint 0.0 0.0 0.0) 0.5 1.9 (mkTexture checker) 
                  (mkTexture checker) (mkMatTexture (mkMatte (fromColor Color.White) 1.0))
    let c = transform cbase (rotateX (Math.PI/4.0))
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [c] [light] ambientLight 2}

  let renderDiscs () =
    let mkMat c = mkMatte (fromColor c) 1.0
    let colours = Array.map mkMat [|Color.Green;Color.Red;Color.Blue;Color.Yellow;Color.Magenta;Color.Orange;Color.Cyan;Color.White|]
    let checker x' y' =
      let x = 2.0*x' - 1.0
      let y = 2.0*y' - 1.0
      let a = atan2 x y
      let a' = if a < 0.0 then a + 2.0 * Math.PI else a
      let d = int (4.0*(a' / Math.PI)) + if x * x + y * y <= 0.25 then 4 else 0
      colours.[d%8]
    let disc = mkDisk (mkPoint 0.0 0.0 0.0) 0.7 (mkTexture checker) 
    let d1 = transform disc (mergeTransformations [translate -0.5 -0.5 -0.5])
    let d2 = transform disc (mergeTransformations [rotateX (-Math.PI/4.0);translate 0.5 0.5 0.5])
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [d1;d2] [light] ambientLight 2}

  let mkColor c = mkMatTexture (mkMatte (fromColor c) 1.0)

  let renderBox () =
    let texture = mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/earth.jpg"
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let box = transform (mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) 
                        (mkColor Color.Blue) (mkColor Color.Red) (mkColor Color.Green) 
                        (mkColor Color.Yellow) (mkColor Color.Purple) (mkColor Color.White)) 
                        (mergeTransformations [rotateY (System.Math.PI/4.0);rotateX (Math.PI/4.0)])
    { camera = mkPinholeCamera (mkPoint 0.0 1.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [box] [light] ambientLight 3}

  let renderPlane numSamples () =
    let white = mkMatteReflective (fromColor Color.White) 1.0 (fromColor Color.White) 0.5
    let green = mkMatte (fromColor Color.Green) 1.0
    let checker = mkCheckeredTexture 1.0 white green

    let reflect = mkMatteReflective (fromColor Color.White) 1.0 (fromColor Color.White) 0.8
    let notreflect = mkMatte (fromColor Color.Green) 1.0
    let checker2 x y =
        let abs' f = if f < 0.0 then 1.0 - (f*2.0) else f * 2.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then reflect
        else notreflect
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 0.9 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let sphere = mkSphere (mkPoint 1.0 1.0 0.0) 1.0 (mkMatTexture (mkMatteReflective (fromColor Color.Blue) 1.0 (fromColor Color.White) 0.2)) in
    let p' = transform (mkPlane (mkMatTexture (mkMatte (fromColor Color.White) 1.0)))
               (mergeTransformations [rotateX (System.Math.PI/2.0); translate 0.0 10.0 0.0])
    let p = transform (mkPlane checker) (rotateX (System.Math.PI/2.0))
    { camera = mkPinholeCamera (mkPoint 0.0 1.9 8.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.66 2.0 1024 768 (mkMultiJitteredSampler numSamples 83);
      scene = mkScene [sphere;p;p'] [light] ambientLight 3}

  let renderBoxes () =
    let ftex c1 c2 c3 c4 = 
      let tfun x y = 
        if x < 0.5 
        then if y < 0.5 then mkMatte c1 1.0 else mkMatte c3 1.0
        else if y < 0.5 then mkMatte c4 1.0 else mkMatte c2 1.0
      mkTexture tfun
    let one = ftex (fromColor Color.White) (fromColor Color.Orange) (fromColor Color.Magenta) (fromColor Color.Blue)
    let two = ftex (fromColor Color.White) (fromColor Color.Orange) (fromColor Color.Magenta) (fromColor Color.Green)
    let three = ftex (fromColor Color.White) (fromColor Color.Orange) (fromColor Color.Magenta) (fromColor Color.Red)
    let white = mkColor Color.White
    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let box1 = transform (mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) 
                           one white two white three white)
                        (mergeTransformations [rotateY (System.Math.PI/4.0);rotateX (Math.PI/4.0); translate -1.5 0.0 0.0])
    let box2 = transform (mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) 
                           white two white three white one)
                        (mergeTransformations [rotateY (System.Math.PI + System.Math.PI/ 4.0);rotateX (Math.PI/ -4.0); translate 1.5 0.0 0.0])
    { camera = mkPinholeCamera (mkPoint 0.0 1.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 4.0 2.0 2000 1000 (mkRegularSampler 1);
      scene = mkScene [box1;box2] [light] ambientLight 3}

  let renderRectangles () =
    let texture = mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/earth.jpg"
    let r1 = mkRectangle (mkPoint -1. -1. 1.) (mkPoint -1. 1. 1.) (mkPoint 0.9999 -1. 1.) texture
    let r2 = mkRectangle (mkPoint -1. -1. 1.) (mkPoint -1. 1. 1.) (mkPoint -1. -1. -0.9999) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0)) 
    let r3 = mkRectangle (mkPoint 1. -1. 1.) (mkPoint 1. 1. 1.) (mkPoint 1. -1. -1.) (mkMatTexture (mkMatte (fromColor Color.Yellow) 1.0)) 
    
    let m1 = mkRectangle (mkPoint -2.0 -1. 0.) (mkPoint -2.0 2.0 0.) (mkPoint 0.0 -1. -2.) (mkMatTexture (mkMatteReflective (fromColor Color.White) 1.0 (fromColor Color.White) 0.95)) 
    let r4 = mkRectangle (mkPoint -1. -1. 1.) (mkPoint -1. -1. -1.) (mkPoint 1. -1. 1.) (mkMatTexture (mkMatte (fromColor Color.Green) 1.0))
    let p = transform (mkPlane (mkMatTexture (mkMatte (fromColor Color.Gray) 1.0)))
              (mergeTransformations [(rotateX (System.Math.PI/2.0)); translate 0.0 -1.001 0.0 ])
    
    let sphere = transform (mkSphere (mkPoint 0.0 0.0 0.0) 0.5 texture) 
                  (mergeTransformations [rotateY (System.Math.PI*1.0);rotateX (Math.PI/4.0)])

    let light = mkLight (mkPoint 0.0 1.0 4.0) (fromColor Color.White) 0.9 in  
    let light2 = mkLight (mkPoint 0.0 0.9 0.9) (fromColor Color.White) 0.7 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    { camera = mkPinholeCamera (mkPoint 30.0 30.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [sphere;m1;r1;r2;r3;r4;p] [light;light2] ambientLight 3}
    

  let renderMarble numSamples () =
    let light = mkLight (mkPoint -4.0 10.0 4.0) (fromColor Color.White) 1. in  
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let earthTex = mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/earth.jpg"
    let marbleTex = mkReflectiveTextureFromFile 0.5 (fun x y -> (x,y)) "../../../textures/marble.jpg"
    let whiteTex = mkMatTexture (mkMatte (mkColour 0.5 0.5 0.5) 1.)
    let earth = transform (mkSphere (mkPoint 0. 1.5 0.) 1.5 earthTex) (mergeTransformations [rotateY (-Math.PI * 0.5); translate 1. 0. -1.])
    let marble = mkBox (mkPoint -6. -0.2 -6.) (mkPoint 6. 0. 6.) whiteTex whiteTex marbleTex whiteTex whiteTex whiteTex
    let cylinder = mkSolidCylinder (mkPoint -2. 1.5 2.) 1.3 3.0 earthTex earthTex earthTex
    { camera = mkPinholeCamera (mkPoint 30.0 30.0 30.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 10.0 2.66 2.0 1024 768 (mkMultiJitteredSampler numSamples 83);
      scene = mkScene [marble;earth;cylinder] [light] ambientLight 3}
    


  let render =
    List.map (Util.mkTarget "texture")
      [(renderDiscs, "discs");
       (renderBox, "box");
       (renderBoxes, "boxes");
       (renderCylinder, "cylinder");
       (renderEarth, "earth");
       (renderPlane 4, "plane");
       (renderSphere, "sphere");
       (renderRectangles, "rectangles");
       (renderMarble 4, "marble");
       ]