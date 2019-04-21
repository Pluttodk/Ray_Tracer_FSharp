namespace TracerTestSuite

open System
open System.Drawing
open Tracer.API
open Util

module AffineTransformations =
 
  let mkCube t = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) t t t t t t

  let ocube (simple : bool) =
    if simple 
    then mkCube (mkMatTexture(mkMatte (fromColor Color.Gold) 1.0))
    else
      let b = fromColor Color.Blue
      let w = fromColor Color.White
      let blue = (mkMatTexture(mkMatteMaterial b 0.1 b 0.7))
      let one = mkSphere (mkPoint 0.0 1.0 0.0) 0.2 blue
      let two1 = mkSphere (mkPoint 1.0 0.5 -0.5) 0.2 blue
      let two2 = mkSphere (mkPoint 1.0 -0.5 0.5) 0.2 blue
      let three1 = mkSphere (mkPoint 0.5 0.5 -1.0) 0.2 blue
      let three2 = mkSphere (mkPoint 0.0 0.0 -1.0) 0.2 blue
      let three3 = mkSphere (mkPoint -0.5 -0.5 -1.0) 0.2 blue
      let four1 = mkSphere (mkPoint 0.5 0.5 1.0) 0.2 blue
      let four2 = mkSphere (mkPoint -0.5 -0.5 1.0) 0.2 blue
      let four3 = mkSphere (mkPoint -0.5 0.5 1.0) 0.2 blue
      let four4 = mkSphere (mkPoint 0.5 -0.5 1.0) 0.2 blue
      let five1 = mkSphere (mkPoint -1.0 0.0 0.0) 0.2 blue
      let five2 = mkSphere (mkPoint -1.0 0.5 0.5) 0.2 blue
      let five3 = mkSphere (mkPoint -1.0 0.5 -0.5) 0.2 blue
      let five4 = mkSphere (mkPoint -1.0 -0.5 0.5) 0.2 blue
      let five5 = mkSphere (mkPoint -1.0 -0.5 -0.5) 0.2 blue
      let six1 = mkSphere (mkPoint 0.5 -1.0 0.5) 0.2 blue
      let six2 = mkSphere (mkPoint 0.0 -1.0 0.5) 0.2 blue
      let six3 = mkSphere (mkPoint -0.5 -1.0 0.5) 0.2 blue
      let six4 = mkSphere (mkPoint 0.5 -1.0 -0.5) 0.2 blue
      let six5 = mkSphere (mkPoint 0.0 -1.0 -0.5) 0.2 blue
      let six6 = mkSphere (mkPoint -0.5 -1.0 -0.5) 0.2 blue

      let dots = [one; 
                  two1; two2; 
                  three1; three2; three3; 
                  four1; four2; four3; four4;
                  five1; five2; five3; five4; five5;
                  six1; six2; six3; six4; six5; six6]
      let g = fromColor Color.Gold
      let ocube_base = mkCube (mkMatTexture(mkPhongMaterial g 0.1 g 0.7 w 0.4 10))
      List.fold subtraction ocube_base dots
  let cube simple = transform (ocube simple) (translate 1.0 1.0 1.0)
  let mkRender o () = 
    let light = mkLight (mkPoint 4.0 2.0 4.0) (fromColor Color.White) 1.0
    let light2 = mkLight (mkPoint -4.0 2.0 4.0) (fromColor Color.White) 1.0
    let lights = [light; light2]
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let camera = mkPinholeCamera (mkPoint 20.0 20.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 18.0 4.0 4.0 500 500 (mkRegularSampler 1) in
    let cx = transform (mkHollowCylinder (mkPoint 0.0 0.0 0.0) 0.1 100.0 (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))) (rotateZ (Math.PI / 2.0))
    let cy = transform (mkHollowCylinder (mkPoint 0.0 0.0 0.0) 0.1 100.0 (mkMatTexture (mkMatte (fromColor Color.GreenYellow) 1.0))) (rotateX (Math.PI / 2.0))
    let cz = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 0.1 100.0 (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
    let axes = group cx (group cy cz)
    {scene = mkScene [o (); axes] lights ambientLight 0; camera = camera}

  

  let render (simple : bool) : Target list =
    let testRotateX angle () = transform (cube simple) (rotateX (Util.degrees_to_radians angle))
    let testRotateY angle () = transform (cube simple) (rotateY (Util.degrees_to_radians angle))
    let testRotateZ angle () = transform (cube simple) (rotateZ (Util.degrees_to_radians angle)) in


    let testMirrorX () = transform (cube simple) mirrorX
    let testMirrorY () = transform (cube simple) mirrorY
    let testMirrorZ () = transform (cube simple) mirrorZ

    let testScaleX x () = transform (ocube simple) (scale x 1.0 1.0)
    let testScaleY y () = transform (ocube simple) (scale 1.0 y 1.0)
    let testScaleZ z () = transform (ocube simple) (scale 1.0 1.0 z)

    let testSheareXY d () = transform (ocube simple) (sheareXY d)
    let testSheareXZ d () = transform (ocube simple) (sheareXZ d)
    let testSheareYX d () = transform (ocube simple) (sheareYX d)

    let testSheareYZ d () = transform (ocube simple) (sheareYZ d)
    let testSheareZX d () = transform (ocube simple) (sheareZX d)
    let testSheareZY d () = transform (ocube simple) (sheareZY d)

    let testTranslateX d () = transform (ocube simple) (translate d 0.0 0.0)
    let testTranslateY d () = transform (ocube simple) (translate 0.0 d 0.0)
    let testTranslateZ d () = transform (ocube simple) (translate 0.0 0.0 d)
    let name = 
      if simple 
      then "affineTransformationsSimple"
      else "affineTransformationsCSG"
    List.map (fun (s,n) -> Util.mkTarget name (mkRender s,n))
      [((fun () -> (ocube simple)), "originBase");
       ((fun () -> (cube simple)), "positiveBase");
       (testRotateX 90.0, "rotateX_90");
       (testRotateX 180.0, "rotateX_180");
       (testRotateX 270.0, "rotateX_270");
       (testRotateY 90.0, "rotateY_90");
       (testRotateY 180.0, "rotateY_180");
       (testRotateY 270.0, "rotateY_270");
       (testRotateZ 90.0, "rotateZ_90");
       (testRotateZ 180.0, "rotateZ_180");
       (testRotateZ 270.0, "rotateZ_270");
       (testMirrorX, "mirrorX");
       (testMirrorY, "mirrorY");
       (testMirrorZ, "mirrorZ");
       (testScaleX 2.0, "scaleX");
       (testScaleY 2.0, "scaleY");
       (testScaleZ 2.0, "scaleZ");
       (testSheareXY 1.0, "sheareXY");
       (testSheareXZ 1.0, "sheareXZ");
       (testSheareYX 1.0, "sheareYX");
       (testSheareYZ 1.0, "sheareYZ");
       (testSheareZX 1.0, "sheareZX");
       (testSheareZY 1.0, "sheareZY");
       (testTranslateX 1.0, "translateX");
       (testTranslateY 1.0, "translateY");
       (testTranslateZ 1.0, "translateZ")]
