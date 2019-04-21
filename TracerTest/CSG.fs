namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module CSG =
 
  let mkColourTexture c r = mkMatTexture (mkMatte (fromColor c) r)
  let mkUnitBox t = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) t t t t t t
  let mkUnitCylinder t = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 t t t
  let l1 () = mkLight (mkPoint 4.0 0.0 4.0) (fromColor Color.White) 1.0 in
  let l2 () = mkLight (mkPoint -4.0 0.0 4.0) (fromColor Color.White) 1.0 in
  let l3 () = mkLight (mkPoint 0.0 0.0 0.0) (fromColor Color.White) 1.0 in
  let l4 () = mkLight (mkPoint 2.0 4.0 4.0) (fromColor Color.White) 1.0 in
  let l5 () = mkLight (mkPoint 0.0 -4.0 0.0) (fromColor Color.White) 1.0 in
  let l6 () = mkLight (mkPoint 0.0 16.0 16.0) (fromColor Color.White) 2.0 in
  let ambientLight () = mkAmbientLight (fromColor Color.White) 0.1 in
  let camera () = mkPinholeCamera (mkPoint 16.0 16.0 16.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 8.0 1.4 1.4 500 500 (mkRegularSampler 1) in
  let camera2 () = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 2.0 2.0 500 500 (mkRegularSampler 1) in

  let cube () = mkUnitBox (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
  let sphere () = mkSphere (mkPoint 0.0 0.0 0.0) 1.3 (mkMatTexture (mkPhongMaterial (fromColor Color.Blue) 1.0 (fromColor Color.Blue) 1.0 (fromColor Color.White) 0.2 10))
  let sphere1 () = mkSphere (mkPoint 0.5 0.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
  let sphere2 () = mkSphere (mkPoint -0.5 0.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
  let sphere3 () = mkSphere (mkPoint -0.5 0.0 0.0) 0.2 (mkMatTexture (mkMatte (fromColor Color.Yellow) 1.0))

  let cy () = transform (mkUnitCylinder (mkMatTexture (mkPhongMaterial (fromColor Color.Yellow) 1.0 (fromColor Color.Yellow) 1.0 (fromColor Color.White) 0.6 10))) (scale 0.7 1.5 0.7)  in
  let cx () = transform (cy ()) (rotateX (Util.degrees_to_radians 90.0)) in
  let cz () = transform (cy ()) (rotateZ (Util.degrees_to_radians 90.0)) in 

  let tdCross () = union (cz ()) (cx ())

  let cross () =
      union (cy ()) (tdCross ())

  let renderUnion () =
    { scene = mkScene [union (sphere1 ()) (sphere2 ())] [l1 (); l2 ()] (ambientLight ()) 0;
      camera = camera2 () }

  let renderUnion2 () =
    { camera = mkPinholeCamera (mkPoint 0.9 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 0.4 0.9 0.9 500 500 (mkRegularSampler 1);
      scene = mkScene [union (sphere1 ()) (sphere2 ()); sphere3 ()] [l3 ()] (ambientLight ()) 0 }

  let renderUnion3 () =
    { camera = mkPinholeCamera (mkPoint 0.9 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 0.4 0.9 0.9 500 500 (mkRegularSampler 1);
      scene = mkScene [group (sphere1 ()) (sphere2 ()); sphere3 ()] [l3 ()] (ambientLight ()) 0 }
    
  let renderIntersection () =
    let cube = mkUnitBox (mkMatTexture (mkMatte (fromColor Color.Red) 1.0)) in
    { scene = mkScene [intersection (sphere1 ()) (sphere2 ())] [l1 (); l2 ()] (ambientLight ()) 0 ;
      camera = camera2 ()}
    
  let renderSubtraction () =
    let cube = mkUnitBox (mkMatTexture (mkMatte (fromColor Color.Red) 1.0)) in
    { scene = mkScene [subtraction (sphere2 ()) (sphere1 ())] [l1 (); l2 ()] (ambientLight ()) 0 ;
      camera = camera ()}

  let renderCY () =
    { scene = mkScene [cy ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let renderCX () =
    { scene = mkScene [cx ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let renderCZ () =
    { scene = mkScene [cz ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let renderSphere () =
    { scene = mkScene [sphere ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let renderBox () =
    { scene = mkScene [cube ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let renderCross () =
    { scene = mkScene [cross ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let rendertdCross () =
    { scene = mkScene [tdCross ()] [l6 ()] (ambientLight ()) 0;
      camera = camera ()}

  let renderIntersection2 () =
    { scene = mkScene [intersection (cube ()) (sphere ())] [l6 ()] (ambientLight ()) 0
      camera = camera ()}

  let renderLantern () =
    { scene = mkScene [subtraction (intersection (sphere ()) (cube ())) (cross ())] [l3 (); l6 ()] (ambientLight ()) 0
      camera = camera ()}

  let renderLantern2 () =
    let sphere2 = mkSphere (mkPoint 1.3 1.3 1.3) 1.4 (mkMatTexture (mkMatte (fromColor Color.AntiqueWhite) 1.0))
    let sphere3 = mkSphere (mkPoint -1.3 -1.3 -1.3) 1.4 (mkMatTexture (mkMatte (fromColor Color.AntiqueWhite) 1.0))

    { scene = mkScene [subtraction (subtraction (subtraction (intersection (cube ()) (sphere ())) (cross ())) sphere2) sphere3] [l1 (); l2 (); l3 ()] (ambientLight ()) 0
      camera = camera ()}

  let renderUnionTorus (R : float) (r : float) () =
    let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
    let s3 = union (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene [s3] [l4 ()] (ambientLight ()) 0}

  let renderIntersectionTorus (R : float) (r : float) () =
    let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
    let s3 = intersection (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene [s3] [l4 ()] (ambientLight ()) 0}

  let renderSubtractionTorus (R : float) (r : float) () =
    let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
    let s3 = subtraction s1 (transform s2 (translate 0.0 (3.0 * r) 0.0))
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene [s3] [l4 ()] (ambientLight ()) 0}

  let renderUnionTorus2 (R : float) (r : float) () =
    let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
    let s3 = union (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
    { camera = mkPinholeCamera (mkPoint 0.0 -4.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene [s3] [l5 ()] (ambientLight ()) 0}

  let renderIntersectionTorus2 (R : float) (r : float) () =
    let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Red) 1.0))
    let s3 = intersection (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
    { camera = mkPinholeCamera (mkPoint 0.0 -2.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene [s3] [l5 ()] (ambientLight ()) 0}

  let render =
    List.map (Util.mkTarget "csg")
      [(renderUnion, "union");
       (renderUnion2, "union2");
       (renderUnion3, "union3");
       (renderIntersection, "intersection");
       (renderSubtraction, "subtraction");
       (renderCross, "cross");
       (rendertdCross, "2dCross");
       (renderCX, "cylinderX");
       (renderCY, "cylinderY");
       (renderCZ, "cylinderZ");
       (renderBox, "box");
       (renderSphere, "sphere");
       (renderIntersection2, "intersection2");
       (renderLantern, "lantern");
       (renderLantern2, "lantern2");
       (renderUnionTorus 3.0 0.5, "unionTorus");
       (renderIntersectionTorus 3.0 0.5, "intersectTorus");
       (renderSubtractionTorus 3.0 0.5, "subtractTorus");
       (renderUnionTorus2 3.0 0.5, "unionTorus2");
       (renderIntersectionTorus2 3.0 0.5, "intersectTorus2")]