namespace TracerTestSuite
open System.Drawing
open Tracer.API
open Util

module ImplicitSurfaces =

  let mkScene' s = 
    let light = mkLight (mkPoint 4.0 2.0 4.0) (fromColor Color.White) 0.5
    let light2 = mkLight (mkPoint -4.0 2.0 4.0) (fromColor Color.White) 0.5
    let lights = [light; light2]
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    mkScene [s] lights ambientLight 0

  let sphere1 (r : float) numSamples () =
    let aqua = (fromColor Color.Aqua)
    let white = fromColor(Color.White)
    let s = mkShape (mkImplicit ("x^2 + y^2 + z^2 - " + (string (r * r)))) (mkMatTexture (mkPhongMaterial aqua 0.2 aqua 0.8 white 0.7 100))
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83)
      scene = mkScene' s }

  let sphere2 (r : float) () =
    let s = mkShape (mkImplicit ("(x^2 + y^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

  let planeX () =
    let s = mkShape (mkImplicit "x") (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    { camera = mkPinholeCamera (mkPoint 1.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 16.0 16.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

  let planeY () =
    let s = mkShape (mkImplicit "y") (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    { camera = mkPinholeCamera (mkPoint 0.0 1.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

  let planeZ () =
    let s = mkShape (mkImplicit "z") (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

// A torus takes two arguments, the inner and the outer radius of the torus (r and R respectively). 
//  The outer radius is the distance from the center of the torus to the center of the tube
//  The inner radius is the radius of the tube
  let torus (R : float) (r : float) () =
    let s = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

  let linkTorus numSamples () =
    let bs = mkImplicit ("(((x^2 + y^2)_2 - " + (string 2.0) + ")^2 + z^2)_2 - " + (string 0.3))
    let orange = fromColor(Color.Orange)
    let violet = fromColor(Color.Violet)
    let white = fromColor(Color.White)

    let t1 = transform 
                (mkShape bs (mkMatTexture (mkPhongReflectiveMaterial orange 0.2 orange 0.8 white 0.3 white 1.0 100)))
                (translate -1.5 0.0 0.0)
    let t2 = transform 
                (mkShape bs (mkMatTexture (mkPhongReflectiveMaterial violet 0.2 violet 0.8 white 0.3 white 1.0 100)))
                (mergeTransformations [rotateX 90.0; translate 1.5 0.0 0.0])
    { camera = mkPinholeCamera (mkPoint 0.0 0.0 6.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83)
      scene = mkScene' (group t1 t2)}

  let torus2 (R : float) (r : float) () =
    let rs1 = "(" + (string R) + "^2" + " + " + (string r) + "^2)"
    let rs2 = "(" + (string R) + "^2" + " - " + (string r) + "^2)"
    let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*" + rs1 + "*x^2"
    let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
    let sz = "z^4 - 2*" + rs1 + "*z^2"
    let sc = rs2 + "^2"
    let eqn = sx + " + " + sy + " + " + sz + " + " + sc 
    //let _ = printf "torus equation %s\n" eqn
    let s = mkShape (mkImplicit eqn) (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0))
    { camera = mkPinholeCamera (mkPoint 0.0 4.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}
      
  let testShape () =
    let is = mkImplicit "(x - 2)^2(x+2)^2 + (y - 2)^2(y+2)^2 + (z - 2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z - 10(x^2 + y^2 + z^2) + 22"
    let s = mkShape is (mkMatTexture (mkMatte (fromColor Color.Gold) 1.0))
    { camera = mkPinholeCamera (mkPoint 6.0 6.0 8.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 -1.0 0.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1);
      scene = mkScene' s}
  
  let heart () =
    let is = mkImplicit "(x^2 + (4.0/9.0)*y^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*y^2*z^3"
    let s = mkShape is (mkMatTexture (mkMatte (fromColor Color.DarkRed) 1.0))
    { camera = mkPinholeCamera (mkPoint 0.0 3.0 1.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

  let factorial x = 
    if x = 0 then 1 else
    let rec fac_aux a acc =
      if a >= x then
        a * acc
      else
        fac_aux (a + 1) (a * acc)
    fac_aux 1 x

  let comb a b = 
    let x = float (factorial a) in
    let y = float (factorial b) in
    let z = float (factorial (a - b)) in
      x / (y * z)

  let rec strSum n f : string =
    if n = 0 then
      f 0
    else
      f n + " + " + (strSum (n - 1) f)

  let chmutov degree () =       
    let T x = strSum (degree / 2) (fun (k : int) -> (string (comb degree (2 * k))) + " * (" + x + "^2 + -1.0)^" + (string k) + " * " + x + "^" + (string (degree - (2 * k))))
    let is = mkImplicit (T "x" + " + " + T "y" + " + " + T "z")
    let s = mkShape is (mkMatTexture (mkMatte (fromColor Color.Gold) 1.0))
    { camera = mkPinholeCamera (mkPoint 16.0 16.0 16.0) (mkPoint 0.0 -0.5 0.0) (mkVector -1.0 1.0 -1.0) 16.0 4.0 4.0 500 500 (mkRegularSampler 1)
      scene = mkScene' s}

  //let test = (T "x" + " + " + T "y" + " + " + T "z")

  let render =
    List.map (Util.mkTarget "implicitSurfaces")
      [(heart, "heart");
       (sphere1 1.0 4, "sphere1");
       (sphere2 1.0, "sphere2");
       (planeX, "planeX");
       (planeY, "planeY");
       (planeZ, "planeZ");
       (torus 1.5 0.5, "torus");
       (torus2 1.5 0.5, "torus2");
       (linkTorus 4, "linkTorus");
       (testShape, "testShape");
       (chmutov 2, "chmutov2");
       (chmutov 3, "chmutov3");
       (chmutov 4, "chmutov4");
       (chmutov 5, "chmutov5");
       (chmutov 6, "chmutov6")
       ]
