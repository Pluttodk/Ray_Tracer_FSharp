namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module Transparency =

  let white () = mkMatte (fromColor Color.White) 1.
  let black () = mkMatte (fromColor Color.Black) 1.
  let blue () = mkMatte (fromColor Color.Blue) 1.
  let green () = mkMatte (fromColor Color.Green) 1.
  let yellow () = mkMatte (fromColor Color.Yellow) 1.
  let red () = mkMatte (fromColor Color.Red) 1.
  let pink () = mkMatte (fromColor Color.Pink) 1.
  let gray () = mkMatte (fromColor Color.Gray) 1.


  let checker c x y =
      let abs' f = if f < 0.0 then 1.0 - (f*16.0) else f * 16.0
      if (int (abs' x) + int (abs' y)) % 2 = 0
      then (white ())
      else c
  let checkerTex c = mkTexture (checker (c ())) 

  let renderSphere () =
    let light = mkLight (mkPoint 0.0 4.0 -2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let sphere = mkSphere (mkPoint 1.0 1.0 0.0) 1.0 (mkMatTexture (mkTransparent (mkColour 1. 0.5 0.5) (mkColour 1. 1. 1.) 1.5 1.0)) in
    let sphere' = mkSphere (mkPoint -0.5 1.0 -4.0) 1.0 (mkMatTexture (mkMatteReflective (mkColour 1. 0.5 0.5) 0.1 (mkColour 1. 0.5 0.5) 0.9)) in
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 0.0 2.9 8.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [sphere;sphere';box] [light] ambientLight 6}

  let renderBoxes () =
    let light = mkLight (mkPoint 0.0 11.0 0.0) (fromColor Color.White) 1. in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.3 in
    let glass = mkMatTexture (mkTransparent (mkColour 0.8 0.8 1.0) (mkColour 1. 1. 1.) 1.5 1.0)
    let (w,h,d) = (2.5,1.5,0.5)
    let baseBox = mkBox (mkPoint (-w/2.) 0.001 (-d/2.)) (mkPoint (w/2.) h (d/2.)) glass glass glass glass glass glass
    let (boxes,_) = List.mapFold (fun s b -> (transform b (translate (s*0.2) 0. s),s-1.)) 0. (List.replicate 4 baseBox)
    let sphere = mkSphere (mkPoint -0.5 1.0 -4.5) 1.0 (mkMatTexture (blue ())) in
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 2.0 3.9 10.0) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 1.5 1024 768 (mkRegularSampler 1);
      scene = mkScene ([sphere;box] @ boxes) [light] ambientLight 10}

  let renderBox () =
    let light = mkLight (mkPoint 0.0 11.0 0.0) (fromColor Color.White) 1. in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.3 in
    let glass = mkMatTexture (mkTransparent (mkColour 0.8 0.8 1.0) (mkColour 1. 1. 1.) 1.5 1.0)
    let (w,h,d) = (2.,2.,2.)
    let tbox = transform (mkBox (mkPoint (-w/2.) 0.001 (-d/2.)) (mkPoint (w/2.) h (d/2.)) glass glass glass glass glass glass)
                         (rotateY (Math.PI*0.25))
    let sphere = mkSphere (mkPoint -0.5 1.0 -4.5) 1.0 (mkMatTexture (blue ())) in
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 2.0 3.9 10.0) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 1.5 1024 768 (mkRegularSampler 1);
      scene = mkScene ([sphere;tbox;box]) [light] ambientLight 10}

  
  
  let renderWater () =
    let light = mkLight (mkPoint 0.0 4.0 2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let waterTex = mkMatTexture (mkTransparent (mkColour 0.6 0.6 1.) (mkColour 1. 1. 1.) 1.33 1.0)
    let water = mkSolidCylinder (mkPoint 0.0 1.01 0.0) 1.0 2.0 waterTex waterTex waterTex in
    let strawTex = mkMatTexture (mkMatte (fromColor Color.Red) 1. )
    let strawTrans = mergeTransformations [rotateZ 0.7; translate 0.6 0.3 0.0]
    let straw = transform (mkHollowCylinder (mkPoint 0.0 1.5 0.0) 0.1 3.0 strawTex) strawTrans
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 0.0 0.9 10.0) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [water;straw;box] [light] ambientLight 6}



  let renderFancyBunny numSamples () =
    let light = mkLight (mkPoint 0.0 8.0 -1.0) (fromColor Color.White) 1. in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let baseBunny = mkPLY "../../../ply/bunny.ply" true
    let t = mergeTransformations
              [scale 18.0 18.0 18.0;
               translate 0.5 -0.6 0.0]
    // normals point the wrong way in horse PLY; swap inside and outside for transparent material
    let bunny = transform (mkShape baseBunny (mkMatTexture (mkTransparent (mkColour 1. 1. 1.) (mkColour 0.8 0.7 0.9) 1. 1.5))) t
    let woodTex = Util.mkTextureFromFile (fun x y -> (x,y)) "../../../textures/wood-white.jpg"
    let platform = transform (mkDisk (mkPoint 0. 0. 0.) 3. woodTex) (rotateX (Math.PI * 0.5))
    let groundTex = Util.mkTextureFromFile (fun x y -> ((8.*x)%1.,(8.*y)%1.)) "../../../textures/gravel.jpg"
    let boxTex = mkMatTexture (mkMatte (fromColor Color.SkyBlue) 1.0)
    let box = mkBox (mkPoint -80. -5. -80.) (mkPoint 80. 12. 80.) boxTex boxTex boxTex groundTex boxTex boxTex
    { camera = mkThinLensCamera (mkPoint 0.0 5.1 8.0) (mkPoint 0.0 1.1 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.66 2.0 1024 768 0.05 8. 
            (mkMultiJitteredSampler numSamples 83) (mkMultiJitteredSampler numSamples 83)
      scene = mkScene [bunny;platform;box] [light] ambientLight 6}



  let renderBunny () =
    let light = mkLight (mkPoint 0.0 4.0 -2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let baseBunny = mkPLY "../../../ply/bunny10k.ply" true
    let t = mergeTransformations
              [scale 18.0 18.0 18.0;
               translate 0.5 -0.4 0.0]
    // normals point the wrong way in horse PLY; swap inside and outside for transparent material
    let bunny = transform (mkShape baseBunny (mkMatTexture (mkTransparent (mkColour 1. 1. 1.) (mkColour 0.2 0.1 0.0) 1. 1.15))) t
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 0.0 1.1 8.0) (mkPoint 0.0 1.1 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [bunny;box] [light] ambientLight 6}
 

  let renderPorsche () =
    let light = mkLight (mkPoint 0.0 4.0 -2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let basePLY = mkPLY "../../../ply/porsche.ply" true
    let t = mergeTransformations
              [scale 0.35 0.35 0.35;
               translate 0.0 0.8 0.0]
    let porsche = transform (mkShape basePLY (mkMatTexture (mkTransparent (mkColour 0.5 0.2 0.0) (mkColour 1. 1. 1.) 1.15 1.))) t
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 0.0 1.1 8.0) (mkPoint 0.0 1.1 0.0) (mkVector 0.0 1.0 0.0) 4.0 3.0 2.0 1200 800 (mkRegularSampler 1);
      scene = mkScene [porsche;box] [light] ambientLight 6}


  let renderUrn () =
    let light = mkLight (mkPoint 0.0 4.0 -2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let basePLY = mkPLY "../../../ply/urn2.ply" false
    let t = mergeTransformations
              [scale 0.9 0.9 0.9;
              translate 0. 1.0 0.]
    let urn = transform (mkShape basePLY (mkMatTexture (mkTransparent (mkColour 0.1 0.04 0.0) (mkColour 1. 1. 1.) 1.5 1.))) t
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 0.0 4.1 8.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [urn;box] [light] ambientLight 10}


  let renderHorse () =
    let checker c x y =
        let abs' f = if f < 0.0 then 1.0 - (f*16.0) else f * 16.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then white ()
        else c
    let checkerTex c = mkTexture (checker (c ()))
    let light = mkLight (mkPoint 0.0 4.0 -2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let basePLY = mkPLY "../../../ply/horse.ply" true
    let t = mergeTransformations
              [scale 4.0 4.0 4.0;
               translate 0.0 1.21 0.0]
    // normals point the wrong way in horse PLY; swap inside and outside for transparent material
    let horse = transform (mkShape basePLY (mkMatTexture (mkTransparent (mkColour 1. 1. 1.) (mkColour 0.1 0.04 0.0) 1. 1.15))) t
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)
    { camera = mkPinholeCamera (mkPoint 0.0 1.1 8.0) (mkPoint 0.0 1.1 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [horse;box] [light] ambientLight 6}

  let renderDie () =
    let glass = mkMatTexture (mkTransparent (mkColour 0.8 0.8 1.0) (mkColour 1. 1. 1.) 1.5 1.0)
    let glass' = mkMatTexture (mkTransparent (mkColour 1. 1. 1.) (mkColour 0.8 0.8 1.0) 1.0 1.5)
    let l3 = mkLight (mkPoint 0.0 0.0 0.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.5 in
    let one = mkSphere (mkPoint 0.0 1.0 0.0) 0.2 glass'
    let two1 = mkSphere (mkPoint 1.0 0.5 -0.5) 0.2 glass'
    let two2 = mkSphere (mkPoint 1.0 -0.5 0.5) 0.2 glass'
    let three1 = mkSphere (mkPoint 0.5 0.5 -1.0) 0.2 glass'
    let three2 = mkSphere (mkPoint 0.0 0.0 -1.0) 0.2 glass'
    let three3 = mkSphere (mkPoint -0.5 -0.5 -1.0) 0.2 glass'
    let four1 = mkSphere (mkPoint 0.5 0.5 1.0) 0.2 glass'
    let four2 = mkSphere (mkPoint -0.5 -0.5 1.0) 0.2 glass'
    let four3 = mkSphere (mkPoint -0.5 0.5 1.0) 0.2 glass'
    let four4 = mkSphere (mkPoint 0.5 -0.5 1.0) 0.2 glass'
    let five1 = mkSphere (mkPoint -1.0 0.0 0.0) 0.2 glass'
    let five2 = mkSphere (mkPoint -1.0 0.5 0.5) 0.2 glass'
    let five3 = mkSphere (mkPoint -1.0 0.5 -0.5) 0.2 glass'
    let five4 = mkSphere (mkPoint -1.0 -0.5 0.5) 0.2 glass'
    let five5 = mkSphere (mkPoint -1.0 -0.5 -0.5) 0.2 glass'
    let six1 = mkSphere (mkPoint 0.5 -1.0 0.5) 0.2 glass'
    let six2 = mkSphere (mkPoint 0.0 -1.0 0.5) 0.2 glass'
    let six3 = mkSphere (mkPoint -0.5 -1.0 0.5) 0.2 glass'
    let six4 = mkSphere (mkPoint 0.5 -1.0 -0.5) 0.2 glass'
    let six5 = mkSphere (mkPoint 0.0 -1.0 -0.5) 0.2 glass'
    let six6 = mkSphere (mkPoint -0.5 -1.0 -0.5) 0.2 glass'

    let dots = [one; 
                two1; two2; 
                three1; three2; three3; 
                four1; four2; four3; four4;
                five1; five2; five3; five4; five5;
                six1; six2; six3; six4; six5; six6]
    
    let ocube_base = Util.mkTexturedBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) glass
    let die = List.fold subtraction ocube_base dots


    let camera = mkPinholeCamera (mkPoint 16.0 16.0 16.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 8.0 1.4 1.4 500 500 (mkRegularSampler 1) in
    let box = mkBox (mkPoint -18. -18. -18.) (mkPoint 18. 18. 18.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)

    { scene = mkScene [die;box] [l3] ambientLight 6
      camera = camera}
  
  let renderLantern () =
    let mkUnitBox t = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) t t t t t t
    let mkUnitCylinder t = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 t t t
    let glass = mkMatTexture (mkTransparent (mkColour 0.8 0.8 1.0) (mkColour 1. 1. 1.) 1.3 1.0)

    let cube = mkUnitBox glass
    let sphere  = mkSphere (mkPoint 0.0 0.0 0.0) 1.3 glass
    let cy = transform (mkUnitCylinder glass) (scale 0.7 1.5 0.7)  in
    let cx = transform cy (rotateX (Util.degrees_to_radians 90.0)) in
    let cz = transform cy (rotateZ (Util.degrees_to_radians 90.0)) in 

    let l3 = mkLight (mkPoint 0.0 0.0 0.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.5 in
    let camera = mkPinholeCamera (mkPoint 16.0 16.0 16.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 8.0 1.4 1.4 500 500 (mkRegularSampler 4) in
    let box = mkBox (mkPoint -18. -18. -18.) (mkPoint 18. 18. 18.) (checkerTex blue) (checkerTex red) (checkerTex yellow) (checkerTex green) (checkerTex pink) (checkerTex black)

    let cross =
        union cy (union cz cx)
    { scene = mkScene [subtraction (intersection sphere cube) cross;box] [l3] ambientLight 6
      camera = camera}

  let renderWaterGlass () =
    let light = mkLight (mkPoint 0.0 4.0 2.0) (fromColor Color.White) 0.4 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.6 in
    let waterIOR = 1.33
    let glassIOR = 1.518
    let waterColour = mkColour 0.2 0.2 1.
    let glassColour = mkColour 0.3 1. 0.3
    let whiteTex = mkMatTexture (white ())
    let waterTex = mkMatTexture (mkTransparent waterColour (mkColour 1. 1. 1.) waterIOR 1.0)
    let glassTex = mkMatTexture (mkTransparent glassColour (mkColour 1. 1. 1.) glassIOR 1.0)
    let glassInvTex = mkMatTexture (mkTransparent (mkColour 1. 1. 1.) glassColour 1.0 glassIOR)
    let glassWaterTex = mkMatTexture (mkTransparent waterColour glassColour waterIOR glassIOR)
    let (height, radius, thickness, bottom, edge) = (2.,1.,0.09,0.2, 0.3)
    let waterHeight = height - edge - bottom
    let glass = subtraction 
                 (mkSolidCylinder (mkPoint 0.0 (0.001 + height/2.) 0.0) radius height glassTex glassTex glassTex)
                 (mkSolidCylinder (mkPoint 0.0 (0.002 + height - (edge/2.)) 0.0) (radius-thickness) (edge + 0.002) glassInvTex glassInvTex glassInvTex)
    let glassWater = group (subtraction glass (mkSolidCylinder (mkPoint 0.0 (bottom + waterHeight/2.)  0.0) (radius-thickness) (waterHeight + 0.003) glassWaterTex glassWaterTex glassWaterTex))
                       (transform (mkDisk (mkPoint 0.0 0.0 0.0) (radius - thickness) waterTex) (mergeTransformations [rotateX (-Math.PI/2.);translate 0. (bottom + waterHeight + 0.001) 0.]))
    
    let strawTex = mkMatTexture (mkMatte (fromColor Color.Red) 1. )
    let strawTrans = mergeTransformations [rotateZ 0.68; translate 0.62 0.27 0.0]
    let straw = transform (mkHollowCylinder (mkPoint 0.0 1.5 0.0) 0.1 3.0 strawTex) strawTrans
    let box = mkBox (mkPoint -12. 0. -10.) (mkPoint 12. 12. 10.) whiteTex whiteTex whiteTex (checkerTex gray) whiteTex whiteTex
    { camera = mkPinholeCamera (mkPoint 0.0 5.9 8.2) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [glassWater;straw;box] [light] ambientLight 10}


  let renderFancyWaterGlass numSamples () =
    let light = mkLight (mkPoint 1.0 8.0 -2.0) (fromColor Color.White) 0.9 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.7 in
    let waterIOR = 1.33
    let glassIOR = 1.518
    let waterColour = mkColour 0.2 0.2 1.
    let glassColour = mkColour 0.3 1. 0.3
    let whiteTex = mkMatTexture (white ())
    let waterTex = mkMatTexture (mkTransparent waterColour (mkColour 1. 1. 1.) waterIOR 1.0)
    let glassTex = mkMatTexture (mkTransparent glassColour (mkColour 1. 1. 1.) glassIOR 1.0)
    let glassInvTex = mkMatTexture (mkTransparent (mkColour 1. 1. 1.) glassColour 1.0 glassIOR)
    let glassWaterTex = mkMatTexture (mkTransparent waterColour glassColour waterIOR glassIOR)
    let (height, radius, thickness, bottom, edge) = (2.,1.,0.09,0.2, 0.3)
    let waterHeight = height - edge - bottom
    let glass = subtraction 
                 (mkSolidCylinder (mkPoint 0.0 (0.001 + height/2.) 0.0) radius height glassTex glassTex glassTex)
                 (mkSolidCylinder (mkPoint 0.0 (0.002 + height - (edge/2.)) 0.0) (radius-thickness) (edge + 0.002) glassInvTex glassInvTex glassInvTex)
    let glassWater = group (subtraction glass (mkSolidCylinder (mkPoint 0.0 (bottom + waterHeight/2.)  0.0) (radius-thickness) (waterHeight + 0.003) glassWaterTex glassWaterTex glassWaterTex))
                       (transform (mkDisk (mkPoint 0.0 0.0 0.0) (radius - thickness) waterTex) (mergeTransformations [rotateX (-Math.PI/2.);translate 0. (bottom + waterHeight + 0.001) 0.]))
    
    let strawTex = mkTexture (fun x y -> if (y*8. + x*2.) % 1. > 0.5 then white () else red ())
    let strawTrans = mergeTransformations [rotateZ 0.68; translate 0.62 0.27 0.0]
    let straw = transform (mkHollowCylinder (mkPoint 0.0 1.5 0.0) 0.1 3.0 strawTex) strawTrans

    let grassTex = Util.mkTextureFromFile (fun x y -> ((8.*x+0.5)%1.,(8.*y+0.5)%1.)) "../../../textures/grass.jpg"
    let woodTex = Util.mkTextureFromFile (fun x y -> (x,y)) "../../../textures/wood.jpg"
    let skyTex = mkMatTexture (mkMatte (fromColor Color.SkyBlue) 1.5)
    //let table = mkRectangle (mkPoint -9. 0. 9.) (mkPoint -9. 0. -9.) (mkPoint 9. 0. 9.) woodTex
    let table = transform (mkDisk (mkPoint 0. 0. 0.) 6. woodTex) (rotateX (Math.PI * 0.5))
    let box = mkBox (mkPoint -80. -5. -80.) (mkPoint 80. 12. 80.) skyTex skyTex skyTex grassTex skyTex skyTex
    { //camera = mkThinLensCamera (mkPoint 0.0 5.9 8.2) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.0 2.0 1000 1000 0.05 10.1 (mkRegularSampler 4) (mkRegularSampler 4);
      camera = mkPinholeCamera (mkPoint 0.0 5.9 8.2) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.66 2.0 1024 768 (if numSamples = 1 then mkRegularSampler 1 else mkMultiJitteredSampler numSamples 83)
      scene = mkScene [glassWater;straw;table;box] [light] ambientLight 10}

  let render =
    List.map (Util.mkTarget "transparency")
      [(renderSphere, "sphere");
       (renderBox, "box");
       (renderBoxes, "boxes");
       (renderBunny, "bunny");
       (renderUrn, "urn");
       (renderHorse, "horse");
       (renderPorsche, "porsche");
       (renderWater, "water");
       (renderLantern, "lantern");
       (renderDie, "die");
       (renderWaterGlass, "water glass");
       (renderFancyBunny 4, "fancy bunny");
       (renderFancyWaterGlass 4, "fancy water glass");
       ]