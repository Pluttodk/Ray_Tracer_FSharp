namespace TracerTestSuite

open System
open System.Drawing
open Tracer.API
open Util

module Meshes =

  let renderIcosahedron () =
    let white = fromColor Color.White
    let baseShape = mkPLY "../../../ply/icosahedron.ply" false
    let shape = transform (mkShape baseShape (mkMatTexture (mkMatte white 1.0))) (scale 4. 4. 4.)
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
      scene = mkScene [shape] [l1; l2; l3] ambientLight 2}

  let renderUrn () =
    let white = fromColor Color.White
    let baseShape = mkPLY "../../../ply/urn2.ply" false
    let shape = transform (mkShape baseShape (mkMatTexture (mkMatte white 1.0))) (scale 4. 4. 4.)
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    let l1 = mkLight (mkPoint 0.0 100.0 0.0) white 0.5
    let l2 = mkLight (mkPoint 4.0 8.0 20.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    { camera = mkPinholeCamera (mkPoint 4.0 12.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
      scene = mkScene [shape] [l1; l2; l3] ambientLight 2}
  
  let renderInsideBunny () =
    let white = fromColor Color.White
    let baseShape = mkPLY "../../../ply/bunny.ply" true
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 40.0 40.0 40.0;
               translate 0.0 -1.5 0.0] in
    let shape = transform (mkShape baseShape (mkMatTexture (mkMatte white 1.0))) t
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    let l1 = mkLight (mkPoint 0.0 0.5 0.0) (fromColor Color.Red) 0.8
    let l2 = mkLight (mkPoint 0.0 -10.0 0.0) (fromColor Color.Blue) 0.9
    { camera = mkPinholeCamera (mkPoint 0.0 0.5 0.0) (mkPoint 0.0 100.0 0.0) (mkVector 1.0 0.0 0.0) 1.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
      scene = mkScene [shape] [l1;l2] ambientLight 2}

  let renderBunny () =
    let baseBunny = mkPLY "../../../ply/bunny.ply" true
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 40.0 40.0 40.0;
               translate 0.0 -1.5 0.0] in
    let white = fromColor Color.White
    let bunny = mkShape baseBunny (mkMatTexture (mkMatte white 1.0))
    let affineBunny = transform bunny t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.Green) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
      scene = mkScene [p;affineBunny] [l1; l2; l3] ambientLight 2}

  let renderBunnyAO () =
    let baseBunny = mkPLY "../../../ply/bunny.ply" true
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 40.0 40.0 40.0;
               translate 0.0 -1.5 0.0] in
    let white = fromColor Color.White
    let bunny = mkShape baseBunny (mkMatTexture (mkMatte white 1.0))
    let affineBunny = transform bunny t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.Green) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientOccluder (fromColor Color.White) 0.5 0.1 (mkMultiJitteredSampler 4 83)
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 3.25 2.5 1024 769 (mkMultiJitteredSampler 4 83);
      scene = mkScene [p;affineBunny] [] ambientLight 2}

  let renderBunnies (size : int) () =
    let baseBunny = mkPLY "../../../ply/bunny.ply" true
    let colours = 
      [| Color.White; Color.Blue; Color.Red; Color.Green; Color.Magenta; Color.Cyan;
         Color.Brown; Color.Gold; Color.LightSeaGreen; Color.SlateGray; Color.Fuchsia |]
    let refls = [|0.0 ; 0.5; 0.2; 0.15 ; 0.7; 0.2 ; 0.4|]
    let rots = [|0.0 ; 0.5; 0.8; 0.15 ; 0.2; 0.4 ; 0.7; 0.9; 0.75; 0.11|]
    let mkBunny x y = 
      let n = (x*size)+y
      let r = rots.[n%rots.Length]
      let f = refls.[n%refls.Length]
      let s1 = (r - f)
      let s2 = (f * r)
      let t = mergeTransformations
                [rotateY (2.0 * Math.PI * r);
                 scale 40.0 40.0 40.0;
                 translate (10. - float x * 15. + s1 * 5.) -1.5 (10. - float y * 15. + s2* 5.)] in
      let c = colours.[n% colours.Length]
      transform (mkShape baseBunny (mkMatTexture (mkMatte (fromColor c) 1.0))) t
    let bunnies = 
        [for x in 0 .. size - 1 do
          for y in 0 .. size - 1 do
            yield mkBunny x y]
    let l1 = mkLight (mkPoint 60.0 20.0 60.0) (fromColor Color.White) 0.5
    let l2 = mkLight (mkPoint -60.0 20.0 60.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -35. 120. 40.) (fromColor Color.White) 0.7
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.Green) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let p2 = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.White) 1.0 (fromColor Color.White) 0.5)))
              (mergeTransformations [(rotateX (System.Math.PI/2.0)); translate 0.0 200.0 0.0])
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 30.0 20.0 30.0) (mkPoint -10.0 0.0 -10.0) (mkVector 0.0 1.0 0.0) 4.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
      scene = mkScene (p :: bunnies) [l1; l2; l3] ambientLight 2}

  let renderSpheres (size : int) () =
    let colours = 
      [| Color.White; Color.Blue; Color.Red; Color.Green; Color.Magenta; Color.Cyan;
         Color.Brown; Color.Gold; Color.LightSeaGreen; Color.SlateGray; Color.Fuchsia |]
    let refls = [|0.0 ; 0.5; 0.2; 0.15 ; 0.7; 0.2 ; 0.4|]
    let rots = [|0.0 ; 0.5; 0.8; 0.15 ; 0.2; 0.4 ; 0.7; 0.9; 0.75; 0.11|]
    let mkSphere x y = 
      let n = (x*size)+y
      let r = rots.[n%rots.Length]
      let f = refls.[n%refls.Length]
      let s1 = (r - f)
      let s2 = (f * r)
      let c = colours.[n% colours.Length]
      mkSphere (mkPoint (2. - float x * 3. + s1 * 1.) (1.5 + float (n % 7) * 2.) (2. - float y * 3. + s2* 1.)) 1.5 (mkMatTexture (mkMatte (fromColor c) 1.0))
    let spheres = 
        [for x in 0 .. size - 1 do
          for y in 0 .. size - 1 do
            yield mkSphere x y]
    let l1 = mkLight (mkPoint 60.0 20.0 60.0) (fromColor Color.White) 0.5
    let l2 = mkLight (mkPoint -60.0 20.0 60.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -35. 120. 40.) (fromColor Color.White) 0.7
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.Green) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let p2 = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.White) 1.0 (fromColor Color.White) 0.5)))
              (mergeTransformations [(rotateX (System.Math.PI/2.0)); translate 0.0 200.0 0.0])
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 30.0 20.0 30.0) (mkPoint -10.0 0.0 -10.0) (mkVector 0.0 1.0 0.0) 4.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
      scene = mkScene (p :: spheres) [l1; l2; l3] ambientLight 2}

  let renderDragon () = 
    let baseDragon = mkPLY "../../../ply/dragon.ply" false
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 40.0 40.0 40.0;
               translate 0.0 -2.3 0.0] in
    let white = fromColor Color.White
    let dragon = mkShape baseDragon (mkMatTexture (mkMatte (fromColor Color.White) 1.0))
    let affineDragon = transform dragon t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Blue) 1.0 (fromColor Color.Blue) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Blue) 0.1
    { camera = mkPinholeCamera (mkPoint 2.0 6.0 12.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 4.0 5.33 4.0 1024 768 (mkRegularSampler 1);
      scene = mkScene [p; affineDragon] [l1; l2; l3] ambientLight 2}

  let renderGoldDragon numSamples () = 
    let baseDragon = mkPLY "../../../ply/dragon.ply" false
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 40.0 40.0 40.0;
               translate 0.0 -2.3 0.0] in
    let lemon = mkColour 1.0 1.0 0.3
    let orange = mkColour 1.0 0.75 0.5
    let white = mkColour 1.0 1.0 1.0
    let dragon = mkShape baseDragon (mkMatTexture (mkPhongGlossyReflectiveMaterial orange 0.1 lemon 0.9 white 0.4 white 0.3 10 10 (mkMultiJitteredSampler numSamples 83)))
    let affineDragon = transform dragon t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    let l3 = mkEnvironmentLight 1000000.0 (mkMatTexture (mkEmissive lemon 0.85)) (mkMultiJitteredSampler numSamples 83)
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Blue) 1.0 (fromColor Color.Blue) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Blue) 0.1
    { camera = mkPinholeCamera (mkPoint 2.0 6.0 12.0) (mkPoint 0.0 1.5 0.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 3.0 1024 768 (mkMultiJitteredSampler numSamples 83);
      scene = mkScene [p; affineDragon] [l1; l2; l3] ambientLight 2}

  let renderHappy () =
    let basehappy = mkPLY "../../../ply/happy.ply" false
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 50.0 50.0 50.0;
               translate 0.0 -2.0 1.0] in
    let white = fromColor Color.White
    let happy = mkShape basehappy (mkMatTexture (mkMatte white 1.0))
    let affinehappy = transform happy t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 0.7
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.White) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 20.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [p; affinehappy] [l1; l2; l3] ambientLight 2}

  let renderPorsche () =
    let baseporsche = mkPLY "../../../ply/porsche.ply" true
    let t = mergeTransformations
              [rotateY (-Math.PI / 4.0);
               scale 2.0 2.0 2.0;
               translate -2.0 4.5 0.0] in
    let white = fromColor Color.White
    let porsche = mkShape baseporsche (mkMatTexture (mkMatte white 1.0))
    let affineporsche = transform porsche t in
    let l1 = mkLight (mkPoint 12.0 4.0 14.0) white 0.4
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) (fromColor Color.Blue) 0.5
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.White) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 25.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [p; affineporsche] [l1; l2; l3] ambientLight 2}

  let renderHorse () =
    let basehorse = mkPLY "../../../ply/horse.ply" true
    let t = mergeTransformations
              [rotateY (-Math.PI / 4.0);
               scale 20.0 20.0 20.0;
               translate 0.0 6.0 0.0] in
    let white = fromColor Color.White
    let horse = mkShape basehorse (mkMatTexture (mkMatte white 1.0))
    let affinehorse = transform horse t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -8.5 40.0 4.0) white 1.0
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.White) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 25.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [p; affinehorse] [l1; l2; l3] ambientLight 2}

 
  let renderHead () =
    let baseShape = mkPLY "../../../ply/head1.ply" false
    let t = mergeTransformations
             [rotateX (-Math.PI / 2.0);
              rotateY (-Math.PI * 0.25);
              scale 60. 60. 60.;
              translate 0.0 3.0 0.0] in
    let white = fromColor Color.White
    let shape = mkShape baseShape (mkMatTexture (mkMatte white 1.0))
    let affineshape = transform shape t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -8.5 40.0 4.0) white 1.0
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 25.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [affineshape] [l1; l2; l3] ambientLight 2}

 
  let renderVase () =
    let baseshape = mkPLY "../../../ply/vase.ply" false
    let t = mergeTransformations
             [//rotateX (-Math.PI / 2.0);
              //rotateY (-Math.PI * 0.25);
              rotateX Math.PI;
              scale 0.04 0.04 0.04;
              translate 10.0 8.0 20.0] in
    let white = fromColor Color.White
    let shape = mkShape baseshape (mkMatTexture (mkMatte white 1.0))
    let affineshape = transform shape t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -8.5 40.0 4.0) white 1.0
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.White) 0.5)))
             (mergeTransformations [rotateX (System.Math.PI/2.0); translate 0. -4. 0.])
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 25.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [affineshape;p] [l1; l2; l3] ambientLight 2}

  let renderArmadillo () =
    let baseshape = mkPLY "../../../ply/armadillo.ply" true
    let t = mergeTransformations
              [rotateY Math.PI;
               scale 0.1 0.1 0.1;
               translate 0.0 5.3 0.0] in
    let white = fromColor Color.White
    let shape = mkShape baseshape (mkMatTexture (mkMatte white 1.0))
    let affineshape = transform shape t in
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -8.5 40.0 4.0) white 1.0
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Red) 1.0 (fromColor Color.White) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Blue) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 12.0 35.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000 (mkRegularSampler 1);
      scene = mkScene [affineshape;p] [l1; l2; l3] ambientLight 2}

  let renderBunnyTexture () =
    let baseBunny = mkPLY "../../../ply/bunny_textured.ply" true
    let t = mergeTransformations
              [rotateY (Math.PI / 4.0);
               scale 6.0 6.0 6.0;
               translate 0.0 3.0 0.0] in
    let white = fromColor Color.White
    let tex = mkTextureFromFile (fun x y -> (y,x)) "../../../textures/bunny.png"
    let bunny = mkShape baseBunny tex
    let affineBunny = transform bunny t in
    let t' = scale 0.5 0.5 0.5
    let l1 = mkLight (mkPoint 6.0 2.0 6.0) white 0.5
    let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
    let l3 = mkLight (mkPoint -3.5 12.0 4.0) white 1.0
    let p = transform (mkPlane (mkMatTexture (mkMatteReflective (fromColor Color.Green) 1.0 (fromColor Color.White) 0.5)))
              (rotateX (System.Math.PI/2.0))
    let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
    { camera = mkPinholeCamera (mkPoint 4.0 8.0 16.0) (mkPoint 0.0 0.5 0.0) (mkVector 0.0 1.0 0.0) 4.0 5.66 4.0 1024 768 (mkRegularSampler 1);
     scene = mkScene [p; affineBunny] [l1; l2; l3] ambientLight 2}

  let render (accel : Acceleration) : Target list =
    let grName = 
      match accel with
      | KDTree -> "meshes"
      | RegularGrid -> "meshes grid"
      | BVH -> "meshes BVH"
    let mkTarget (render, name) = 
      let render' () =
        setAcceleration accel
        render ()
      Util.mkTarget grName (render', name)
    List.map mkTarget
      [
       (renderIcosahedron, "icosahedron");
       (renderUrn, "urn");
       (renderBunny, "bunny");
       (renderBunnyTexture, "bunny texture");
       (renderBunnyAO, "bunnyAO");
       (renderInsideBunny, "inside bunny");
       (renderBunnies 7, "bunnies small");
       (renderBunnies 17, "bunnies big");
       (renderSpheres 17, "spheres small");
       (renderSpheres 47, "spheres big");
       (renderDragon, "dragon");
       (renderGoldDragon 4, "gold dragon");
       (renderHappy, "happy");
       (renderPorsche, "porsche");
       (renderHorse, "horse");
       (renderHead, "head")
       (renderVase, "vase")
       (renderArmadillo, "armadillo")
       ]