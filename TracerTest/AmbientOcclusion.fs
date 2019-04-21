namespace TracerTestSuite

open Tracer.API
open System
open System.Drawing
open Util

module AmbientOcclusion =

  let renderSphere s1 s2 () =
    let ambientLight = mkAmbientOccluder (fromColor Color.White) 1.0 0.1 s1 in
    let sphere = mkSphere (mkPoint 0.0 1.0 0.0) 1.0 (mkMatTexture (mkMatte (fromColor Color.Blue) 1.0)) in
    let plane = transform (mkPlane (mkMatTexture (mkMatteMaterial (fromColor Color.Gray) 1.0 (fromColor Color.Gray) 1.0)))
                  (rotateX (System.Math.PI/2.0)) in
    { camera = mkPinholeCamera (mkPoint 0.0 8.0 12.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 3.0 2.0 2.0 500 500 s2;
      scene = mkScene [sphere; plane] [] ambientLight 0 }

  let render num_samples =
    List.map (Util.mkTarget "ambientOcclusion")
       ((List.map (fun x -> ((fun () -> renderSphere (mkRegularSampler x) (mkRegularSampler x) ()), 
                             "ao_sphere_regular_" + (string(x * x)))) num_samples) @
        (List.map (fun x -> ((fun () -> renderSphere (mkRandomSampler (x * x) 83) (mkRandomSampler (x * x) 83) ()), 
                             "ao_sphere_random_" + (string(x * x)))) num_samples) @
        (List.map (fun x -> ((fun () -> renderSphere (mkJitteredSampler x 83) (mkJitteredSampler x 83) ()), 
                             "ao_sphere_jittered_" + (string(x * x)))) num_samples) @
        (List.map (fun x -> ((fun () -> renderSphere (mkNRooksSampler (x * x) 83) (mkNRooksSampler (x * x) 83) ()), 
                             "ao_sphere_nrooks_" + (string(x * x)))) num_samples) @
        (List.map (fun x -> ((fun () -> renderSphere (mkMultiJitteredSampler x 83) (mkMultiJitteredSampler x 83) ()), 
                             "ao_sphere_multi_jittered_" + (string(x * x)))) num_samples))