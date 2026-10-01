module TerrainTests

open System
open Assert
open Tracer.Basics
open Tracer.Animation

let private settings : Terrain.Settings =
    { Size = 400.; Resolution = 201; Relief = 40.; FeatureSize = 400.
      Peaks = [ { X = 0.; Z = 0.; Height = 100.; Radius = 90. } ]
      SnowLine = 100.; Seed = 3 }

let private grid () = Terrain.heightGrid settings (Terrain.height settings)

let normalTests () =
    let step = settings.Size / float (settings.Resolution - 1)
    let normals = Terrain.gridNormals step (grid ())
    Assert.True (normals |> Array.forall (fun n -> abs (n.Magnitude - 1.) < 1e-9 && n.Y > 0.), "terrain normals are unit length and face up")
    let n = settings.Resolution
    let mutable worst = 0.
    for j in 0 .. n - 1 do
        for i in 0 .. n - 2 do
            worst <- max worst (acos (min 1. (normals.[j * n + i] * normals.[j * n + i + 1])))
            if j < n - 1 then worst <- max worst (acos (min 1. (normals.[j * n + i] * normals.[(j + 1) * n + i])))
    Assert.True (worst < 0.8, "terrain normals vary continuously between neighbouring vertices")

let determinismTests () =
    Assert.True (grid () = grid (), "the terrain height grid is deterministic")
    Assert.True (Terrain.height settings 12.5 -7.25 = Terrain.height settings 12.5 -7.25, "terrain height is deterministic")
    let other = Terrain.heightGrid { settings with Seed = 4 } (Terrain.height { settings with Seed = 4 })
    Assert.True (grid () <> other, "a different seed gives a different terrain")
    let c1, s1 = Terrain.surfaceAt settings 10. 20. 50. 0.2
    let c2, s2 = Terrain.surfaceAt settings 10. 20. 50. 0.2
    Assert.True (c1 = c2 && s1 = s2, "the procedural surface is deterministic")

let allTest () =
    normalTests ()
    determinismTests ()
