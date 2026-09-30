namespace Tracer.SceneAssets

open System
open System.IO
open Tracer.SceneFormat

module Checks =
    let private require condition message = if not condition then failwith message

    let private rejects name action =
        let rejected =
            try action(); false
            with :? InvalidDataException -> true
        require rejected $"Geometry validation did not reject {name}."

    let run (meshes: Mesh array) (scenes: SceneSpec array) =
        let sphere = meshes |> Array.find (fun mesh -> mesh.Id = "small-sphere")
        let check name mesh = rejects name (fun () -> Meshes.validate mesh |> ignore)
        check "an open hole" { sphere with Triangles = sphere.Triangles.[1..] }
        let flipped = Array.copy sphere.Triangles
        let a, b, c = flipped.[0]
        flipped.[0] <- a, c, b
        check "a reversed face" { sphere with Triangles = flipped }
        let invalidIndex = Array.copy sphere.Triangles
        invalidIndex.[0] <- a, b, sphere.Vertices.Length
        check "an out-of-range index" { sphere with Triangles = invalidIndex }
        let repeatedIndex = Array.copy sphere.Triangles
        repeatedIndex.[0] <- a, a, c
        check "a degenerate indexed face" { sphere with Triangles = repeatedIndex }
        let mutateFirst change =
            let vertices = Array.copy sphere.Vertices
            vertices.[0] <- change vertices.[0]
            { sphere with Vertices = vertices }
        check "a non-finite position" (mutateFirst (fun v -> { v with Position = { v.Position with X = Double.NaN } }))
        check "a non-unit normal" (mutateFirst (fun v -> { v with Normal = V3.zero }))
        check "a non-finite UV" (mutateFirst (fun v -> { v with U = Double.PositiveInfinity }))
        check "inward shading normals" { sphere with Vertices = sphere.Vertices |> Array.map (fun v -> { v with Normal = V3.scale -1. v.Normal }) }
        for x, y, z in [ 1., 0., 0.; -1., 0., 0.; 0., 1., 0.; 0., -1., 0.; 0., 0., 1.; 0., 0., -1.; 1., 2., 3. ] do
            let a, b = V3.create 0.3 -0.7 0.4, V3.create x y z
            let transform = Matrix.between a b 0.2 0.3
            require (V3.length (V3.sub (Matrix.point transform (V3.create 0. -1. 0.)) a) < 1e-12) "Segment transform moved its lower endpoint."
            require (V3.length (V3.sub (Matrix.point transform (V3.create 0. 1. 0.)) b) < 1e-12) "Segment transform moved its upper endpoint."
        for scene in scenes do
            require (scene.Camera.ViewWidth = scene.Camera.ViewHeight && scene.Camera.ViewDistance = 1.) $"{scene.Id}: expected square benchmark camera."
            let vector (components: float array) = V3.create components.[0] components.[1] components.[2]
            let eye, target = vector scene.Camera.Position, vector scene.Camera.Target
            let forward = V3.sub target eye |> V3.unit
            let right = V3.cross forward (vector scene.Camera.Up) |> V3.unit
            let up = V3.cross right forward |> V3.unit
            let byId = meshes |> Array.map (fun mesh -> mesh.Id, mesh) |> Map.ofArray
            let subjects = Set.ofArray scene.Subjects
            for instance in scene.Objects do
                let m = instance.Transform
                let determinant =
                    m.[0] * (m.[5] * m.[10] - m.[6] * m.[9])
                    - m.[1] * (m.[4] * m.[10] - m.[6] * m.[8])
                    + m.[2] * (m.[4] * m.[9] - m.[5] * m.[8])
                require (determinant > 0.) $"{scene.Id}/{instance.Id}: asset placement must preserve outward winding."
                if subjects.Contains instance.Id then
                    for vertex in byId.[instance.Mesh].Vertices do
                        let relative = V3.sub (Matrix.point m vertex.Position) eye
                        let depth = V3.dot relative forward
                        require (depth > 0.) $"{scene.Id}/{instance.Id}: subject passes behind its camera."
                        let x = 2. * V3.dot relative right / (depth * scene.Camera.ViewWidth)
                        let y = 2. * V3.dot relative up / (depth * scene.Camera.ViewHeight)
                        require (abs x < 0.96 && abs y < 0.96) $"{scene.Id}/{instance.Id}: subject is clipped or has no framing margin."
        printfn "Passed invalid-solid rejection, row-major placement and square-camera framing checks."
