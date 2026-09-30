module Program

open System
open System.Diagnostics
open Assert

let private suites: (string * (unit -> unit)) list =
    [ "bvh", BVHTest.allTest
      "regular-grids", RegularGridsTest.allTest
      "transformation", TransformationTest.allTest
      "shapes", ShapeTest.allTest
      "sampling", SamplingTest.allTest
      "bsdf", BsdfTests.allTest
      "path-transport", PathTransportTests.allTest
      "expr-parse", ExprParseTests.allTest
      "expr-to-poly", ExprToPolyTests.allTest
      "expr-to-poly-2", ExprToPolyTests2.allTest
      "poly-to-unipoly", PolyToUnipolyTests.allTest
      "implicit-surfaces", ImplicitSurfacesTests.allTest
      "image-io", ImageIOTests.allTest
      "acceleration", AccelerationRegressionTests.allTest
      "lighting-regression", LightingRegressionTests.allTest
      "geometry", GeometryRegressionTests.allTest
      "mesh", MeshRegressionTests.allTest
      "numerics", NumericalRegressionTests.allTest
      "motion", MotionTests.allTest
      "animation", AnimationTests.allTest
      "physics", PhysicsTests.allTest
      "gltf", GltfTests.allTest
      "film", FilmTests.allTest ]

let private usage () =
    printfn "Usage: dotnet run --project UnitTests -- [--suite NAME | --suites NAME,NAME]..."
    printfn "Without selection, all suites run. --list lists suites; --help shows this help."

let private selectedSuites (arguments: string[]) =
    let rec parse selected = function
        | [] -> List.rev selected
        | ("--suite" | "--suites") :: value :: rest ->
            let names = value.Split(',', StringSplitOptions.TrimEntries)
            if names |> Array.exists String.IsNullOrWhiteSpace then invalidArg "arguments" "Suite names cannot be empty."
            parse (List.rev (Array.toList names) @ selected) rest
        | option :: _ -> invalidArg "arguments" ("Invalid or incomplete option: " + option)
    let canonicalName (name: string) =
        match name.ToLowerInvariant() with
        | "lighting" -> "lighting-regression"
        | "numerical" -> "numerics"
        | value -> value
    let requested = parse [] (Array.toList arguments) |> List.map canonicalName |> List.distinct
    if List.isEmpty requested then suites
    else
        requested |> List.map (fun name ->
            match suites |> List.tryFind (fun (candidate, _) -> candidate = name) with
            | Some suite -> suite
            | None -> invalidArg "arguments" ("Unknown suite: " + name))

[<EntryPoint>]
let main argv =
    if argv = [| "--help" |] || argv = [| "-h" |] then
        usage ()
        0
    elif argv = [| "--list" |] then
        suites |> List.iter (fst >> printfn "%s")
        0
    else
        try
            let selected = selectedSuites argv
            Assert.Reset()
            let timer = Stopwatch.StartNew()
            for name, run in selected do
                printfn "-=-=-=-=-=-=- %s -=-=-=-=-=-=-" name
                try run ()
                with error -> Assert.Fail(sprintf "Suite %s threw: %O" name error)
            timer.Stop()
            printfn "%d/%d assertions passed across %d suites. Duration: %.3f ms."
                Assert.AmountPassed (Assert.AmountPassed + Assert.AmountFailed) selected.Length timer.Elapsed.TotalMilliseconds
            if Assert.AmountFailed = 0 then 0 else 1
        with :? ArgumentException as error ->
            eprintfn "%s" error.Message
            usage ()
            2
