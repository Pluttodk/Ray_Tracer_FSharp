open Assert

[<EntryPoint>]
let main argv =
    let before = new System.TimeSpan()
    printfn("-=-=-=-=-=-=- BVH Test -=-=-=-=-=-=-")
    BVHTest.allTest
    printfn("-=-=-=-=-=-=- RegularGrid Test -=-=-=-=-=-=-")
    RegularGridsTest.allTest
    printfn("-=-=-=-=-=-=- Transformation Test -=-=-=-=-=-=-")
    TransformationTest.allTest
    printfn("-=-=-=-=-=-=- Shapes Test -=-=-=-=-=-=-")
    ShapeTest.allTest
    printfn "-=-=-=-=-=-=- Sampling Test -=-=-=-=-=-=-"
    SamplingTest.allTest
    printfn "-=-=-=-=-=-=- ExprParse Test (Implicit Surfaces) -=-=-=-=-=-=-"
    ExprParseTests.allTest
    printfn "-=-=-=-=-=-=- ExprToPoly Test (Implicit Surfaces) -=-=-=-=-=-=-"
    ExprToPolyTests.allTest
    printfn "-=-=-=-=-=-=- ExprToPoly Test v2 (Implicit Surfaces) -=-=-=-=-=-=-"
    ExprToPolyTests2.allTest
    printfn "-=-=-=-=-=-=- PolyToUnipoly Test (Implicit Surfaces) -=-=-=-=-=-=-"
    PolyToUnipolyTests.allTest
    printfn "-=-=-=-=-=-=- Implicit Surfaces -=-=-=-=-=-=-"
    ImplicitSurfacesTests.allTest

    printfn ""
    printfn "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    printfn ""

    let passed = new System.TimeSpan() - before
    printfn "%d/%d Tests passed. Duration: %d ms." 
        Assert.AmountPassed (Assert.AmountFailed+Assert.AmountPassed) passed.Milliseconds
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0