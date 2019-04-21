module RegularGridsTest

open Tracer.Basics.RegularGrids
open Assert

let allTest = 
    // Used for debug, will print to console etc. 
    // ----------------------------- Common data -----------------------------


    // ----------------------------- TEST BEGIN -----------------------------
    let clampTest_X_lessThan_0_Return_0 = 
        let expected = 0.0
        let result = clamp (-1.0, 5)
        Assert.Equal (expected, result, "clampTest_X_lessThan_0_Return_0")
    clampTest_X_lessThan_0_Return_0

    // ----------------------------- TEST BEGIN -----------------------------
    let clampTest_X_largerThan_B_Return_B =
        let expected = 5.0
        let result = clamp (10.0, 5)
        Assert.Equal (expected, result, "clampTest_X_largerThan_B_Return_B")
    clampTest_X_largerThan_B_Return_B

    // ----------------------------- TEST BEGIN -----------------------------
    let clampTest_OtherWise_Return_X =
        let expected = 4.0
        let result = clamp (4.0, 5)
        Assert.Equal (expected, result, "clampTest_OtherWise_Return_X")
    clampTest_OtherWise_Return_X
        