module PolyToUnipolyTests

open Assert
open Tracer.PolyToUnipoly
open Tracer.ExprToPoly
open Tracer.ExprParse
open Tracer.ImplicitSurfaces

let allTest =

  let test01 =
      let p = toUnipoly (sepolyToSIEpoly (polyAsList ((parseStr >> exprToPoly) "3x^3 + 3x^2 + 5x + 1" "x"))) [|1.;1.;1.;1.;1.;1.|]
      let actual = unipolyDerivative p
      let expected = UP [(2, 9.0);(1, 6.0);(0, 5.0)]
      Assert.Equal (expected, actual, "reducedPolyDerivative: 3x^3 + 3x^2 + 5x + 1 ; p' = 9x^2 + 6x + 5")

  let test02 =
    let p = toUnipoly (sepolyToSIEpoly (polyAsList ((parseStr >> exprToPoly) " - 3x^3 - 3x^2 - 5x - 1" "x"))) [|1.;1.;1.;1.;1.;1.|]
    let actual = unipolyDerivative p
    let expected = UP [(2, -9.0);(1, -6.0);(0, -5.0)]
    Assert.Equal (expected, actual, "reducedPolyDerivative: -3x^3 - 3x^2 - 5x - 1 ; p' = -9x^2 - 6x - 5")

  let test03 =
    let up = toUnipoly (sepolyToSIEpoly (polyAsList ((parseStr >> exprToPoly) "x^4 + 2x^2 + 4x + 1" "x"))) [|1.;1.;1.;1.;1.;1.|]
    let actual = sturmSeq up (unipolyDerivative up)
    let expected = [UP [(0, (-92. / 81.) + 1.0)];
                          // Oh how I love floating points. The result will print as
                          //.Add(0, -0.1358024691)); which however doesn't equal itself
                    UP [(1, -36.0);(0, -16.0)]; 
                    UP [(2, -1.0);(1, -3.0);(0, -1.0)];
                    UP [(3, 4.0);(1, 4.0);(0, 4.0);];
                    UP [(4, 1.0);(2, 2.0);(1, 4.0);(0, 1.0)]]
    Assert.Equal (expected, actual, "simple Sturm seq")

  test01
  test02
  test03