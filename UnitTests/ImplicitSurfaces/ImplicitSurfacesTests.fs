module ImplicitSurfacesTests

open Assert
open Tracer.ExprParse
open Tracer.ExprToPoly
open Tracer.ImplicitSurfaces
open Tracer.Basics
open Tracer.PolyToUnipoly
open Tracer.Basics

let allTest =
   
  // partial derivatives, first checking that its only done with respect to the given variable
  let test01 = 
    let input = "2*y^2*x^2*z^2"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "4*x*y^2*z^2"
    Assert.Equal (expected, actual, "derivative with respect to x")

  let test02 =
    let input = "2*y^2*x^2*z^2"
    let actual = (parseStr >> partialDerivative "y" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "4*x^2*y*z^2"
    Assert.Equal (expected, actual, "derivative with respect to y")

  let test03 =
    let input = "2*y^2*x^2*z^2"
    let actual = (parseStr >> partialDerivative "z" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "4*x^2*y^2*z"     
    Assert.Equal (expected, actual, "derivative with respect to z")

  // the following are based on the rules listed on page 42, in the notes, bottom of the page
  // case 1
  let test04 =
    let input = "2y^4"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "" // simpleExpr removes the 0
    Assert.Equal (expected, actual, "derivativeRule1: a * d/dx = 0")

  // case 2
  let test05 =
    let input = "x + 5"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "1"
    Assert.Equal (expected, actual, "derivativeRule2: x * d/dx = 1")

  // case 3
  let test06 =
    let input = "x^2 + x^3"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "2*x+3*x^2"
    Assert.Equal (expected, actual, "derivativeRule3: (e1 + e2) * d/dx = (e1 * d/dx) + (e2 * d/dx)")

  // case 4
  let test07 =
    let input = "x^2*y"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "2*x*y"
    Assert.Equal (expected, actual, "derivativeRule4: (e1*e2) * d/dx = e2 * (e1 * d/dx) + e1 * (e2 * d/dx)")

  // case 5
  let test08 =
    let input = "y^2 / x^2"
    // here we can't use exprToSimpleExpr, since it uses rewriteExpr, which removes division
    let actual = (parseStr >> partialDerivative "x" >> ppExpr) input
    let expected = "-1 * (y)^2 * 1 * 2 * (x)^1 / ((x)^2)^2"
    Assert.Equal (expected, actual, "derivativeRule5: (e1/e2) * d/dx = (e2 * (e1 * d/dx) - e1 * (e2 * d/dx)) / e2^2")
  
  // case 5
  let test09 =
    let input = "x^2 / y^2"
    // here we can't use exprToSimpleExpr, since it uses rewriteExpr, which removes division
    let actual = (parseStr >> partialDerivative "x" >> ppExpr) input
    let expected = "(y)^2 * 1 * 2 * (x)^1 / ((y)^2)^2" // no minus term, since that results in 0, because y * d/dx = 0 (case 1)
    Assert.Equal (expected, actual, "derivativeRule5: flipped x and y")

    // case 5
  let test10 =
    let input = "y^2 / x^2"
    // here we can't use exprToSimpleExpr, since it uses rewriteExpr, which removes division
    let actual = (parseStr >> partialDerivative "y" >> ppExpr) input
    let expected = "(x)^2 * 1 * 2 * (y)^1 / ((x)^2)^2"
    Assert.Equal (expected, actual, "derivativeRule5: with respect to y instead of x")
  
  // case 6
  let test11 =
    let input = "x^3"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "3*x^2"
    Assert.Equal (expected, actual, "derivativeRule6: e^n = (e * d/dx) * n * e^(n-1)")

  // case 6
  let test12 =
    let input = "y^3"
    let actual = (parseStr >> partialDerivative "x" >> exprToSimpleExpr >> ppSimpleExpr) input
    let expected = "" // simpleExpr removes the 0
    Assert.Equal (expected, actual, "derivativeRule6: y^3 = 0, when done with respect to d/dx")

  // case 7
  let test13 =
    let input = "(x^7)_3"
    let actual = (parseStr >> partialDerivative "x" >> ppExpr) input
    let expected = "1 * 7 * (x)^6 / 3 * (((x)^7)_3)^2"
    Assert.Equal (expected, actual, "derivativeRule7: e_n = (d * d/dx) / (n * ((e)_n)^(n-1))")

    // case 7
  let test14 =
    let input = "(x^7)_3"
    let actual = (parseStr >> partialDerivative "y" >> ppExpr) input // note the y
    let expected = "0" // 0 are still present in expr
    Assert.Equal (expected, actual, "derivativeRule7:with respect to different variable returns 0")

  // testing the polynomial derivative function
  let test15 =
    let input = parseStr "4*x^2 + z*x^3 + 4*x^5*z^2"
    let pol = exprToPoly input "z"
    let actual = ppPoly "z" pol
    let expected = "(4*x^2)+z(x^3)+z^2(4*x^5)"
    let actualDerivative = ppPoly "z" (polyDerivative pol)
    let expectedDerivative = "(x^3)+z(8*x^5)"
    Assert.Equal (expected, actual, "polyDerivative: regular poly")
    Assert.Equal (expectedDerivative, actualDerivative, "polyDerivative: changed to its derivative")
 
  // simple tests on newton-raphson
  let test16 =
    let input = toUnipoly (sepolyToSIEpoly (polyAsList ((exprToPoly << parseStr) "3x^2 - 3" "x"))) [|1.;1.;1.;1.;1.;1.|]
    let actual = newtonRaphson input (unipolyDerivative input) 0.1
    let expected = Some 1.0
    Assert.Equal (expected, actual, "newtonraphsontest: 3 * x^2 - 3 = 0, x = 1")

  let test17 =
    let input = toUnipoly (sepolyToSIEpoly (polyAsList ((exprToPoly << parseStr) "3x^2" "x"))) [|1.;1.;1.;1.;1.;1.|]
    let actual = newtonRaphson input (unipolyDerivative input) 0.1
    let expected = None
    Assert.Equal (expected, actual, "newtonraphsontest: 3 * x^2 = 0, no possible x")

  // tests on exprtopoly for simple shape equations
  let test18 = 
    let input = (parseStr >> substWithRayVars) "x^2 + y^2 + z^2 - r^2"
    let inputradical = (parseStr >> substWithRayVars) "(x^2 + y^2 + z^2)_2 - r"
    let actual = ppPoly "t" (exprToPoly input "t")
    let actualradical = ppPoly "t" (exprToPoly inputradical "t")
    let expected = "(-1*r^2+oz^2+oy^2+ox^2)+t(2*dz*oz+2*dy*oy+2*dx*ox)+t^2(dz^2+dy^2+dx^2)"
    Assert.Equal (expected, actual, "sphereTest1: sphere")
    Assert.Equal (expected, actualradical, "sphereTest2: sphere with radical")

  let test19 =
    let sphere = (mkImplicit "x^2 + y^2 + z^2 - 1.0").toShape (Textures.mkMatTexture( PhongMaterial(Colour.Black, 0.2, Colour.Black, 0.8, Colour.Black, 0.7, 100)))
    let actualInside = sphere.isInside (Point(0.,0.,0.))
    let actualOutside = sphere.isInside (Point(0.,0.,2.))
    let expectedInside = true
    let expectedOutside = false
    Assert.Equal (expectedInside, actualInside, "is.isInside true")
    Assert.Equal (expectedOutside, actualOutside, "is.isInside false")

  test01
  test02
  test03
  test04
  test05
  test06
  test07
  test08
  test09
  test10
  test11
  test12
  test13
  test14
  test15
  test16
  test17
  test18
  test19