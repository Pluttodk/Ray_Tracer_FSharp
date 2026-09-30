module NumericalRegressionTests

open System
open Assert
open Tracer
open Tracer.Basics
open Tracer.ExprParse
open Tracer.PolyToUnipoly
open Tracer.ImplicitSurfaces

let private near tolerance expected actual name =
    Assert.True(Double.IsFinite actual && abs(expected-actual) <= tolerance, name)

let allTest() =
    let signedPoint = Point(2.,3.,0.)
    for expression, expected in
        [ "-2^2", 4.
          "(-2)^2", 4.
          "-(2^2)", -4.
          "(-x)^2", 4.
          "-(x^2)", -4.
          "(-2)^-3", -0.125
          "2^ -3", 0.125
          "-(-x)", 2. ] do
        near 1e-12 expected (solveExpr signedPoint (parseStr expression)) ("signed-primary-meaning-" + expression)

    let zero = UP []
    Assert.Equal(0.,solveUnipoly zero 17.,"empty-polynomial-evaluates-to-zero")
    Assert.Equal(zero,unipolyDerivative zero,"empty-polynomial-derivative")
    Assert.Equal([],sturmSeq zero zero,"empty-sturm-sequence")
    Assert.Equal(None,getInterval [] 0. 2. 20,"empty-sturm-interval")
    Assert.Equal([||],realRootsInInterval zero 0. 10.,"zero-polynomial-has-no-isolated-roots")
    Assert.Equal([|3.|],realRootsInInterval (UP [2,0.; 0,-6.; 1,2.]) 0. 10.,"zero-leading-term-reduces-degree")
    Assert.Equal(UP [], (UP [2,1.; 0,-1.]) % (UP [1,1.; 0,-1.]), "exact-polynomial-division-has-zero-remainder")
    let repeated = UP [2,1.; 1,-2.; 0,1.]
    let sequence = sturmSeq repeated (unipolyDerivative repeated)
    Assert.Equal(2,sequence.Length,"repeated-root-sturm-terminates")
    Assert.True((getInterval sequence 0. 2. 30).IsSome,"repeated-root-isolated")
    Assert.Equal([|1.|],realRootsInInterval repeated 0. 2.,"quadratic-repeated-root")
    Assert.Equal(Some 0.,newtonRaphson (UP [(2,1.)]) (UP [(1,2.)]) 0.,"newton-exact-stationary-root")
    let cubic = UP [3,1.; 2,-9.; 1,23.; 0,-15.]
    let cubicRoots = realRootsInInterval cubic 0. 10.
    Assert.Equal(3,cubicRoots.Length,"cubic-finds-all-positive-roots")
    Array.iter2 (fun expected actual -> near 1e-10 expected actual "cubic-root") [|1.;3.;5.|] cubicRoots
    let tiny = realRootsInInterval (UP [2,1.; 0,-1e-200]) 0. 1.
    Assert.Equal(1,tiny.Length,"small-root-retained")
    near 1e-110 1e-100 tiny.[0] "small-root-relative-precision"
    let quartic = UP [4,1.; 3,-404.; 2,61206.; 1,-4121204.; 0,104060385.]
    let distantRoots = realRootsInInterval quartic 0. (rootBound quartic)
    Assert.Equal(2,distantRoots.Length,"quartic-root-search-not-fixed-at-100")
    near 1e-5 99. distantRoots.[0] "quartic-near-root"
    near 1e-5 103. distantRoots.[1] "quartic-far-root"

    let texture = Textures.mkMatTexture Material.None
    let xRay = Ray(Point.Zero,Vector(1.,0.,0.))
    for expression in ["0"; "1"; "-1"] do
        let shape = (mkImplicit expression).toShape texture
        Assert.True(not ((shape.hitFunction xRay).DidHit),"constant-implicit-does-not-crash")
    let plane = (mkImplicit "y-2").toShape texture
    Assert.True(not ((plane.hitFunction xRay).DidHit),"parallel-linear-implicit-miss")
    near 1e-12 2. (plane.hitFunction(Ray(Point.Zero,Vector(0.,1.,0.)))).Time "linear-implicit-hit"
    let negativeCoefficient = (mkImplicit "-2*x+4").toShape texture
    near 1e-12 2. (negativeCoefficient.hitFunction xRay).Time "negative-literal-coefficient-implicit-hit"
    let degenerateQuadratic = (mkImplicit "x^2+y-2").toShape texture
    near 1e-12 2. (degenerateQuadratic.hitFunction(Ray(Point.Zero,Vector(0.,1.,0.)))).Time
        "quadratic-ray-degenerates-to-linear"
    let sphere = (mkImplicit "x^2+y^2+z^2-1").toShape texture
    let sphereHit = sphere.hitFunction xRay
    near 1e-12 1. sphereHit.Time "implicit-missing-linear-coefficient-is-zero"
    Assert.True(sphereHit.DidHit && not sphereHit.FrontFace,"implicit-inside-ray-retains-frontface")
    let tangent = sphere.hitFunction(Ray(Point(-2.,1.,0.),Vector(1.,0.,0.)))
    Assert.True(tangent.DidHit,"implicit-tangent-root")
    near 1e-12 2. tangent.Time "implicit-tangent-time"
    let equation = "(x-150)^4+y^2+z^2-1"
    let distant = (mkImplicit equation).toShape texture
    let distantHit = distant.hitFunction xRay
    Assert.True(distantHit.DidHit && distantHit.FrontFace,"unbounded-implicit-beyond-100")
    near 1e-8 149. distantHit.Time "original-expression-refines-expanded-polynomial"
    near 1e-8 151. ((box distant :?> IIntervalShape).HitWithin(xRay,distantHit.Time,infinity)).Time "native-implicit-interval-excludes-entry"
    let bounded = (mkBoundedImplicit equation (BBox(Point(148.,-2.,-2.),Point(152.,2.,2.)))).toShape texture
    near 1e-8 149. (bounded.hitFunction xRay).Time "bounded-implicit-uses-slab-interval"
    Assert.Equal(Point(148.,-2.,-2.),bounded.getBoundingBox().lowPoint,"bounded-implicit-bounds-exposed")
    let interval = (mkImplicitInInterval equation 150.5 200.).toShape texture
    near 1e-8 151. (interval.hitFunction xRay).Time "explicit-implicit-interval-selects-exit"
    let clipped = (mkImplicitInInterval equation 0. 145.).toShape texture
    Assert.True(not ((clipped.hitFunction xRay).DidHit),"explicit-implicit-interval-clips-only-when-requested")
    let radical = (mkImplicit "(x)_2+1").toShape texture
    Assert.True(not ((radical.hitFunction xRay).DidHit),"radical-extraneous-root-rejected")
    let rational = (mkImplicit "x/x").toShape texture
    Assert.True(not ((rational.hitFunction(Ray(Point(-1.,0.,0.),Vector(1.,0.,0.)))).DidHit),"division-pole-is-not-a-surface")
    near 1e-12 -2. (solveExpr (Point(-8.,0.,0.)) (parseStr "x_3")) "odd-root-of-negative-value"
    let oddRoot = (mkImplicit "(x)_3+2").toShape texture
    near 1e-8 2. (oddRoot.hitFunction(Ray(Point(-10.,0.,0.),Vector(1.,0.,0.)))).Time "odd-radical-implicit-hit"
    let identityRoot = (mkImplicit "(x)_1-2").toShape texture
    near 1e-10 2. (identityRoot.hitFunction xRay).Time "degree-one-root-does-not-loop"
    let largeConstant = (mkImplicit "x-3000000000").toShape texture
    near 1e-5 3000000000. (largeConstant.hitFunction xRay).Time "implicit-integer-literal-does-not-overflow-int32"
    Assert.Throws<ParseErrorException>((fun () -> mkImplicit "(0/0)+x" |> ignore),"invalid-constant-expression-terminates")
    Assert.Throws<ArgumentException>((fun () -> reduceExpr (FNum Double.NaN) |> ignore),"nan-expression-reduction-terminates")
    Assert.Throws<ArgumentException>((fun () ->
        Tracer.ExprToPoly.exprToSimpleExpr (FDiv(FNum 0.,FNum 0.)) |> ignore),"nan-polynomial-simplification-terminates")
    let rejectedRoot =
        try parseStr "x_0" |> ignore; false
        with ParseErrorException -> true
    Assert.True(rejectedRoot,"zero-degree-root-rejected")
    let apiBounded = (API.mkBoundedImplicit equation (Point(148.,-2.,-2.)) (Point(152.,2.,2.))).toShape texture
    near 1e-8 149. (apiBounded.hitFunction xRay).Time "bounded-implicit-public-api"
