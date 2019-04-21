module ExprToPolyTests

  open Tracer.ExprParse
  open Tracer.ExprToPoly
  open Assert

  // All of these tests are based on the ones given in the Functional Programming course
  let allTest =

    let test01 = 
      let ex01 =
        FAdd(FAdd(FAdd(FExponent(FVar "x",2),
                       FExponent(FVar "y",2)),
                  FExponent(FVar "z",2)),
             FMult(FNum -1.0,FNum 1.0))
      let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
      let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
      let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))
      let ex01Subst = List.fold subst ex01 [("x",ex);("y",ey);("z",ez)]
      let pd = exprToPoly ex01Subst "t"
      Assert.Equal ("(pz^2+py^2+px^2+-1)+t(2*dz*pz+2*dy*py+2*dx*px)+t^2(dz^2+dy^2+dx^2)", (ppPoly "t" pd), "TestPoly01")

    let test02 =
      let e = FAdd(FAdd(FAdd(FExponent(FVar "x",2),
                             FExponent(FVar "y",2)),
                        FExponent(FVar "z",2)),
                   FAdd(FNum -1.0,FNum 1.0))
      let px = exprToPoly e "x"
      Assert.Equal ("(z^2+y^2)+x^2", (ppPoly "x" px), "TestPoly02")

    let test03 =
      let plane = FAdd (FMult (FVar "a", FVar "x"), FAdd (FMult (FVar "b", FVar "y"), FAdd (FMult (FVar "c", FVar "z"), FVar "d")))
      let planeStr = parseStr "a*x+b*y+c*z+d"
      let ex = FAdd (FVar "ox", FMult (FVar "t", FVar "dx"))
      let ey = FAdd (FVar "oy", FMult (FVar "t", FVar "dy"))
      let ez = FAdd (FVar "oz", FMult (FVar "t", FVar "dz"))
      let planeSubst = List.fold subst plane [("x",ex);("y",ey);("z",ez)]
      let planeStrSubst = List.fold subst planeStr [("x",ex);("y",ey);("z",ez)]
      let plane_d = exprToPoly planeSubst "t"
      let planeStr_d = exprToPoly planeStrSubst "t"

      let circle = FAdd (FExponent (FVar "x", 2) ,
                         FAdd (FExponent (FVar "y", 2),
                               FAdd (FExponent (FVar "z", 2),
                                     FMult (FNum -1.0, FExponent (FVar "r", 2))))) 
      let circleStr = parseStr "x^2+y^2+z^2+-1r^2"
      let circleSubst = List.fold subst circle [("x",ex);("y",ey);("z",ez)]
      let circleStrSubst = List.fold subst circleStr [("x",ex);("y",ey);("z",ez)]  
      let circle_d = exprToPoly circleSubst "t"
      let circleStr_d = exprToPoly circleSubst "t"    
      let testlist = [("TestPoly03",plane_d,"(d+c*oz+b*oy+a*ox)+t(c*dz+b*dy+a*dx)");
                      ("TestPoly04",planeStr_d,"(d+c*oz+b*oy+a*ox)+t(c*dz+b*dy+a*dx)");
                      ("TestPoly05",circle_d,"(-1*r^2+oz^2+oy^2+ox^2)+t(2*dz*oz+2*dy*oy+2*dx*ox)+t^2(dz^2+dy^2+dx^2)");
                      ("TestPoly06",circleStr_d,"(-1*r^2+oz^2+oy^2+ox^2)+t(2*dz*oz+2*dy*oy+2*dx*ox)+t^2(dz^2+dy^2+dx^2)")]
      Assert.EqualMany testlist (ppPoly "t")

    let test04 =
      let es = List.map (fun n -> FExponent (FAdd (FVar "a", FVar "b"), n)) [1 .. 5]
      let esStr = List.map (fun n -> parseStr ("(a+b)^"+((string)n))) [1 .. 5]
      let es2 = List.map (fun n -> FExponent (FAdd (FVar "a", FAdd(FVar "b", FVar "c")),n)) [1 .. 5]
      let es2Str = List.map (fun n -> parseStr ("(a+b+c)^"+((string)n))) [1 .. 5]
      let rs s = [("TestSimplify01"+s,"a+b");
                  ("TestSimplify02"+s,"2*a*b+a^2+b^2");
                  ("TestSimplify03"+s,"3*a*b^2+3*a^2*b+a^3+b^3");
                  ("TestSimplify04"+s,"4*a*b^3+6*a^2*b^2+4*a^3*b+a^4+b^4");
                  ("TestSimplify05"+s,"5*a*b^4+10*a^2*b^3+10*a^3*b^2+5*a^4*b+a^5+b^5")]
      let rs2 s = [("TestSimplify06"+s,"a+b+c");
                   ("TestSimplify07"+s,"2*a*b+2*a*c+a^2+2*b*c+b^2+c^2");
                   ("TestSimplify08"+s,"6*a*b*c+3*a*b^2+3*a*c^2+3*a^2*b+3*a^2*c+a^3+3*b*c^2+3*b^2*c+b^3+c^3");
                   ("TestSimplify09"+s,"12*a*b*c^2+12*a*b^2*c+4*a*b^3+4*a*c^3+12*a^2*b*c+6*a^2*b^2+6*a^2*c^2+4*a^3*b+4*a^3*c+a^4+4*b*c^3+6*b^2*c^2+4*b^3*c+b^4+c^4");
                   ("TestSimplify10"+s,"20*a*b*c^3+30*a*b^2*c^2+20*a*b^3*c+5*a*b^4+5*a*c^4+30*a^2*b*c^2+30*a^2*b^2*c+10*a^2*b^3+10*a^2*c^3+20*a^3*b*c+10*a^3*b^2+10*a^3*c^2+5*a^4*b+5*a^4*c+a^5+5*b*c^4+10*b^2*c^3+10*b^3*c^2+5*b^4*c+b^5+c^5")]
      let allEs = List.zip (rs "") es
      let allEsStr = List.zip (rs "Parse") esStr
      let allEs2 = List.zip (rs2 "") es2
      let allEs2Str = List.zip (rs2 "Parse") es2Str
      let genChk ((n,r),e) = (n,(exprToSimpleExpr >> ppSimpleExpr) e,r)
      let testlist = List.map genChk (allEs @ allEsStr @ allEs2 @ allEs2Str)
      Assert.EqualMany testlist (parseStr >> exprToSimpleExpr >> ppSimpleExpr)
     
    let test05 =
      let sphere = FAdd(FAdd(FAdd(FExponent(FVar "x",2),
                                  FExponent(FVar "y",2)),
                             FExponent(FVar "z",2)),
                        FMult(FNum -1.0,FVar "R"))
      let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
      let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
      let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))
      let eR = FNum -1.0
      let sphereSubst = List.fold subst sphere [("x",ex);("y",ey);("z",ez);("R",eR)]
      let sphereSE = exprToSimpleExpr sphereSubst
      Assert.Equal ((FAdd (FAdd (FAdd (FExponent (FAdd (FVar "px",FMult (FVar "t",FVar "dx")),2),
                                                     FExponent (FAdd (FVar "py",FMult (FVar "t",FVar "dy")),2)),
                                               FExponent (FAdd (FVar "pz",FMult (FVar "t",FVar "dz")),2)),
                                         FMult (FNum -1.0,FNum -1.0))), sphereSubst, "TestSphere01")
      Assert.Equal ((SE [[ANum 1.0];
                                 [ANum 2.0; AExponent ("dx",1); AExponent ("px",1); AExponent ("t",1)];
                                 [AExponent ("dx",2); AExponent ("t",2)];
                                 [ANum 2.0; AExponent ("dy",1); AExponent ("py",1); AExponent ("t",1)];
                                 [AExponent ("dy",2); AExponent ("t",2)];
                                 [ANum 2.0; AExponent ("dz",1); AExponent ("pz",1); AExponent ("t",1)];
                                 [AExponent ("dz",2); AExponent ("t",2)]; [AExponent ("px",2)];
                                 [AExponent ("py",2)]; [AExponent ("pz",2)]]), sphereSE, "TestSphere02")
     
    let test06 = 
      let actual = simplifyAtomGroup [AExponent ("px",1); AExponent ("px",2); ANum -2.0; ANum -2.0]
      let expected = [ANum 4.0; AExponent ("px",3)]
      Assert.Equal (expected, actual, "TestSE01")

    let test07 = 
      let actual = simplifySimpleExpr (SE [[ANum 3.0];[ANum 4.0];[AExponent("x",2);AExponent("y",3)];[AExponent("x",2); AExponent("y",3)]])
      let expected = SE [[ANum 7.0]; [ANum 2.0; AExponent ("x",2); AExponent ("y",3)]]
      Assert.Equal (expected, actual, "TestSE02")

    let test08 =
      let actual = (parseStr >> exprToSimpleExpr >> simplifySimpleExpr >> ppSimpleExpr) "a*b+b*a"
      let expected = "2*a*b"
      Assert.Equal (expected, actual, "TestSimplify11")
 
    test01 
    test02
    test03
    test04
    test05
    test06
    test07
    test08