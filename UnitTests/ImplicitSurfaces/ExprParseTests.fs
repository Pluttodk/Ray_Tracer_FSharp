module ExprParseTests

open Assert
open Tracer.ExprParse

// All these tests are based on, without shame, ones given in the Functional Programming, Spring 2018, course.
let allTest = 

  let testScan= 
    let testlist =
      [("TestScan01","2*3",[Int 2; Mul; Int 3]);
        ("TestScan02","-2*3",[Int -2; Mul; Int 3]);
        ("TestScan03","-2*-3",[Int -2; Mul; Int -3]);
        ("TestScan04","2+3*4",[Int 2; Add; Int 3; Mul; Int 4]);
        ("TestScan05","2+-3*-4",[Int 2; Add; Int -3; Mul; Int -4]);
        ("TestScan06","(2.0+3)*4",[Lpar; Float 2.0; Add; Int 3; Rpar; Mul; Int 4]);
        ("TestScan07","-1(2+3)",[Int -1; Lpar; Int 2; Add; Int 3; Rpar]);
        ("TestScan08","2^4",[Int 2; Pwr; Int 4]);
        ("TestScan09","x^2",[Var "x"; Pwr; Int 2]);
        ("TestScan10","x2",[Var "x2"]);
        ("TestScan11","2x",[Int 2; Var "x"]);
        ("TestScan12","-1x",[Int -1; Var "x"]);
        ("TestScan13","2 x",[Int 2; Var "x"]);
        ("TestScan14","2 x y",[Int 2; Var "x"; Var "y"]);
        ("TestScan15","2xy",[Int 2; Var "xy"]);
        ("TestScan16","2x(2x)",[Int 2; Var "x"; Lpar; Int 2; Var "x"; Rpar]);
        ("TestScan17","2 x 2 y(2 x(-2))",[Int 2; Var "x"; Int 2; Var "y"; Lpar; Int 2; Var "x"; Lpar; Int -2; Rpar; Rpar]);
        ("TestScan18","2x^2*2y^2",[Int 2; Var "x"; Pwr; Int 2; Mul; Int 2; Var "y"; Pwr; Int 2]);
        ("TestScan19","2x^2(2y^2)",[Int 2; Var "x"; Pwr; Int 2; Lpar; Int 2; Var "y"; Pwr; Int 2; Rpar])]
    Assert.EqualMany testlist scan

  let testInsertMult =
    let testlist =
      [("TestInsertMult01","2*3",[Int 2; Mul; Int 3]);
        ("TestInsertMult02","-2*3",[Int -2; Mul; Int 3]);
        ("TestInsertMult03","-2*-3",[Int -2; Mul; Int -3]);
        ("TestInsertMult04","2+3*4",[Int 2; Add; Int 3; Mul; Int 4]);
        ("TestInsertMult05","2+-3*-4",[Int 2; Add; Int -3; Mul; Int -4]);
        ("TestInsertMult06","(2.0+3)*4",[Lpar; Float 2.0; Add; Int 3; Rpar; Mul; Int 4]);
        ("TestInsertMult07","-1(2+3)",[Int -1; Mul; Lpar; Int 2; Add; Int 3; Rpar]);
        ("TestInsertMult08","2^4",[Int 2; Pwr; Int 4]);
        ("TestInsertMult09","x^2",[Var "x"; Pwr; Int 2]);
        ("TestInsertMult10","x2",[Var "x2"]);
        ("TestInsertMult11","2x",[Int 2; Mul; Var "x"]);
        ("TestInsertMult12","-1x",[Int -1; Mul; Var "x"]);
        ("TestInsertMult13","2 x",[Int 2; Mul; Var "x"]);
        ("TestInsertMult14","2 x y",[Int 2; Mul; Var "x"; Mul; Var "y"]);
        ("TestInsertMult15","2xy",[Int 2; Mul; Var "xy"]);
        ("TestInsertMult16","2x(2x)",[Int 2; Mul; Var "x"; Mul; Lpar; Int 2; Mul; Var "x"; Rpar]);
        ("TestInsertMult17","2 x 2 y(2 x(-2))",[Int 2; Mul; Var "x"; Mul; Int 2; Mul; Var "y"; Mul; Lpar; Int 2; Mul; Var "x"; Mul; Lpar; Int -2; Rpar; Rpar]);
        ("TestInsertMult18","2x^2*2y^2",[Int 2; Mul; Var "x"; Pwr; Int 2; Mul; Int 2; Mul; Var "y"; Pwr; Int 2]);
        ("TestInsertMult19","2x^2(2y^2)",[Int 2; Mul; Var "x"; Pwr; Int 2; Mul; Lpar; Int 2; Mul; Var "y"; Pwr; Int 2; Rpar])]
    Assert.EqualMany testlist (scan >> insertMult)

  let testParse =
    let testlist = 
      [("TestParse01","2*3",FMult (FNum 2.0,FNum 3.0));
        ("TestParse02","-2*3",FMult (FNum -2.0,FNum 3.0));
        ("TestParse03","-2*-3",FMult (FNum -2.0,FNum -3.0));
        ("TestParse04","2+3*4",FAdd (FNum 2.0,FMult (FNum 3.0,FNum 4.0)));
        ("TestParse05","2+-3*-4",FAdd (FNum 2.0,FMult (FNum -3.0,FNum -4.0)));
        ("TestParse06","(2.0+3)*4",FMult (FAdd (FNum 2.0,FNum 3.0),FNum 4.0));
        ("TestParse07","-1(2+3)",FMult (FNum -1.0,FAdd (FNum 2.0,FNum 3.0)));
        ("TestParse08","2^4",FExponent (FNum 2.0,4));
        ("TestParse09","x^2",FExponent (FVar "x",2));
        ("TestParse10","x2",FVar "x2");
        ("TestParse11","2x",FMult (FNum 2.0,FVar "x"));
        ("TestParse12","-1x",FMult (FNum -1.0,FVar "x"));
        ("TestParse13","2 x",FMult (FNum 2.0,FVar "x"));
        ("TestParse14","2 x y",FMult (FMult (FNum 2.0,FVar "x"),FVar "y"));
        ("TestParse15","2xy",FMult (FNum 2.0,FVar "xy"));
        ("TestParse16","2x(2x)",FMult (FMult (FNum 2.0,FVar "x"),FMult (FNum 2.0,FVar "x")));
        ("TestParse17","2 x 2 y(2 x(-2))",FMult (FMult (FMult (FMult (FNum 2.0,FVar "x"),FNum 2.0),FVar "y"), FMult (FMult (FNum 2.0,FVar "x"),FNum -2.0)));
        ("TestParse18","2x^2*2y^2",FMult (FMult (FMult (FNum 2.0,FExponent (FVar "x",2)),FNum 2.0), FExponent (FVar "y",2)));
        ("TestParse19","2x^2(2y^2)",FMult (FMult (FNum 2.0,FExponent (FVar "x",2)), FMult (FNum 2.0,FExponent (FVar "y",2))))]
    Assert.EqualMany testlist parseStr

  testScan
  testInsertMult
  testParse
