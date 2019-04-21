module TransformationTest

open Tracer.Basics.Transformation
open System
open Assert
open Tracer.Basics

let allTest = 
  0
//    let testPointLowest = 
//        let p1 = Point(1.,2.,0.)
//        let p2 = Point(0.,-2.,1.)
//        let result = p1.Lowest p2
//        let expected = Point(0.,-2.,0.)
//        Assert.Equal(expected,result,"LowestPoint")

//    let testPointHighest = 
//        let p1 = Point(5.01352001468749,4.476354,4.27325982495822)
//        let p2 = Point(5.01352001468749,-3.116544,-1.38118319041031)
//        let result = p1.Highest p2
//        let expected = Point(5.01352001468749,4.476354,4.27325982495822)
//        Assert.Equal(expected,result,"HighestPoint")

//    let testMatrixMultiplicationEqualsCorrectMatrix = 
//        let firstMatrix = mkMatrix [|[|2.;0.;0.;0.|];[|0.;0.5;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        let secondMatrix = mkMatrix [|[|0.5;0.;0.;0.|];[|0.;2.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        let result = getList (Matrix.multi (firstMatrix,secondMatrix))
//        let expected = [|[|1.0;0.0;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal (expected, result, "MatrixMultiplicationEqualsCorrectMatrix")
    
//    let testTranspose = 
//        let matrix = mkMatrix [|[|1.;2.;3.;4.|];[|5.;6.;7.;8.|];[|9.;10.;11.;12.|];[|13.;14.;15.;16.|]|]
//        let result = getList (transpose matrix)
//        let expected = [|[|1.;5.;9.;13.|];[|2.;6.;10.;14.|];[|3.;7.;11.;15.|];[|4.;8.;12.;16.|]|]
//        Assert.Equal (expected,result, "Transpose")
    
//    let testGetListReturnsListAttachedToMatrix = 
//        let matrix = getList ( mkMatrix [|[|1.;2.;3.;4.|];[|1.;2.;3.;4.|];[|1.;2.;3.;4.|];[|1.;2.;3.;4.|]|])
//        let expected = [|[|1.;2.;3.;4.|];[|1.;2.;3.;4.|];[|1.;2.;3.;4.|];[|1.;2.;3.;4.|]|]
//        Assert.Equal(expected, matrix, "GetListReturnsCorrectList")

//    let testScale = 
//        let result = getList (getMatrix (scale 2. 0.5 1.))
//        let expected = [|[|2.;0.;0.;0.|];[|0.;0.5;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal (expected,result, "Scale")

//    let testScaleInv = 
//        let trans = scale 2. 0.5 1.
//        let matrix1 = (getMatrix trans)
//        let matrix2 = (getInvMatrix trans)
//        let result = getList (Matrix.multi (matrix2,matrix1))
//        let expected = [|[|1.;0.;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal (expected,result, "ScaleInv")
    
//    let testTranslate = 
//        let matrix = getList (getMatrix (translate 10. 10. -10.))
//        let expected = [|[|1.;0.;0.;10.|];[|0.;1.;0.;10.|];[|0.;0.;1.;-10.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, matrix, "TranslateTest")

//    let testTranslateInv = 
//        let trans = translate 2. 0.5 1.
//        let matrix1 = (getMatrix (trans))
//        let matrix2 = (getInvMatrix (trans))
//        let result = getList (Matrix.multi (matrix1,matrix2))
//        let expected = [|[|1.;0.;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "TranslateInverseTest")
    
//    let testSheareXY = 
//        let result = getList (getMatrix (sheare (10.,0.,0.,0.,0.,0.)))
//        let expected = [|[|1.;0.;0.;0.|];[|10.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "SheareXY")
//    let testSheareXZ = 
//        let result = getList (getMatrix (sheare (0.,-7.,0.,0.,0.,0.)))
//        let expected = [|[|1.;0.;0.;0.|];[|0.;1.;0.;0.|];[|-7.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "SheareXZ")
//    let testSheareYX = 
//        let result = getList (getMatrix (sheare (0.,0.,10.,0.,0.,0.)))
//        let expected = [|[|1.;10.;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "SheareYX")
//    let testSheareYZ = 
//        let result = getList (getMatrix (sheare (0.,0.,0.,10.,0.,0.)))
//        let expected = [|[|1.;0.;0.;0.|];[|0.;1.;0.;0.|];[|0.;10.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "SheareYZ")
//    let testSheareZX = 
//        let result = getList (getMatrix (sheare (0.,0.,0.,0.,10.,0.)))
//        let expected = [|[|1.;0.;10.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "SheareZX")
//    let testSheareZY = 
//        let result = getList (getMatrix (sheare (0.,0.,0.,0.,0.,10.)))
//        let expected = [|[|1.;0.;0.;0.|];[|0.;1.;10.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected, result, "SheareZY")

//    let testSheareInv = 
//        let trans = sheare (10., 0.,0.,0.,0.,0.)
//        let matrix1 = getMatrix trans
//        let matrix2 = getInvMatrix trans
//        let result = getList (Matrix.multi (matrix1,matrix2))
//        let expected = [|[|1.;0.;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected,result,"SheareInv")

//    let testRotateXMultiplyInverseRotateXGivesIdentityMatrix = 
//        let trans = rotateX 1.5
//        let matrix1 = getMatrix trans
//        let matrix2 = getInvMatrix trans
//        let result = getList (Matrix.multi (matrix1,matrix2))
//        let expected = [|[|1.0;0.0;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected,result,"RotateXMultiplyInverseRotateXGivesIdentityMatrix")

//    let testRotateYMultiplyInverseRotateYGivesIdentityMatrix = 
//        let matrix1 = rotateY 1.5
//        let result = getList (Matrix.multi (getMatrix matrix1,getInvMatrix matrix1))
//        let expected = [|[|1.0;0.0;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected,result,"RotateYMultiplyInverseRotateYGivesIdentityMatrix")

//    let testRotateZMultiplyInverseRotateZGivesIdentityMatrix = 
//        let matrix1 = rotateZ 1.5
//        let result = getList (Matrix.multi (getMatrix matrix1,getInvMatrix matrix1))   
//        let expected = [|[|1.0;0.0;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]
//        Assert.Equal(expected,result,"RotateZMultiplyInverseRotateZGivesIdentityMatrix")

//    let testMergeTransformationWithTwoTransformations = 
//        let matrix1 = (scale 10. 10. 10.)
//        let matrix2 = translate 2. 2. 2.
//        let listMatrix = [matrix1; matrix2]
//        let result = getMatrix (mergeTransformations listMatrix)
//        let resultInv = getInvMatrix (mergeTransformations listMatrix)
//        let expected = Matrix.multi (getMatrix matrix1, getMatrix matrix2)
//        let expectedInv = Matrix.multi (getInvMatrix matrix2, getInvMatrix matrix1)
//        Assert.Equal(expected,result,"MergeTransformationWithTwoTransformations")
//        Assert.Equal(expectedInv,resultInv,"MergeTransformationWithTwoInvTransformations")

        
//    let testMergeTransformationWithFiveTransformations = 
//        let matrix1 = (scale 10. 10. 10.)
//        let matrix2 = (translate 10. 10. 10.)
//        let matrix3 = (rotateX 1.5)
//        let matrix4 = (translate 10. 10. 10.)
//        let matrix5 = (translate 10. 10. 10.)
//        let listMatrix = [matrix1; matrix2; matrix3; matrix4; matrix5]
//        let merge = mergeTransformations listMatrix
//        let result = Matrix.multi ((getMatrix merge),(getInvMatrix merge))
//        let expected = mkMatrix ([|[|1.;0.;0.;0.|];[|0.;1.;0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|])
//        //Has to use ToString() Since fsharp overrides the type and the equals somehow can't compare it
//        Assert.Equal(((getList expected).ToString()),(getList result).ToString(),"MergeTransformationWithFiveTransformations")
    

//    //TODO: fix build error in this test 
//    (*
//    let testTransformPointLight = 
//        let light = new PointLight(new Colour(1.,1.,1.), 1., new Point(0.,10.,0.))
//        let move = translate 5. -20. 0.
//        let result = ((transformLight light move) :?> PointLight).Position.GetCoord
//        let expected = (new PointLight(new Colour(1.,1.,1.), 1., new Point(5.,-10.,0.))).Position.GetCoord
//        Assert.Equal(expected,result, "TrasformingPointLight")
//    *)
//    0