module Tracer.Basics.Transformation

open System
open Tracer.Basics
    

    type QuickMatrix = {
        Pos1x1 : float
        Pos1x2 : float
        Pos1x3 : float
        Pos1x4 : float
        Pos2x1 : float
        Pos2x2 : float
        Pos2x3 : float
        Pos2x4 : float
        Pos3x1 : float
        Pos3x2 : float
        Pos3x3 : float
        Pos3x4 : float
        Pos4x1 : float
        Pos4x2 : float
        Pos4x3 : float
        Pos4x4 : float}
    let defaultQuickMatrix = {
        Pos1x1 = 1.
        Pos1x2 = 0.
        Pos1x3 = 0.
        Pos1x4 = 0.
        Pos2x1 = 0.
        Pos2x2 = 1.
        Pos2x3 = 0.
        Pos2x4 = 0.
        Pos3x1 = 0.
        Pos3x2 = 0.
        Pos3x3 = 1.
        Pos3x4 = 0.
        Pos4x1 = 0.
        Pos4x2 = 0.
        Pos4x3 = 0.
        Pos4x4 = 1.}
    type Transformation = 
        | T of QuickMatrix * QuickMatrix
    let private finiteMatrix matrix =
        [matrix.Pos1x1; matrix.Pos1x2; matrix.Pos1x3; matrix.Pos1x4
         matrix.Pos2x1; matrix.Pos2x2; matrix.Pos2x3; matrix.Pos2x4
         matrix.Pos3x1; matrix.Pos3x2; matrix.Pos3x3; matrix.Pos3x4
         matrix.Pos4x1; matrix.Pos4x2; matrix.Pos4x3; matrix.Pos4x4]
        |> List.forall Double.IsFinite
    let mkTransformation (a,b) =
        if not (finiteMatrix a && finiteMatrix b) then
            invalidArg "transformation" "A transformation and its inverse must have finite coefficients."
        T(a,b)

    let multAr ((a:float array),(b:float array)) = a.[0]*b.[0]+a.[1]*b.[1]+a.[2]*b.[2]+a.[3]*b.[3]
    type QuickMatrix with
        member this.transpose = 
            {this with 
                        Pos1x2 = this.Pos2x1; 
                        Pos1x3 = this.Pos3x1;
                        Pos1x4 = this.Pos4x1;
                        Pos2x1 = this.Pos1x2; 
                        Pos2x3 = this.Pos3x2;
                        Pos2x4 = this.Pos4x2;
                        Pos3x1 = this.Pos1x3; 
                        Pos3x2 = this.Pos2x3;
                        Pos3x4 = this.Pos4x3;
                        Pos4x1 = this.Pos1x4; 
                        Pos4x2 = this.Pos2x4;
                        Pos4x3 = this.Pos3x4;}
        static member multi (Q1 : QuickMatrix, Q2 : QuickMatrix) = 
            {defaultQuickMatrix with 
                Pos1x1 = (Q1.Pos1x1*Q2.Pos1x1)+(Q1.Pos1x2*Q2.Pos2x1)+(Q1.Pos1x3*Q2.Pos3x1)+(Q1.Pos1x4*Q2.Pos4x1);
                Pos1x2 = (Q1.Pos1x1*Q2.Pos1x2)+(Q1.Pos1x2*Q2.Pos2x2)+(Q1.Pos1x3*Q2.Pos3x2)+(Q1.Pos1x4*Q2.Pos4x2);
                Pos1x3 = (Q1.Pos1x1*Q2.Pos1x3)+(Q1.Pos1x2*Q2.Pos2x3)+(Q1.Pos1x3*Q2.Pos3x3)+(Q1.Pos1x4*Q2.Pos4x3);
                Pos1x4 = (Q1.Pos1x1*Q2.Pos1x4)+(Q1.Pos1x2*Q2.Pos2x4)+(Q1.Pos1x3*Q2.Pos3x4)+(Q1.Pos1x4*Q2.Pos4x4);
                Pos2x1 = (Q1.Pos2x1*Q2.Pos1x1)+(Q1.Pos2x2*Q2.Pos2x1)+(Q1.Pos2x3*Q2.Pos3x1)+(Q1.Pos2x4*Q2.Pos4x1);
                Pos2x2 = (Q1.Pos2x1*Q2.Pos1x2)+(Q1.Pos2x2*Q2.Pos2x2)+(Q1.Pos2x3*Q2.Pos3x2)+(Q1.Pos2x4*Q2.Pos4x2);
                Pos2x3 = (Q1.Pos2x1*Q2.Pos1x3)+(Q1.Pos2x2*Q2.Pos2x3)+(Q1.Pos2x3*Q2.Pos3x3)+(Q1.Pos2x4*Q2.Pos4x3);
                Pos2x4 = (Q1.Pos2x1*Q2.Pos1x4)+(Q1.Pos2x2*Q2.Pos2x4)+(Q1.Pos2x3*Q2.Pos3x4)+(Q1.Pos2x4*Q2.Pos4x4);
                Pos3x1 = (Q1.Pos3x1*Q2.Pos1x1)+(Q1.Pos3x2*Q2.Pos2x1)+(Q1.Pos3x3*Q2.Pos3x1)+(Q1.Pos3x4*Q2.Pos4x1);
                Pos3x2 = (Q1.Pos3x1*Q2.Pos1x2)+(Q1.Pos3x2*Q2.Pos2x2)+(Q1.Pos3x3*Q2.Pos3x2)+(Q1.Pos3x4*Q2.Pos4x2);
                Pos3x3 = (Q1.Pos3x1*Q2.Pos1x3)+(Q1.Pos3x2*Q2.Pos2x3)+(Q1.Pos3x3*Q2.Pos3x3)+(Q1.Pos3x4*Q2.Pos4x3);
                Pos3x4 = (Q1.Pos3x1*Q2.Pos1x4)+(Q1.Pos3x2*Q2.Pos2x4)+(Q1.Pos3x3*Q2.Pos3x4)+(Q1.Pos3x4*Q2.Pos4x4);
                Pos4x1 = (Q1.Pos4x1*Q2.Pos1x1)+(Q1.Pos4x2*Q2.Pos2x1)+(Q1.Pos4x3*Q2.Pos3x1)+(Q1.Pos4x4*Q2.Pos4x1);
                Pos4x2 = (Q1.Pos4x1*Q2.Pos1x2)+(Q1.Pos4x2*Q2.Pos2x2)+(Q1.Pos4x3*Q2.Pos3x2)+(Q1.Pos4x4*Q2.Pos4x2);
                Pos4x3 = (Q1.Pos4x1*Q2.Pos1x3)+(Q1.Pos4x2*Q2.Pos2x3)+(Q1.Pos4x3*Q2.Pos3x3)+(Q1.Pos4x4*Q2.Pos4x3);
                Pos4x4 = (Q1.Pos4x1*Q2.Pos1x4)+(Q1.Pos4x2*Q2.Pos2x4)+(Q1.Pos4x3*Q2.Pos3x4)+(Q1.Pos4x4*Q2.Pos4x4)
            }
    end

    let identityMatrixWithPos (x,y,z) = {defaultQuickMatrix with Pos1x4 = x; Pos2x4 = y; Pos3x4 = z } //CREATES AN IDENTITY MATRIX
    let getMatrix (T(a,_)) = a
    let getInvMatrix (T(_,b)) = b
    let vectorToMatrix (v:Vector) = {identityMatrixWithPos(v.X,v.Y,v.Z) with Pos4x4 = 0.}
    let pointToMatrix (p:Point) = identityMatrixWithPos (p.X,p.Y,p.Z)

    let private finite name value =
        if not (Double.IsFinite value) then invalidArg name "Transformation values must be finite."

    let translate x y z =
        finite "x" x
        finite "y" y
        finite "z" z
        mkTransformation (identityMatrixWithPos (x,y,z), identityMatrixWithPos (-x,-y,-z))

    let scale width height depth = 
        for name, value in ["width", width; "height", height; "depth", depth] do
            finite name value
            if value = 0. || not (Double.IsFinite (1. / value)) then
                invalidArg name "A scale must have a finite, nonzero inverse."
        let scaleMatrix (x,y,z) = {defaultQuickMatrix with Pos1x1 = x; Pos2x2 = y; Pos3x3 = z}

        mkTransformation (scaleMatrix(width,height,depth),scaleMatrix(1./width,1./height,1./depth))
    let sheare (xy:float,xz:float,yx:float,yz:float,zx:float,zy:float) = 
        for value in [xy; xz; yx; yz; zx; zy] do finite "shear" value
        let matrix = {defaultQuickMatrix with Pos1x2 = yx; Pos1x3 = zx; Pos2x1 = xy; Pos2x3 = zy; Pos3x1 = xz; Pos3x2 = yz}
        let det = 1. - xy*yx - xz*zx - yz*zy + xy*yz*zx + xz*yx*zy
        if det = 0. || not (Double.IsFinite det) || not (Double.IsFinite (1. / det)) then
            invalidArg "shear" "A shear must be invertible."
        let mult = 1./det

        let inv = {defaultQuickMatrix with 
                                            Pos1x1 = mult*(1.-(yz*zy)); Pos1x2 = mult*(-yx+yz*zx); Pos1x3 = mult*(-zx+yx*zy);
                                            Pos2x1 = mult*(-xy+xz*zy); Pos2x2 = mult*(1.-xz*zx); Pos2x3 = mult*(-zy+xy*zx);
                                            Pos3x1 = mult*(-xz+xy*yz); Pos3x2 = mult*(-yz+xz*yx); Pos3x3 = mult*(1.-xy*yx);
                                            Pos4x4 = mult*det}
        mkTransformation(matrix,inv)

    let rotateX angle = 
        finite "angle" angle
        let cos = Math.Cos(angle)
        let sin = Math.Sin(angle) 
        mkTransformation(
            {defaultQuickMatrix with Pos2x2 = cos; Pos2x3 = -sin; Pos3x2 = sin; Pos3x3 = cos},
            {defaultQuickMatrix with Pos2x2 = cos; Pos2x3 = sin; Pos3x2 = -sin; Pos3x3 = cos})
    let rotateY angle = 
        finite "angle" angle
        let cos = Math.Cos(angle)
        let sin = Math.Sin(angle) 
        mkTransformation(
            {defaultQuickMatrix with Pos1x1 = cos; Pos1x3 = sin; Pos3x1 = -sin; Pos3x3 = cos},
            {defaultQuickMatrix with Pos1x1 = cos; Pos1x3 = -sin; Pos3x1 = sin; Pos3x3 = cos})
    let rotateZ angle =
        finite "angle" angle
        let cos = Math.Cos(angle)
        let sin = Math.Sin(angle) 
        mkTransformation(
            {defaultQuickMatrix with Pos1x1 = cos; Pos1x2 = -sin; Pos2x1 = sin; Pos2x2 = cos},
            {defaultQuickMatrix with Pos1x1 = cos; Pos1x2 = sin; Pos2x1 = -sin; Pos2x2 = cos})
    let mergeMatrix (l : QuickMatrix List) = 
        let rec sum (value,l2)= 
            match l2 with
            | first::rest ->
                let v = QuickMatrix.multi(value,first)
                sum(v,rest)
            | _ -> value
        sum (defaultQuickMatrix,l)

    let mergeTransformations (l: Transformation List) : Transformation = 
        let matrixList = (List.rev l) |> List.map (fun a -> getMatrix a)
        let NewMatrix = mergeMatrix matrixList
        let reverseMatrixList = l |> List.map (fun a -> getInvMatrix a)
        let newInverseMatrix = mergeMatrix reverseMatrixList
        mkTransformation (NewMatrix, newInverseMatrix)

    let matrixToVector (q:QuickMatrix) = 
        let x = q.Pos1x4
        let y = q.Pos2x4
        let z = q.Pos3x4
        new Vector(x, y, z)

    let matrixToPoint (q:QuickMatrix) = 
        let x = q.Pos1x4
        let y = q.Pos2x4
        let z = q.Pos3x4
        new Point(x, y, z)

    let transformPoint (p:Point, m:QuickMatrix) =
        Point(Math.FusedMultiplyAdd(m.Pos1x1,p.X,Math.FusedMultiplyAdd(m.Pos1x2,p.Y,Math.FusedMultiplyAdd(m.Pos1x3,p.Z,m.Pos1x4))),
              Math.FusedMultiplyAdd(m.Pos2x1,p.X,Math.FusedMultiplyAdd(m.Pos2x2,p.Y,Math.FusedMultiplyAdd(m.Pos2x3,p.Z,m.Pos2x4))),
              Math.FusedMultiplyAdd(m.Pos3x1,p.X,Math.FusedMultiplyAdd(m.Pos3x2,p.Y,Math.FusedMultiplyAdd(m.Pos3x3,p.Z,m.Pos3x4))))
    let transformVector (v:Vector, m:QuickMatrix) =
        Vector(Math.FusedMultiplyAdd(m.Pos1x1,v.X,Math.FusedMultiplyAdd(m.Pos1x2,v.Y,m.Pos1x3*v.Z)),
               Math.FusedMultiplyAdd(m.Pos2x1,v.X,Math.FusedMultiplyAdd(m.Pos2x2,v.Y,m.Pos2x3*v.Z)),
               Math.FusedMultiplyAdd(m.Pos3x1,v.X,Math.FusedMultiplyAdd(m.Pos3x2,v.Y,m.Pos3x3*v.Z)))



    
