module Tracer.Basics.Transformation

open Tracer.Basics
type QuickMatrix =
    { Pos1x1: float; Pos1x2: float; Pos1x3: float; Pos1x4: float
      Pos2x1: float; Pos2x2: float; Pos2x3: float; Pos2x4: float
      Pos3x1: float; Pos3x2: float; Pos3x3: float; Pos3x4: float
      Pos4x1: float; Pos4x2: float; Pos4x3: float; Pos4x4: float }
    member transpose : QuickMatrix
    static member multi : QuickMatrix * QuickMatrix -> QuickMatrix
type Transformation

val mkTransformation : QuickMatrix * QuickMatrix -> Transformation
  
val translate : x : float -> y : float -> z : float -> Transformation
val getMatrix : Transformation -> QuickMatrix
val getInvMatrix : Transformation -> QuickMatrix
val scale : width : float -> height : float -> depth : float -> Transformation
val vectorToMatrix : Vector -> QuickMatrix
val pointToMatrix : Point -> QuickMatrix
val matrixToVector : QuickMatrix -> Vector
val matrixToPoint :  QuickMatrix -> Point
val rotateX : angle : float -> Transformation
val rotateY : angle : float -> Transformation
val rotateZ : angle : float -> Transformation
val sheare : xy : float * xz :float * yx : float * yz : float * zx : float * zy : float-> Transformation
val mergeTransformations : Transformation list -> Transformation
val transformPoint : Point * QuickMatrix -> Point
val transformVector : Vector * QuickMatrix -> Vector