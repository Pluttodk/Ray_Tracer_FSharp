module Tracer.Basics.Transformation

open Tracer.Basics
[<Sealed>]
type QuickMatrix =
    member transpose : QuickMatrix
    static member multi : QuickMatrix * QuickMatrix -> QuickMatrix
type Transformation
  
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