namespace Tracer

module PolyToUnipoly =

  type poly = ExprToPoly.poly

  type unipoly = 
    | UP of (int * float) list
    static member ( - ) : unipoly * unipoly -> unipoly
    static member ( % ) : unipoly * unipoly -> unipoly //* unipoly
    static member ( * ) : unipoly * (int * float) -> unipoly
  type simpleExpr = ExprToPoly.simpleExpr
  type iAtom = IANum of float | IAExponent of int * int
  type iAG = iAtom list
  type simpleIntExpr = SIE of iAG list

  val seToSIE : simpleExpr -> simpleIntExpr
  val solveSIE : simpleIntExpr -> float array -> float
  val toUnipoly : (int*simpleIntExpr) list -> float array -> unipoly
  val solveUnipoly : unipoly -> float -> float
  val unipolyDerivative : unipoly -> unipoly
  val sturmSeq : unipoly -> unipoly -> unipoly list
  val getInterval : unipoly list -> float -> float -> int -> (float * float * float) option
