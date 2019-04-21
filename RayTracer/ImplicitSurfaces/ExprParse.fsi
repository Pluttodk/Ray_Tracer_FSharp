namespace Tracer

module ExprParse =
  open Tracer.Basics
  
  type terminal = | Add | Mul | Div | Pwr | Root | Lpar | Rpar | Int of int | Float of float | Var of string
  exception ScanErrorException
  val scan : char seq -> terminal list
  val insertMult : terminal list -> terminal list

  type expr = 
    | FNum of float
    | FVar of string
    | FAdd of expr * expr
    | FMult of expr * expr
    | FDiv of expr * expr
    | FExponent of expr * int
    | FRoot of expr * int

  exception ParseErrorException
  val parse : terminal list -> expr
  val parseStr : char seq -> expr

  val reduceExpr : expr -> expr
  val solveExpr : Point -> expr -> float