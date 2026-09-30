namespace Tracer

module ImplicitSurfaces =

  open Tracer.Basics

  type hf = Ray -> (float * Vector) option

  type baseShape = Tracer.BaseShape.BaseShape
  type shape = Tracer.Basics.Shape

  val mkImplicit : string -> baseShape

  // needed for testing
  type expr = ExprParse.expr
  val partialDerivative : string -> expr -> expr
  val substWithRayVars : expr -> expr
  val getValArray : Ray -> float array

  type Ray = Tracer.Basics.Ray
  type poly = ExprToPoly.poly
  type unipoly = PolyToUnipoly.unipoly

  val newtonRaphson : unipoly -> unipoly -> float -> float option

  type simpleExpr = ExprToPoly.simpleExpr
  type simpleIntExpr = PolyToUnipoly.simpleIntExpr
  val sepolyToSIEpoly : (int * simpleExpr) list -> (int * simpleIntExpr) list
