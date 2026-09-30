namespace Tracer

module ImplicitSurfaces =
  open System
  open Tracer.ExprParse
  open Tracer.ExprToPoly
  open Tracer.PolyToUnipoly
  open Tracer.Basics

  type hf = Ray -> (float * Vector) option
  type hitPoint = Tracer.Basics.HitPoint
  type baseShape = Tracer.BaseShape.BaseShape
  type shape = Tracer.Basics.Shape
  type expr = ExprParse.expr
  type poly = ExprToPoly.poly
  type unipoly = PolyToUnipoly.unipoly
  type Ray = Tracer.Basics.Ray
  type simpleIntExpr = PolyToUnipoly.simpleIntExpr
  type simpleExpr = ExprToPoly.simpleExpr

  let substWithRayVars expression =
    let replacement origin direction = FAdd(FVar origin, FMult(FVar "t", FVar direction))
    List.fold subst expression [("x", replacement "ox" "dx"); ("y", replacement "oy" "dy"); ("z", replacement "oz" "dz")]

  let partialDerivative variable expression =
    let rec derivative = function
      | FNum _ -> FNum 0.
      | FVar name -> FNum(if name = variable then 1. else 0.)
      | FExponent(_, 0) -> FNum 0.
      | FExponent(e, n) -> FMult(derivative e, FMult(FNum(float n), FExponent(e, n-1)))
      | FAdd(a, b) -> FAdd(derivative a, derivative b)
      | FMult(a, b) -> FAdd(FMult(derivative a, b), FMult(derivative b, a))
      | FDiv(a, b) -> FDiv(FAdd(FMult(b, derivative a), FMult(FNum -1., FMult(a, derivative b))), FExponent(b, 2))
      | FRoot(e, n) -> FDiv(derivative e, FMult(FNum(float n), FExponent(FRoot(e, n), n-1)))
    derivative expression |> reduceExpr

  let normalVector point dx dy dz =
    Vector(solveExpr point dx, solveExpr point dy, solveExpr point dz).Normalise

  let discriminant a b c = b*b - 4.*a*c
  let getDistances a b d = [(-b + sqrt d)/(2.*a); (-b - sqrt d)/(2.*a)]
  let getValArray (ray: Ray) =
    let origin, direction = ray.GetOrigin, ray.GetDirection
    [|origin.X; origin.Y; origin.Z; direction.X; direction.Y; direction.Z|]
  let nrtolerance = 1e-12
  let nrepsilon = 1e-10

  let newtonRaphson polynomial derivative initial =
    let rec improve guess remaining =
      if remaining = 0 || not (Double.IsFinite guess) then None
      else
        let value, slope = solveUnipoly polynomial guess, solveUnipoly derivative guess
        if not (Double.IsFinite value && Double.IsFinite slope) then None
        elif value = 0. then Some guess
        elif abs slope < nrepsilon then None
        else
          let next = guess - value/slope
          if not (Double.IsFinite next) then None
          elif next = guess || abs(next-guess) <= nrtolerance*abs next then Some next
          else improve next (remaining-1)
    improve initial 25

  let sepolyToSIEpoly (terms: (int * simpleExpr) list) : (int * simpleIntExpr) list =
    terms |> List.map (fun (degree, coefficient) -> degree, seToSIE coefficient)

  let private roots terms (ray: Ray) minimum maximum =
    let polynomial = toUnipoly terms (getValArray ray)
    let high = if Double.IsFinite maximum then maximum else rootBound polynomial
    if high < minimum then [||] else realRootsInInterval polynomial minimum high

  let private polynomialHF terms dx dy dz : hf =
    fun ray ->
      if not ray.IsValid then None
      else
        roots terms ray 0. infinity
        |> Array.tryPick (fun time ->
            if time <= 0. then None
            else
              let normal = normalVector (ray.PointAtTime time) dx dy dz
              if normal.IsFinite && normal <> Vector.Zero then Some(time, normal) else None)

  let getFirstDegreeHF terms dx dy dz = polynomialHF terms dx dy dz
  let getSecondDegreeHF terms dx dy dz = polynomialHF terms dx dy dz
  let getHigherDegreeHF terms dx dy dz = polynomialHF terms dx dy dz

  // Evaluate the original expression too: clearing denominators and radicals can introduce extraneous roots.
  let rec private compile expression : Point -> struct (float * float) =
    match expression with
    | FNum value ->
        if not (Double.IsFinite value) then invalidArg "expression" "Implicit constants must be finite."
        fun _ -> struct (value, abs value)
    | FVar name ->
        let coordinate =
          match name with
          | "x" -> fun (point: Point) -> point.X
          | "y" -> fun (point: Point) -> point.Y
          | "z" -> fun (point: Point) -> point.Z
          | _ -> invalidArg "expression" ("Unknown implicit variable '" + name + "'.")
        fun point -> let value = coordinate point in struct (value, abs value)
    | FAdd(a,b) ->
        let a, b = compile a, compile b
        fun point ->
          let struct (av, am), struct (bv, bm) = a point, b point
          struct (av+bv, am+bm)
    | FMult(a,b) ->
        let a, b = compile a, compile b
        fun point ->
          let struct (av, am), struct (bv, bm) = a point, b point
          struct (av*bv, am*bm)
    | FDiv(a,b) ->
        let a, b = compile a, compile b
        fun point ->
          let struct (av, am), struct (bv, _) = a point, b point
          struct (av/bv, am/abs bv)
    | FExponent(e, n) ->
        let e = compile e
        fun point ->
          let struct (value, magnitude) = e point
          struct (pown value n, pown magnitude n)
    | FRoot(e, n) ->
        if n <= 0 then invalidArg "expression" "Implicit root degrees must be positive."
        let e = compile e
        fun point ->
          let struct (value, magnitude) = e point
          struct (realRoot value n, realRoot magnitude n)

  let private createImplicit (expression: string) minimum maximum (bounds: BBox option) : baseShape =
    let expression = parseStr expression
    let evaluate = compile expression
    let dx, dy, dz =
      compile (partialDerivative "x" expression),
      compile (partialDerivative "y" expression),
      compile (partialDerivative "z" expression)
    let terms =
      exprToPoly (substWithRayVars expression) "t"
      |> polyAsList |> sepolyToSIEpoly
    let normal point =
      let struct (x, _), struct (y, _), struct (z, _) = dx point, dy point, dz point
      Vector(x,y,z).Normalise
    let refine (ray: Ray) low high initial =
      let mutable time, active, iterations = initial, true, 0
      while active && iterations < 20 do
        iterations <- iterations+1
        let point = ray.PointAtTime time
        let struct (value, _) = evaluate point
        let struct (x, _), struct (y, _), struct (z, _) = dx point, dy point, dz point
        let direction = ray.GetDirection
        let slope = x*direction.X + y*direction.Y + z*direction.Z
        if value = 0. || slope = 0. || not (Double.IsFinite value && Double.IsFinite slope) then active <- false
        else
          let candidate = time-value/slope
          if not (Double.IsFinite candidate) || candidate < low || candidate > high || candidate = time then active <- false
          else time <- candidate
      time
    { new baseShape() with
        member _.toShape texture =
          let intersect (owner: shape) (ray: Ray) lower upper =
            let queryMinimum, queryMaximum = max minimum lower, min maximum upper
            if not ray.IsValid || queryMinimum >= queryMaximum then HitPoint(ray)
            else
              let interval =
                match bounds with
                | None -> Some(queryMinimum, queryMaximum)
                | Some box -> box.IntersectInterval(ray, queryMinimum, queryMaximum)
              match interval with
              | None -> HitPoint(ray)
              | Some(low, high) ->
                  roots terms ray low high
                  |> Array.tryPick (fun initial ->
                      let time = refine ray low high initial
                      if time <= queryMinimum || time >= queryMaximum then None
                      else
                        let point = ray.PointAtTime time
                        let struct (value, magnitude) = evaluate point
                        if not (Double.IsFinite value && Double.IsFinite magnitude)
                           || abs value > 1e-8*magnitude then None
                        else
                          let outward = normal point
                          if not outward.IsFinite || outward.IsZero then None
                          else Some(HitPoint(ray, time, outward, Textures.getFunc texture 0. 0., owner, 0., 0.)))
                  |> Option.defaultWith (fun () -> HitPoint(ray))
          { new shape() with
              member _.Bounds = bounds
              member _.IsOpaque = Textures.isOpaque texture
              member this.hitFunction ray = intersect this ray 0. infinity
              member _.isInside point =
                let struct (value, _) = evaluate point
                Double.IsFinite value && value < 0. && (bounds |> Option.forall (fun box -> box.isInside point))
              member _.getBoundingBox() =
                match bounds with
                | Some box -> box
                | None -> invalidOp "An unbounded implicit surface has no finite bounding box."
            interface IIntervalShape with
              member this.HitWithin(ray, lower, upper) = intersect (this :?> shape) ray lower upper } }

  let mkImplicit (expression: string) : baseShape = createImplicit expression 0. infinity None

  let mkImplicitInInterval (expression: string) (minimum: float) (maximum: float) : baseShape =
    if not (Double.IsFinite minimum && Double.IsFinite maximum) || minimum < 0. || maximum <= minimum then
      invalidArg "interval" "Implicit ray bounds must be finite, nonnegative and strictly ordered."
    createImplicit expression minimum maximum None

  let mkBoundedImplicit (expression: string) (bounds: BBox) : baseShape =
    if bounds.IsEmpty || not bounds.lowPoint.IsFinite || not bounds.highPoint.IsFinite then
      invalidArg "bounds" "Implicit bounds must be finite and ordered."
    createImplicit expression 0. infinity (Some bounds)
