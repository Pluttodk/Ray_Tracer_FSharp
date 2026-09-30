namespace Tracer

module PolyToUnipoly =
  open System
  open Tracer.ExprToPoly

  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr
  type iAtom = IANum of float | IAExponent of int * int
  type iAG = iAtom list
  type simpleIntExpr = SIE of iAG list
  type unipoly = UP of (int * float) list

  let seToSIE (SE groups) =
    let atom = function
      | ANum value -> IANum value
      | AExponent(variable, exponent) ->
          let index =
            match variable with
            | "ox" -> 0 | "oy" -> 1 | "oz" -> 2
            | "dx" -> 3 | "dy" -> 4 | "dz" -> 5
            | _ -> invalidArg "variable" ("Unbound polynomial variable '" + variable + "'.")
          IAExponent(index, exponent)
      | _ -> invalidArg "expression" "Radicals must be eliminated before polynomial evaluation."
    SIE(List.map (List.map atom) groups)

  let solveSIE (SIE groups) (values: float array) =
    let atom = function
      | IANum value -> value
      | IAExponent(index, exponent) -> pown values.[index] exponent
    groups |> List.sumBy (fun group -> group |> List.fold (fun value term -> value * atom term) 1.)

  let private canonical terms =
    let mutable coefficients = Map.empty
    for degree, value in terms do
      if degree < 0 then invalidArg "polynomial" "Polynomial exponents must be nonnegative."
      if not (Double.IsFinite value) then invalidArg "polynomial" "Polynomial coefficients must be finite."
      let coefficient = value + defaultArg (Map.tryFind degree coefficients) 0.
      if not (Double.IsFinite coefficient) then invalidArg "polynomial" "Polynomial arithmetic overflowed."
      coefficients <- Map.add degree coefficient coefficients
    coefficients |> Map.toList |> List.rev |> List.filter (fun (_, value) -> value <> 0.)

  let toUnipoly terms values =
    UP(terms |> List.map (fun (degree, expression) -> degree, solveSIE expression values) |> canonical)

  let solveUnipoly (UP terms) value =
    match canonical terms with
    | [] -> 0.
    | (degree, coefficient)::rest ->
        let result, remaining =
          rest |> List.fold (fun (result, previous) (degree, coefficient) ->
            result * pown value (previous-degree) + coefficient, degree) (coefficient, degree)
        result * pown value remaining

  let unipolyDerivative (UP terms) =
    UP(canonical terms |> List.choose (fun (degree, coefficient) ->
      if degree = 0 then None else Some(degree-1, float degree * coefficient)))

  let getFirstTerm (UP terms) =
    match canonical terms with
    | [] -> 0, 0.
    | term::_ -> term

  let multUnipoly (UP terms) (exponent, coefficient) =
    UP(terms |> List.map (fun (degree, value) -> degree+exponent, value*coefficient) |> canonical)

  let negateUnipoly (UP terms) = UP(terms |> List.map (fun (degree, value) -> degree, -value) |> canonical)
  let epsilon = 1e-20

  let subtractUnipoly (UP first) (UP second) =
    UP(canonical (first @ List.map (fun (degree, value) -> degree, -value) second))

  let getDegree (UP terms) =
    match canonical terms with [] -> -1 | (degree, _)::_ -> degree

  let isEmpty (UP terms) = List.isEmpty (canonical terms)

  type unipoly with
    static member (-) (first, second) = subtractUnipoly first second
    static member (*) (polynomial, term) = multUnipoly polynomial term

  let unipolyLongDiv (UP dividend) (UP divisor) =
    let divisor = canonical divisor
    match divisor with
    | [] -> invalidArg "divisor" "Cannot divide by the zero polynomial."
    | (divisorDegree, divisorCoefficient)::_ ->
        let rec remainder terms =
          match terms with
          | [] -> UP []
          | (degree, _)::_ when degree < divisorDegree -> UP terms
          | (degree, coefficient)::_ ->
              let multiple = multUnipoly (UP divisor) (degree-divisorDegree, coefficient/divisorCoefficient)
              let (UP reduced) = subtractUnipoly (UP terms) multiple
              // The leading term cancels algebraically; remove only its floating-point roundoff.
              remainder (reduced |> List.filter (fun (power, _) -> power < degree))
        remainder (canonical dividend)

  type unipoly with
    static member (%) (first, second) = unipolyLongDiv first second

  let sturmSeq (UP terms) (UP derivativeTerms) =
    let polynomial, derivative = UP(canonical terms), UP(canonical derivativeTerms)
    if isEmpty polynomial then []
    elif isEmpty derivative then [polynomial]
    else
      let rec build previous current result =
        let remainder = negateUnipoly (unipolyLongDiv previous current)
        if isEmpty remainder then result
        else build current remainder (remainder::result)
      build polynomial derivative [derivative; polynomial]

  let countSignChanges sequence value =
    let mutable previous, changes = 0, 0
    for polynomial in sequence do
      let result = solveUnipoly polynomial value
      if Double.IsNaN result then invalidArg "interval" "Polynomial evaluation is not finite at this interval."
      let sign = if result > 0. then 1 elif result < 0. then -1 else 0
      if sign <> 0 then
        if previous <> 0 && previous <> sign then changes <- changes + 1
        previous <- sign
    changes

  let getInterval sequence low high maxDepth =
    if not (Double.IsFinite low && Double.IsFinite high) || low > high then
      invalidArg "interval" "Root-isolation bounds must be finite and ordered."
    match List.tryLast sequence with
    | None -> None
    | Some polynomial ->
        let roots a b = countSignChanges sequence a - countSignChanges sequence b
        let rec search a b depth =
          if solveUnipoly polynomial a = 0. then Some(a,a,a)
          elif roots a b <= 0 then
            if solveUnipoly polynomial b = 0. then Some(b,b,b) else None
          else
            let midpoint = a/2. + b/2.
            if depth >= maxDepth || midpoint = a || midpoint = b then Some(a,b,midpoint)
            elif roots a midpoint > 0 || solveUnipoly polynomial midpoint = 0. then search a midpoint (depth+1)
            else search midpoint b (depth+1)
        search low high 0

  let rootBound (UP terms) =
    match canonical terms with
    | [] | [(0, _)] -> 0.
    | (degree, leading)::rest ->
        let mutable logarithm = -infinity
        for exponent, coefficient in rest do
          logarithm <- max logarithm ((log (abs coefficient) - log (abs leading)) / float (degree-exponent))
        if Double.IsNegativeInfinity logarithm then 0.
        else
          let bound = exp (log 2. + logarithm)
          if not (Double.IsFinite bound) then
            invalidArg "polynomial" "The finite root bound overflowed; provide an explicit finite ray interval."
          Math.BitIncrement bound

  let realRootsInInterval (UP terms) low high =
    if not (Double.IsFinite low && Double.IsFinite high) || low > high then
      invalidArg "interval" "Root-search bounds must be finite and ordered."
    let terms = canonical terms
    let machineEpsilon = 2.2204460492503131e-16
    let normalize (coefficients: float array) =
      let scale = coefficients |> Array.fold (fun maximum value -> max maximum (abs value)) 0.
      if scale = 0. then coefficients else coefficients |> Array.map (fun value -> value/scale)
    let evaluate (coefficients: float array) x =
      let degree = coefficients.Length-1
      let mutable value, magnitude = 0., 0.
      if abs x > 1. then
        let inverse = 1./x
        for i = 0 to degree do
          value <- value*inverse + coefficients.[i]
          magnitude <- magnitude*abs inverse + abs coefficients.[i]
        if x < 0. && degree % 2 = 1 then value <- -value
      else
        for i = degree downto 0 do
          value <- value*x + coefficients.[i]
          magnitude <- magnitude*abs x + abs coefficients.[i]
      value, magnitude
    let isRoot degree (value, magnitude) =
      abs value <= 32. * machineEpsilon * float (degree+1) * magnitude
    let distinct (roots: float array) =
      let sorted = Array.sort roots
      let result = ResizeArray<float>()
      for root in sorted do
        if result.Count = 0
           || abs (root-result.[result.Count-1]) > 64.*machineEpsilon*max (abs root) (abs result.[result.Count-1]) then
          result.Add root
      result.ToArray()
    let rec search (coefficients: float array) =
      let degree = coefficients.Length-1
      if degree <= 0 then [||]
      elif degree = 1 then
        let root = -coefficients.[0] / coefficients.[1]
        if Double.IsFinite root && root >= low && root <= high then [|root|] else [||]
      elif degree = 2 then
        let a, b, c = coefficients.[2], coefficients.[1], coefficients.[0]
        let discriminant = b*b - 4.*a*c
        if discriminant < 0. then [||]
        else
          let roots =
            if discriminant = 0. then [|-b/(2.*a)|]
            else
              let q = -0.5 * (b + Math.CopySign(sqrt discriminant, b))
              [|q/a; c/q|]
          roots |> Array.filter (fun root -> Double.IsFinite root && root >= low && root <= high) |> distinct
      else
        let derivative = Array.init degree (fun i -> float (i+1)*coefficients.[i+1]) |> normalize
        let critical = search derivative |> Array.filter (fun root -> root > low && root < high)
        let knots = Array.concat [ [|low|]; critical; [|high|] ] |> distinct
        let results = ResizeArray<float>()
        let values = knots |> Array.map (evaluate coefficients)
        for i = 0 to knots.Length-1 do
          if isRoot degree values.[i] then results.Add knots.[i]
        for i = 0 to knots.Length-2 do
          let valueA, valueB = fst values.[i], fst values.[i+1]
          if not (isRoot degree values.[i] || isRoot degree values.[i+1])
             && ((valueA < 0.) <> (valueB < 0.)) then
            let mutable a, b, fa = knots.[i], knots.[i+1], valueA
            let mutable root, finished, iterations = a/2. + b/2., false, 0
            while not finished do
              iterations <- iterations+1
              let midpoint = a/2. + b/2.
              let evaluation = evaluate coefficients midpoint
              root <- midpoint
              if isRoot degree evaluation || midpoint = a || midpoint = b
                 || b-a <= 8.*machineEpsilon*max (abs a) (abs b) then finished <- true
              elif iterations > 2200 then invalidOp "Polynomial root isolation failed to converge."
              elif (fst evaluation < 0.) = (fa < 0.) then
                a <- midpoint
                fa <- fst evaluation
              else b <- midpoint
            results.Add root
        results.ToArray() |> distinct
    match terms with
    | [] -> [||]
    | (degree, _)::_ ->
        let coefficients = Array.zeroCreate<float> (degree+1)
        for degree, value in terms do coefficients.[degree] <- value
        search (normalize coefficients)
