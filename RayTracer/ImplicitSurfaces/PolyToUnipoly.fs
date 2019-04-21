namespace Tracer

module PolyToUnipoly =
  open Tracer.ExprToPoly
  
  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr

  (*
      Similar to simpleExpr, atomGroup and atom from ExprToPoly,
      with the difference that exponents contain an integer instead of a string variable

      The integers are mapped as follows:
          ox -> 0
          oy -> 1
          oz -> 2
          dx -> 3
          dy -> 4
          dx -> 5
  *)
  type iAtom = IANum of float | IAExponent of int * int
  type iAG = iAtom list
  type simpleIntExpr = SIE of iAG list

  (*
      Univariate polynomial type, i.e. only one variable, t, which is implicitly present in all map elements of a poly
  *)
  type unipoly = UP of (int * float) list

  (*
      Turns a simpleExpr into an simpleIntExpr
  *)
  let seToSIE (SE se) =
    let asubst (a:atom) : iAtom =
      match a with
      | ANum c         -> IANum c
      | AExponent(e,x) ->
          match e with
          | "ox" -> IAExponent(0,x)
          | "oy" -> IAExponent(1,x)
          | "oz" -> IAExponent(2,x)
          | "dx" -> IAExponent(3,x)
          | "dy" -> IAExponent(4,x)
          | "dz" -> IAExponent(5,x)
          | _    -> failwith "SEtoSIE: unmatched clause"
    let agtrav ag = List.foldBack (fun a acc -> asubst a :: acc) ag []
    SIE (List.foldBack (fun ag acc -> agtrav ag :: acc) se [])

  (*
      Solves a simpleIntExpr, with an array of ray values, where index positions match the first int of
      an IAExponent tuple
  *)
  let solveSIE (SIE sie) (valArr:float array) =
    let iasolver = function
    | IANum c         -> c
    | IAExponent(e,x) -> pown (valArr.[e]) x
    let iagsolver iag = List.fold (fun acc ia -> acc * iasolver ia) 1.0 iag
    List.fold (fun acc iag -> acc + iagsolver iag) 0.0 sie

  (*
      Takes a poly list ((int * simpleIntExpr) list) and converts it to
      a new list of int*float (unipoly).
      Requires an array with all the ray float values (based on origin point and direction vector)

      The list order is kept
  *)
  let toUnipoly (lis: (int*simpleIntExpr) list) valArr : unipoly =
    UP (List.foldBack (fun (n, (SIE sie)) acc -> (n, solveSIE (SIE sie) valArr)::acc) lis [])
  
  (*
      Solves an unipoly for a given t float value
  *)
  let solveUnipoly (UP up:unipoly) t =
    List.fold (fun acc (n,c) -> if n > 0 then (acc + (pown t n) * c)
                                else (acc + c)) 0.0 up

  (*
      Returns the derivative unipoly of the given unipoly
  *)
  let unipolyDerivative (UP up:unipoly) : unipoly =
    UP (List.foldBack (fun (n,c) acc -> 
                     if n = 0 then acc
                     else (n-1, float n * c) :: acc) up [])

  (*
      Returns the first element of a unipoly.
      If done correctly, this will be the term with the highest degree
  *)
  let getFirstTerm (UP up:unipoly) = up.[0]

  (*
      Multiplies an unipoly with a term (of a constant and an exponent)
  *)
  let multUnipoly (UP up:unipoly) (exp, con) : unipoly =
    UP (List.foldBack (fun (n,c) acc -> (n + exp, c * con)::acc) up [])

  (*
      Negates an unipoly
  *)
  let negateUnipoly (UP up:unipoly) = UP (List.foldBack (fun (n,c) acc -> (n, -c)::acc) up [])

  (*
      Very small number. Considered as good as zero.
  *)
  let epsilon = 10.**(-20.)

  (*
      Subtracts an unipoly, up2, from another unipoly, up1. I.e up1 - up2
  *)
  let subtractUnipoly (UP up1:unipoly) (UP up2:unipoly) : unipoly =
    let rec inner res (sub:(int*float) list) = function
    | []        ->  if not sub.IsEmpty then 
                      let (UP rest) = negateUnipoly (UP sub)
                      res @ rest
                    else res
    | (n,c)::cr ->  if sub.IsEmpty then
                      if abs c < epsilon then inner res sub cr
                      else inner (res @ [(n, c)]) sub cr
                    else 
                      let sn,sc = sub.[0]
                      if sn = n then
                        let v = c - sc
                        if abs v < epsilon then inner res sub.Tail cr
                        else inner (res @ [(n, v)]) sub.Tail cr
                      else if sn < n then inner (res @ [(n,c)]) sub cr
                      else inner (res @ [(sn, -sc)]) sub.Tail ((n,c)::cr)
    UP (inner [] up2 up1)
  
  (*
      Returns the degree of a univariate polynomial
  *)
  let getDegree (UP up:unipoly) =
    let (degree,_) = up.[0]
    degree
  
  let isEmpty (UP up:unipoly) = up.IsEmpty

  (*
      Subtraction and multiplication operators for unipoly
  *)
  type unipoly with
    static member ( - ) (up1, up2)    = subtractUnipoly up1 up2
    static member ( * ) (up1, (n, c)) = multUnipoly up1 (n, c)

  (*
      Exercises polynomial long division on two unipolies.
      up1 is the dividend, up2 is the divisor.

      It is assumed that up2 is of a lower degree than up1

      Only the remainder of the operation is returned,
      in other words the quotient is not collected (it is commented out of the running code)

      Logic is inspired from https://rosettacode.org/wiki/Polynomial_long_division#OCaml

      Potential for an endless loop, if no epsilon is used in subtractUnipoly, or if the epsilon is two big
  *)
  let unipolyLongDiv up1 up2 : unipoly = // * unipoly =
    // s is the smaller, f the larger, q is quotient.
    let rec inner (f:unipoly) (s:unipoly) = //(UP q:unipoly) =
      if getDegree f - getDegree s < 0 then f//(UP q, f) // the difference between the degrees of f and s
      else
        let (fExp, fConst) = getFirstTerm f // dividend's highest degree term
        let (sExp, sConst) = getFirstTerm s // divisor's highest degree term
        let k = fExp - sExp, fConst / sConst // division of the two terms
        let ks = s * k // k multiplied into the divisor poly
        //let q' = UP (q @ [k]) // k added to the current quotient
        let f' = f - ks
        if isEmpty f' then f//(UP q, f)
        else inner f' s //q'
    inner up1 up2 //(UP [])

  (*
      Modulo operator for unipoly, returns the remainder of polynomial long division on two unipolies
  *)
  type unipoly with 
    static member ( % ) (up1, up2) = unipolyLongDiv up1 up2

  (*
      Generates a Sturm sequence chain for a unipoly, up, and its derivate, up'
  *)
  let sturmSeq up up' : unipoly list =
    let rec inner (uplist: unipoly list) = 
      match getDegree uplist.[0] with
      | 0 -> uplist
      | _ -> inner ((negateUnipoly (uplist.[1] % uplist.[0])) :: uplist)
    inner [up';up] // p0 will always be the last element in the list
  
  (*
      Counts sign changes in a unipoly list, for a given value inserted in the variable's place
  *)
  let countSignChanges uplist x =
    let rec inner fmr cnt = function
    | []        -> cnt
    | up::rest  ->
        let rs = solveUnipoly up x
        if (rs > 0.0 && fmr > 0.0) || (rs < 0.0 && fmr < 0.0)
          then inner rs cnt rest
        else inner rs (cnt + 1) rest
    inner 0.0 0 uplist

  (*
      Finds the smallest interval where the smallest root lives.
      
      Uses binary search to recursively split the search space in halves, and checks
      the number of real roots that lives in the current space.
      
      Runs for maxDepth or when no roots exists in the space
  *)
  let getInterval uplist intvallo intvalhi maxdepth =
    let roots g1 g2 =  
      let s1 = countSignChanges uplist g1
      let s2 = countSignChanges uplist g2
      s1 - s2
    let rec search lo hi currentdepth =
      let mid = (lo + hi) / 2.
      if currentdepth >= maxdepth then Some (lo, hi, mid)
      else
        let lodiff = roots lo mid
        if lodiff > 0 then search lo mid (currentdepth + 1)
        else
          let hidiff = roots mid hi
          if hidiff > 0 then search mid hi (currentdepth + 1)
          else None
    search intvallo intvalhi 0