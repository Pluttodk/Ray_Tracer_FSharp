namespace Tracer

module ExprParse =
  open Tracer.Basics

  let realRoot value degree =
    if degree <= 0 then invalidArg "degree" "A root degree must be positive."
    if value < 0. && degree % 2 = 1 then -((-value) ** (1. / float degree))
    else value ** (1. / float degree)

  type terminal = 
    | Add               // addition
    | Mul               // multiplication
    | Div               // Division
    | Pwr               // Power
    | Root              // Root    
    | Neg
    | Lpar              // Left parenthesis
    | Rpar              // Right parenthesis
    | Int of int        // Wrapped value of integer
    | Float of float    // Wrapped value of float
    | Var of string     // Wrapped variable of string

  let isblank c = System.Char.IsWhiteSpace c
  let isdigit c = c >= '0' && c <= '9'
  let isletter c = System.Char.IsLetter c
  let isletterdigit c = System.Char.IsLetterOrDigit c

  let explode s = [for c in s -> c]

  let floatval (c:char) = float((int)c - (int)'0')
  let intval (c:char) = (int)c - (int)'0'

  exception ScanErrorException

  let rec scnum (cs, value) = 
    match cs with 
    | '.' :: c :: cr when isdigit c       -> scfrac(c :: cr, (float)value, 0.1)
    | c :: cr when isdigit c              ->
        let digit = intval c
        if value > (System.Int32.MaxValue-digit)/10 then scfloat(cr, 10.*float value + float digit)
        else scnum(cr, 10*value + digit)
    | _                                   -> (cs,Int value) // Number without fraction is an integer
  and scfloat (cs, value) =
    if not (System.Double.IsFinite value) then raise ScanErrorException
    match cs with
    | '.' :: c :: cr when isdigit c -> scfrac(c::cr, value, 0.1)
    | c :: cr when isdigit c -> scfloat(cr, 10.*value + float (intval c))
    | _ -> cs, Float value
  and scfrac (cs, value, wt) =
    match cs with
    | c :: cr when isdigit c  -> scfrac(cr, value+wt*floatval c, wt/10.0)
    | _                       -> (cs, Float value)

  let rec scname (cs, value) =
    match cs with
    | c :: cr when isletterdigit c  -> scname(cr, value + c.ToString())
    | _                             -> (cs, value)
  
  (*
      Scans a string/char seq, and returns a list of terminals
  *)
  let scan s =
    let negateNumber = function
        Int i -> Int -i
      | Float f -> Float -f
      | _ -> raise ScanErrorException // Expected a number
    let rec sc expectsOperand cs =
      match cs with
      | []                              -> []
      | '+' :: cr                       -> Add :: sc true cr
      | '*' :: cr                       -> Mul :: sc true cr
      | '^' :: cr                       -> Pwr :: sc true cr
      | '/' :: cr                       -> Div :: sc true cr
      | '(' :: cr                       -> Lpar :: sc true cr
      | ')' :: cr                       -> Rpar :: sc false cr
      | '_' :: cr                       -> Root :: sc true cr
      | '-' :: cr when expectsOperand ->
          match List.skipWhile isblank cr with
          | c :: rest when isdigit c ->
              let remaining, number = scnum(rest, intval c)
              negateNumber number :: sc false remaining
          | rest -> Neg :: sc true rest
      | '-' :: cr -> Add :: Int -1 :: Mul :: sc true cr
      | c :: cr when isdigit c          -> let (cs1, t) = scnum(cr, intval c)
                                           t :: sc false cs1
      | c :: cr when isblank c          -> sc expectsOperand cr
      | c :: cr when isletter c         -> let (cs1, n) = scname(cr, (string)c)
                                           Var n :: sc false cs1
      | _                               -> raise ScanErrorException
    sc true (explode s)

  (*
      Active patterns on terminals
  *)
  let (|Lterm|NoMatch|) left = 
    match left with
    | Float _ | Var _ | Int _ | Rpar -> Lterm left
    | _                       -> NoMatch
  let (|Rterm|NoMatch|) right =
    match right with
    | Float _ | Var _ | Int _ | Lpar _  -> Rterm right
    | _                                 -> NoMatch

  (*
      Inserts multiply terminals between terms where it has been implicit in the scanned string
  *)
  let rec insertMult = function
    | Lterm left::Rterm right::ts -> left::Mul::insertMult (right::ts)
    | t::ts                       -> t::insertMult ts
    | []                          -> []

  type expr = 
    | FNum of float
    | FVar of string
    | FAdd of expr * expr
    | FMult of expr * expr
    | FDiv of expr * expr
    | FExponent of expr * int
    | FRoot of expr * int

  exception ParseErrorException

  (* 
      Grammar:
      E    = T Eopt .
      Eopt = "+" T Eopt | e .
      T    = F Topt .
      Topt = "*" F Topt | "/" F Topt | e .
      F    = P Fopt .
      Fopt = "^" Int | "_" Int | e .
      P    = "-" P | Int | Float | Var | "(" E ")" .
      e is the empty sequence.
      Signed literals and unary negation are primaries, so -x^2 means (-x)^2.
      Use -(x^2) to negate the power. Binary subtraction remains addition of -1 times a term.
  *)
  let rec E (ts:terminal list) = (T >> Eopt) ts // or Eopt (T ts), and the full composition translates to Eopt (Topt (Fopt (P ts)))
  and Eopt (ts, (inval)) = 
    match ts with 
    | Add::tr   -> let (ts1, tv) = T tr
                   Eopt (ts1, FAdd (inval, tv))
    | _         -> (ts, inval)
  and T ts = (F >> Topt) ts // or Topt (F ts)
  and Topt (ts, inval) =
    match ts with
    | Mul::tr   -> let (ts1, fv) = F tr // fv = factor value?
                   Topt (ts1, FMult (inval, fv))
    | Div::tr   -> let (ts1, fv) = F tr
                   Topt (ts1, FDiv (inval, fv))                       
    | _         -> (ts, inval)          
  and F ts = (P >> Fopt) ts // or Fopt (P ts)
  and Fopt (ts, inval) =
    match ts with
    | Pwr::Int i::tr  -> (tr, FExponent (inval, i))
    | Root::Int i::tr when i > 0 -> (tr, FRoot (inval, i))
    | Root::_ -> raise ParseErrorException
    | _               -> (ts, inval)                    
  and P ts = // this function is executed first?
    match ts with
    | Neg::tr ->
        let remaining, operand = P tr
        remaining, FMult(FNum -1., operand)
    | Float r::tr -> (tr, FNum r)
    | Int i::tr   -> (tr, FNum (float i))
    | Var x::tr   -> (tr, FVar x)
    | Lpar::tr    -> let (ts1, ev) = E tr
                     match ts1 with
                     | Rpar :: tr -> (tr, ev)
                     | _          -> raise ParseErrorException
    | Add::tr -> P tr // if the original equation started with a negative term, we end here.
    | _           -> raise ParseErrorException

  let parse ts : expr= 
    match E ts with
    | ([], result)  -> result
    | _             -> raise ParseErrorException

  (*
      Reduces all possible expressions that include numbers
  *)
  let rec reduceExpr e =
    let rec inner = function
      | FNum value when not (System.Double.IsFinite value) ->
          invalidArg "expression" "Expression constants and constant arithmetic must be finite."
      // remove zero-terms
      | FAdd(e1, FNum 0.0) -> inner e1
      | FAdd(FNum 0.0, e1) -> inner e1
      | FMult(FNum 0.0, _) -> FNum 0.0
      | FMult(_, FNum 0.0) -> FNum 0.0
      | FDiv(FNum 0.0, _)  -> FNum 0.0
      | FDiv(_, FNum 0.0)  -> raise ParseErrorException
      // some small simplifications with numbers
      | FAdd(FNum c1, FNum c2)  -> FNum (c1 + c2)
      | FMult(FNum c1, FNum c2) -> FNum (c1 * c2)
      | FRoot(e, 1) -> inner e
      | FRoot(FNum c1, n) as original ->
          if not (System.Double.IsFinite c1) then invalidArg "expression" "Root constants must be finite."
          let value = realRoot c1 n
          if System.Double.IsFinite value then FNum value else original
      | FDiv(FNum c1, FNum c2)  -> FNum (c1 / c2)
      | FExponent(FNum c1,n)    -> FNum (pown c1 n)
      // all others should just continue recursively
      | FRoot(e1,n)        -> FRoot (inner e1, n)
      | FAdd(e1,e2)        -> FAdd (inner e1, inner e2)
      | FMult(e1,e2)       -> FMult (inner e1, inner e2)
      | FDiv(e1,e2)        -> FDiv (inner e1, inner e2)
      | FExponent(e1,n)    -> FExponent (inner e1, n)
      | ex                 -> ex // FVar and FNum
    let altered = inner e
    if e = altered then e
    else reduceExpr altered

  (*
      Given a point, with values for x, y, and z, solves the expression
  *)
  let rec solveExpr (p:Point) = function
  | FNum c          -> c
  | FVar s          -> match s with
                       | "x" -> p.X
                       | "y" -> p.Y
                       | "z" -> p.Z
                       | _    -> failwith "solveExpr: unmatched variable"
  | FRoot(e1,n)     -> realRoot (solveExpr p e1) n
  | FAdd(e1,e2)     -> solveExpr p e1 + solveExpr p e2
  | FMult(e1,e2)    -> solveExpr p e1 * solveExpr p e2
  | FDiv(e1,e2)     -> solveExpr p e1 / solveExpr p e2
  | FExponent(e1,n) -> pown (solveExpr p e1) n

  (*
      Runs all the above functions for a string equation.
      Returns an expr
  *)
  let parseStr s = (scan >> insertMult >> parse) s