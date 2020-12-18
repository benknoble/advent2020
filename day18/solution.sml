structure Solution = struct

  datatype expr = Num of int
                | Add of expr * expr
                | Mult of expr * expr

  fun eval_expr e =
    case e
      of Num n => n
       | Add (e1, e2) => eval_expr e1 + eval_expr e2
       | Mult (e1, e2) => eval_expr e1 * eval_expr e2

  structure BasicMath = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun nump getc = (decp $> Num) getc

    (* based loosely on the example at
     * https://www.smlnj.org/doc/smlnj-lib/Util/str-ParserComb.html
     *
     * the difference is we have more than one possible sub-expression in the
     * left-to-right chain produced by ?+ (expr') whereas the example is all
     * addition
     * so we pair them with the expr-building function (addp, multp use Add and
     * Mult) and then apply that in exprp *)
    fun basep getc = (nump || parenp) getc
    and parenp getc =
      ((skip_ws (PC.char #"(") +> exprp +> PC.char #")")
      $> (#1 o #2))
      getc
    and addp getc =
      ((skip_ws (PC.char #"+") +> basep)
      $> (fn (_, right) => (Add, right)))
      getc
    and multp getc =
      ((skip_ws (PC.char #"*") +> basep)
      $> (fn (_, right) => (Mult, right)))
      getc
    and exprp' getc = (?+ (addp || multp)) getc
    and exprp getc =
      ((basep +> exprp')
      $> (fn (left, rights) => List.foldl (fn ((f, a), b) => f (b, a)) left rights))
      getc

    fun exprsp getc = (?+ (skip_ws exprp)) getc

    val exprs = prun exprsp
  end

  structure AdvancedMath = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    (* straightforward stratified LL(1) expression grammar with + and ⋅ flipped:
     * E := T ('⋅' E)?
     * T := F ('+' T)?
     * F := number | '(' E ')' *)
    fun exprp getc =
      ((termp +> ?? multp)
      $> (fn (left, right) => case right
                                of NONE => left
                                 | SOME right' => Mult (left, right')))
      getc
    and termp getc =
      ((basep +> ?? addp)
      $> (fn (left, right) => case right
                                of NONE => left
                                 | SOME right' => Add (left, right')))
      getc
    and multp getc =
      ((skip_ws (PC.char #"*") +> exprp) $> #2)
      getc
    and addp getc =
      ((skip_ws (PC.char #"+") +> termp) $> #2)
      getc
    and basep getc = (BasicMath.nump || parenp) getc
    and parenp getc =
      ((skip_ws (PC.char #"(") +> exprp +> PC.char #")")
      $> (#1 o #2))
      getc

    fun exprsp getc = (?+ (skip_ws exprp)) getc

    val exprs = prun exprsp
  end

  val sum_expr = List'.sum o (List.map eval_expr)
  fun part f = Option.map sum_expr o f o Readers.all o Readers.file
  val part1 = part BasicMath.exprs
  val part2 = part AdvancedMath.exprs

end
