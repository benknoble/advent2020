structure Solution = struct

  datatype expr = Num of int
                | Add of expr * expr
                | Mult of expr * expr

  fun eval_expr e =
    case e
      of Num n => n
       | Add (e1, e2) => eval_expr e1 + eval_expr e2
       | Mult (e1, e2) => eval_expr e1 * eval_expr e2

  structure Math = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun nump getc = (decp $> Num) getc

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

  val part1' = List'.sum o (List.map eval_expr)
  val part1 = Option.map part1' o Math.exprs o Readers.all o Readers.file

end
