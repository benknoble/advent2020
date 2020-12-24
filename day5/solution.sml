signature DAY5 = sig
  datatype row = F | B
  datatype column = L | R
  type seat
  type seat_nr

  val seat_nr: seat -> seat_nr
  val seat_id: seat_nr -> int

  structure Pass: sig
    val rowp: (row, 'strm) ParserComb.parser
    val columnp: (column, 'strm) ParserComb.parser
    val rowsp: (row list, 'strm) ParserComb.parser
    val columnsp: (column list, 'strm) ParserComb.parser

    val pass': (seat, 'strm) ParserComb.parser
    val pass: string -> seat option
    val passes: string -> seat option list
  end

  val part1': seat list -> int
  val part1: string -> int

  val ids: seat list -> IntRedBlackSet.set
  val candidates: seat list -> IntRedBlackSet.set * int list

  val part2': seat list -> int list
  val part2: string -> int list
end

structure Solution: DAY5 = struct

  datatype row = F | B
  datatype column = L | R
  type seat = row list * column list
  type seat_nr = {row: int, column: int}

  fun seat_nr (rows, columns) =
    let
      val rows = List.map (fn F => Range.Lower | B => Range.Upper) rows
      val columns = List.map (fn L => Range.Lower | R => Range.Upper) columns
    in
      {row=Range.bsp 0 rows, column=Range.bsp 0 columns}
    end

  fun seat_id {row, column} = row * 8 + column

  structure Pass = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun rowp getc =
      ((PC.char #"F" $> Lambda.k F) || (PC.char #"B" $> Lambda.k B))
      getc

    fun columnp getc =
      ((PC.char #"L" $> Lambda.k L) || (PC.char #"R" $> Lambda.k R))
      getc

    fun rowsp getc = (++ rowp) getc
    fun columnsp getc = (++ columnp) getc

    fun pass' getc = (rowsp +> columnsp) getc
    val pass = prun pass'
    val passes = (List.map pass) o Readers.lines
  end

  val part1' = (List.foldl Int.max ~1) o (List.map (seat_id o seat_nr))
  (* any failures I want to be loud *)
  val part1 = part1' o (List.map Option.valOf) o Pass.passes o Readers.all o Readers.file

  val ids = IntRedBlackSet.fromList o List.map (seat_id o seat_nr)

  fun candidates passes =
    let
      val max = part1' passes
      val ids = ids passes
      val allids = Range.toList {min=0, max=max}
      fun filled id = IntRedBlackSet.member (ids, id)
    in
      (ids, List.filter (not o filled) allids)
    end

  fun part2' passes =
    let
      val (ids, candidates) = candidates passes
      fun has_neighbor id = IntRedBlackSet.member (ids, id-1) andalso IntRedBlackSet.member (ids, id+1)
    in
      List.filter has_neighbor candidates
    end
  val part2 = part2' o (List.map Option.valOf) o Pass.passes o Readers.all o Readers.file

end
