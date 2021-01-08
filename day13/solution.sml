signature DAY13 = sig
  structure Timetable: sig
    val earliestp: (int, 'strm) ParserComb.parser
    val idp: (int option, 'strm) ParserComb.parser
    val idsp: (int option list, 'strm) ParserComb.parser
    val timetablep: (int * int option list, 'strm) ParserComb.parser
    val timetable: string -> (int * int option list) option
  end

  val next_multiple: int -> int -> int
  val next_bus: int -> int list -> {bus: int, arrival: int} option

  val part1': int -> int list -> int option
  val part1: string -> int option

  val congruences: int option list -> (int * int) list

  val part2': int option list -> int
  val part2: string -> int option
end

structure Solution: DAY13 = struct

  structure Timetable = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun earliestp getc = ((decp +> PC.char #"\n") $> #1) getc

    fun idp getc =
      ((decp $> SOME) || (PC.char #"x" $> Lambda.k NONE))
      getc

    fun idsp getc =
      ((idp +> ++ (PC.char #"," +> idp))
      $> (fn (first, rest) => first::(List.map #2 rest)))
      getc

    fun timetablep getc = (earliestp +> idsp) getc
    val timetable = prun timetablep

  end

  fun next_multiple target factor =
    target + (factor - target mod factor)

  fun next_bus earliest buses =
    let
      val next_buses =
        List.map (fn b => (b, next_multiple earliest b)) buses
      val find_min =
        List.foldl (fn (curr, smallest) => if #2 curr < #2 smallest
                                           then curr
                                           else smallest)
      val smallest = Option.map (fn (h, t) => find_min h t) (List.getItem next_buses)
    in
      Option.map (fn (bus, arrival) => {bus=bus, arrival=arrival}) smallest
    end

  fun part1' earliest =
    Option.map (fn {bus, arrival} => bus * (arrival - earliest))
    o (next_bus earliest)

  val part1 =
    (Option.mapPartial (fn (earliest, buses) => part1' earliest (List.mapPartial Lambda.id buses)))
    o Timetable.timetable
    o Readers.all
    o Readers.file

  fun congruences buses =
    List.map (fn (i, v) => (~i, Option.valOf v))
    (List.filter (Option.isSome o #2)
    (List'.with_indices buses))

  val part2' = Math'.crt o congruences
  val part2 =
    Option.map (part2' o #2)
    o Timetable.timetable
    o Readers.all
    o Readers.file

end
