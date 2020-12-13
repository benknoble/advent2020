structure Solution = struct

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

end
