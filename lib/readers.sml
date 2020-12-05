structure Readers = struct

  fun collect_t t_to_string = List.mapPartial t_to_string
  fun on_char c = String.tokens (Lambda.is c)
  fun on_chars cs = String.tokens (fn c => List.exists (Lambda.is c) cs)
  val on_spaces = String.tokens Char.isSpace
  val lines = on_char #"\n"
  val all = TextIO.inputAll
  val file = TextIO.openIn

  val file_to_int_list = (collect_t Int.fromString) o on_spaces o all o file

  structure Map = struct

    fun fromString insert base_map char_to_terrain =
      let
        val lines = (List.map String.explode) o lines
        fun acc' acc x y c =
          case char_to_terrain c
            of NONE => acc
             | SOME t => insert (acc, (Point.new x y), t)
        fun readRow row y acc =
          #2 (List.foldl (fn (c, (x, acc)) => (x+1, acc' acc x y c)) (0, acc) row)
        fun readLines lines =
          #2 (List.foldl (fn (l, (y, acc)) => (y+1, readRow l y acc)) (0, base_map) lines)
      in
        readLines o lines
      end

  end

  structure PC = ParserComb
  val prun: ('a, StringCvt.cs) PC.parser -> string -> 'a option = StringCvt.scanString

end
