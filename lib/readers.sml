signature MAP_READER = sig
  val fromString: ('a * Point.point * 'b -> 'a) -> 'a -> (char -> 'b option) -> string -> 'a
end

signature PARSEROPS = sig
  structure PC: PARSER_COMB
  val $> : ('a, 'strm) PC.parser * ('a -> 'b) -> ('b, 'strm) PC.parser
  val +> : ('a, 'strm) PC.parser * ('b, 'strm) PC.parser -> ('a * 'b, 'strm) PC.parser
  val >> : ('a, 'strm) PC.parser * ('a -> ('b, 'strm) PC.parser) -> ('b, 'strm) PC.parser
  val || : ('a, 'strm) PC.parser * ('a, 'strm) PC.parser -> ('a, 'strm) PC.parser
  val ||| : ('a, 'strm) PC.parser list -> ('a, 'strm) PC.parser
  val ?+ : ('a, 'strm) PC.parser -> ('a list, 'strm) PC.parser
  val ++ : ('a, 'strm) PC.parser -> ('a list, 'strm) PC.parser
  val ?? : ('a, 'strm) PC.parser -> ('a option, 'strm) PC.parser
  val !! : ('a option, 'strm) PC.parser -> ('a, 'strm) PC.parser

  val skip_ws: ('a, 'strm) PC.parser -> ('a, 'strm) PC.parser

  val prun: ('a, StringCvt.cs) PC.parser -> string -> 'a option

  val anyc: (char, 'strm) PC.parser

  val stop: 'a list -> 'b -> ('b, 'strm) PC.parser
  val finish: ('a, 'strm) PC.parser -> ('a, 'strm) PC.parser

  val digitp: (char, 'strm) PC.parser
  val hexdigitp: (char, 'strm) PC.parser
  val decp: (int, 'strm) PC.parser
  val hexp: (int, 'strm) PC.parser

  val mkint: char list -> int
  val mkhex: char list -> int
end

signature DICT_READER = sig
  val kvp: (Atom.atom * string, 'strm) ParserComb.parser
end

signature READERS = sig
  val collect_t: (string -> 'a option) -> string list -> 'a list
  val on_char: char -> string -> string list
  val on_chars: char list -> string -> string list
  val on_spaces: string -> string list
  val lines: string -> string list
  val all: TextIO.instream -> TextIO.vector
  val file: string -> TextIO.instream
  val file_to_int_list: string -> int list
  val blank_line_sep_records: string -> string list
  structure Map: MAP_READER
  structure ParserOps: PARSEROPS
  structure Dict: DICT_READER
end

structure Readers: READERS = struct

  fun collect_t t_to_string = List.mapPartial t_to_string
  fun on_char c = String.tokens (Lambda.is c)
  fun on_chars cs = String.tokens (fn c => List.exists (Lambda.is c) cs)
  val on_spaces = String.tokens Char.isSpace
  val lines = on_char #"\n"
  val all = TextIO.inputAll
  val file = TextIO.openIn

  val file_to_int_list = (collect_t Int.fromString) o on_spaces o all o file

  val blank_line_sep_records =
    (List.map (String.concatWith "\n"))
    o (List.foldl (fn (line, acc) =>
        if line = ""
        then []::acc
        else case acc
                of h::t => if List.null h
                          then [line ^ "\n"]::t
                          else (line::h)::t
                | [] => [[line ^ "\n"]])
      [])
    o (String.fields (Lambda.is #"\n"))

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

  structure ParserOps = struct
    structure PC = ParserComb

    val $> = PC.wrap
    val +> = PC.seq
    val >> = PC.bind
    val || = PC.or
    val ||| = PC.or'
    val ?+ = PC.zeroOrMore
    val ++ = PC.oneOrMore
    val ?? = PC.option
    val !! = PC.join

    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun skip_ws getc = PC.skipBefore Char.isSpace getc

    val prun: ('a, StringCvt.cs) PC.parser -> string -> 'a option = StringCvt.scanString

    fun anyc getc = (PC.eatChar (Lambda.k true)) getc

    fun anything getc = (?+ anyc) getc

    fun stop rest result =
      if List.null rest
      then PC.result result
      else PC.failure

    fun finish p getc =
      ((p +> anything) >> (fn (result, rest) => stop rest result))
      getc

    fun digitp getc = PC.eatChar Char.isDigit getc
    fun hexdigitp getc = PC.eatChar Char.isHexDigit getc
    fun decp getc = Int.scan StringCvt.DEC getc
    fun hexp getc = Int.scan StringCvt.HEX getc

    val mkint = Option.valOf o Int.fromString o String.implode
    val mkhex = Option.valOf o prun hexp o String.implode
  end

  (* open Readers.ParserOps *)
  (* infixr 3 $> *)
  (* infixr 3 +> *)
  (* infixr 3 >> *)
  (* infixr 3 || *)

  structure Dict = struct
    open ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun kvp getc =
      ((PC.token Char.isAlpha +> skip_ws (PC.char #":") +> PC.token (not o Char.isSpace))
      $> (fn (k, (_, v)) => (Atom.atom k, v)))
      getc

  end

end
