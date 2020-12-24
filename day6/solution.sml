signature DAY6 = sig
  type answer
  type individual_answers
  type group
  type group_answers

  val individual_answers: answer -> individual_answers
  val group_answers: (group_answers * group_answers -> group_answers) -> group_answers list -> group_answers

  structure Form: sig
    val answerp': (individual_answers, 'strm) ParserComb.parser
    val groupp': (group_answers * group_answers -> group_answers) -> (group_answers, 'strm) ParserComb.parser

    val answerp: string -> individual_answers option
    val groupp: (group_answers * group_answers -> group_answers) -> string -> group_answers option

    val forms: (group_answers * group_answers -> group_answers) -> string -> group_answers option list
  end

  val part1': group_answers list -> int
  val part1: string -> int
  val part2: string -> int
end

structure Solution: DAY6 = struct

  structure CharSet = CharMap'.KeySet

  type answer = char list
  type individual_answers = CharSet.set
  type group = answer list
  type group_answers = CharSet.set

  val individual_answers = CharSet.fromList
  fun group_answers comb xs =
    case xs
      of [] => CharSet.empty
       | [x] => x
       | h::t => List.foldl comb h t

  structure Form = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun answerp' getc = (++ (PC.eatChar Char.isLower) $> individual_answers) getc
    fun groupp' comb getc =
      (++ (answerp' +> PC.char #"\n") $> ((group_answers comb) o (List.map #1)))
      getc

    val answerp = prun answerp'
    val groupp = prun o groupp'

    fun forms comb = (List.map (groupp comb)) o Readers.blank_line_sep_records
  end

  val part1' = List'.sum o (List.map CharSet.numItems)
  val part1 = part1' o (List.map Option.valOf) o (Form.forms CharSet.union) o Readers.all o Readers.file
  val part2 = part1' o (List.map Option.valOf) o (Form.forms CharSet.intersection) o Readers.all o Readers.file

end
