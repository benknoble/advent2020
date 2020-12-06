structure Solution = struct

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
