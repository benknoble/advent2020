structure Solution = struct

  structure CharSet = CharMap'.KeySet

  type answer = char list
  type individual_answers = CharSet.set
  type group = answer list
  type group_answers = CharSet.set

  val individual_answers = CharSet.fromList
  val group_answers = List.foldl CharSet.union CharSet.empty

  structure Form = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun answerp' getc = (++ (PC.eatChar Char.isLower) $> individual_answers) getc
    fun groupp' getc =
      (++ (answerp' +> PC.char #"\n") $> (group_answers o (List.map #1)))
      getc

    val answerp = prun answerp'
    val groupp = prun groupp'

    val forms = (List.map groupp) o Readers.blank_line_sep_records
  end

  val part1' = List'.sum o (List.map CharSet.numItems)
  val part1 = part1' o (List.map Option.valOf) o Form.forms o Readers.all o Readers.file

end
