structure Solution = struct
  type field = string
  type value = int
  type ticket = value list
  datatype range = Range of Range.range
                 | Or of range * range
  type rule = field * range

  type tickets = rule list * ticket * ticket list

  structure Tickets = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun fieldp getc =
      ((PC.token (not o Lambda.is #":") +> PC.char #":")
      $> #1)
      getc

    fun rangep getc =
      let
        fun rangep' getc =
          ((decp +> PC.char #"-" +> decp)
          $> (fn (min, (_, max)) => Range {min=min, max=max}))
          getc
      in
        ((rangep'
          +> ++ ((PC.string " or " +> rangep') $> #2))
        $> (fn (first, rest) => List.foldr Or first rest))
        getc
      end

    fun rulep getc = (fieldp +> (skip_ws rangep)) getc
    fun rulesp getc = (++ (skip_ws rulep)) getc

    fun ticketp getc =
      ((decp +> ++ ((PC.char #"," +> decp) $> #2)) $> op::)
      getc
    fun ticketsp' getc = (++ (skip_ws ticketp)) getc

    fun ticketsp getc =
      ((rulesp
        +> (skip_ws (PC.string "your ticket:\n"))
        +> ticketp
        +> (skip_ws (PC.string "nearby tickets:\n"))
        +> ticketsp')
      $> (fn (rules, (_, (mine, (_, nearby)))) => (rules, mine, nearby)))
      getc

    val tickets = prun ticketsp
  end

  fun range_valid range value =
    case range
      of Range r => Range.includes r value
       | Or (r1, r2) => range_valid r1 value orelse range_valid r2 value

  fun rule_valid (_, range) value =
    range_valid range value

  fun rule_invalid r = not o (rule_valid r)

  fun invalid_for_all rules v = List.all (fn r => rule_invalid r v) rules

  fun part1' (rules, _, nearby) =
    let
      val invalid =
        List.map
        (#1 o List.partition (invalid_for_all rules))
        nearby
    in
      List'.sum (List.concat invalid)
    end

  val part1 = (Option.map part1') o Tickets.tickets o Readers.all o Readers.file

end
