signature DAY16 = sig
  type field
  type value
  type ticket
  datatype range = Range of Range.range
                 | Or of range * range
  type rule
  type tickets

  structure Tickets: sig
    val fieldp: (string, 'strm) ParserComb.parser
    val rangep: (range, 'strm) ParserComb.parser
    val rulep: (rule, 'strm) ParserComb.parser
    val rulesp: (rule list, 'strm) ParserComb.parser
    val ticketp: (ticket, 'strm) ParserComb.parser
    val ticketsp': (ticket list, 'strm) ParserComb.parser
    val ticketsp: (tickets, 'strm) ParserComb.parser
    val tickets: string -> tickets option
  end

  val range_valid: range -> value -> bool
  val rule_valid: rule -> value -> bool
  val valid_for_any: rule list -> value -> bool
  val rule_invalid: rule -> value -> bool
  val invalid_for_all: rule list -> value -> bool

  val part1': tickets -> int
  val part1: string -> int option

  structure RuleSet: ORD_SET where type Key.ord_key = rule

  val valid_for_all: rule list -> value list -> RuleSet.set

  val solve_set_eqns: RuleSet.set list -> rule list option

  val part2': tickets -> int option
  val part2: string -> int option option
end

structure Solution: DAY16 = struct
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
  fun valid_for_any rules v = List.exists (fn r => rule_valid r v) rules

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

  structure RuleSet = RedBlackSetFn(struct
    type ord_key = rule
    fun compare ((f1, _), (f2, _)) = String.compare (f1, f2)
  end)

  fun valid_for_all rules vs =
    RuleSet.fromList
      (List.filter
      (fn r => List.all (rule_valid r) vs)
      rules)

  (* cands is a list of sets, so acts like the mapping
   * index -> {possible rules}
   * we want to find the mapping
   * index -> rule
   * if it exists such that every rule has a unique index and every index has a
   * unique rule
   * the result is a list of rules *)
  fun solve_set_eqns cands =
    (* all singletons, solved *)
    if List.all ((Lambda.is 1) o RuleSet.numItems) cands
    then SOME (List.concat (List.map RuleSet.toList cands))
    else
      let
        (* set of already solved equations *)
        val fixed =
          List.foldl RuleSet.union RuleSet.empty
          (List.filter ((Lambda.is 1) o RuleSet.numItems) cands)
      in
        (* if none are already solved, there is no unique solution
         * there may be multiple solutions, but we want the unique one *)
        if RuleSet.isEmpty fixed
        then NONE
        else
          let
            val sans_fixed =
              List.map
              (fn c => if RuleSet.numItems c = 1
                       then c (* only subtract from the non-singletons *)
                       else RuleSet.difference (c, fixed))
              cands
          in
            (* if there are empty sets, there is no solution *)
            if List.exists RuleSet.isEmpty sans_fixed
            then NONE
            else solve_set_eqns sans_fixed
          end
      end

  fun part2' (rules, mine, nearby) =
    let
      val valid =
        mine::(List.filter (List.all (valid_for_any rules)) nearby)
      val by_field = List'.transpose valid
      val cands = List.map (valid_for_all rules) by_field
    in
      Option.map
      (List'.prod (* product *)
      o List.map (fn (i, _) => List.nth (mine, i)) (* my departures *)
      o List.filter (String.isPrefix "departure" o #1 o #2) (* starts with departure *)
      o List'.with_indices (* label the fields *))
      (solve_set_eqns cands)
    end

  val part2 = (Option.map part2') o Tickets.tickets o Readers.all o Readers.file

end
