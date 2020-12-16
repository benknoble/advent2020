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

  fun solve_set_eqns cands =
    if List.all ((Lambda.is 1) o RuleSet.numItems) cands
    then SOME (List.concat (List.map RuleSet.toList cands))
    else
      let
        val fixed =
          List.foldl RuleSet.union RuleSet.empty
          (List.filter ((Lambda.is 1) o RuleSet.numItems) cands)
      in
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
