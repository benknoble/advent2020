structure Solution = struct

  datatype rule = Char of char
                | Alt of int list * int list
  type rules = rule IntRedBlackMap.map

  structure Data = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun labelp getc = decp getc

    fun charp getc =
      ((PC.char #"\"" +> anyc +> PC.char #"\"")
      $> (fn (_, (c, _)) => Char c))
      getc

    fun seqp getc =
      ((labelp +> ?+ ((PC.string " " +> labelp) $> #2))
      $> op::)
      getc

    fun altp getc =
      ((seqp +> ?? (PC.string " | " +> seqp))
      $> (fn (left, rightm) =>
            Alt (left, case rightm
                         of NONE => []
                          | SOME (_, right) => right)))

      getc

    fun rhsp getc = (charp || altp) getc

    fun rulep getc =
      ((decp +> PC.string ": " +> rhsp +> ?? (PC.char #"\n"))
      $> (fn (i, (_, (r, _))) => (i, r)))
      getc

    fun rulesp getc =
      (?+ rulep
      $> List.foldl IntRedBlackMap.insert' IntRedBlackMap.empty)
      getc

    fun run_rulep n rs extra acc getc =
      let
        fun run_seq rs' =
          List.foldl
          (fn (curr, acc') =>
            ((acc' +> (run_rulep curr rs extra acc))
            $> (IntRedBlackMap.unionWith op+)))
          (PC.result acc)
          rs'
      in
        case extra (acc, n)
          of SOME p => p getc
           | NONE =>
               (case IntRedBlackMap.lookup (rs, n)
                  of Char c =>
                       (PC.char c
                       $> Lambda.k (IntRedBlackMap.insertWith op+ (acc, n, 1)))
                       getc
                   | Alt (r1s, r2s) =>
                       ((case (List.length r1s, List.length r2s)
                           of (0, 0) => anything >> Lambda.k PC.failure
                            | (0, _) => run_seq r2s
                            | (_, 0) => run_seq r1s
                            | (_, _) => (run_seq r1s) || (run_seq r2s))
                            $> (fn acc' => IntRedBlackMap.insertWith op+ (acc', n, 1)))
                            getc)
      end

    fun run_rule n rs extra acc = prun (finish (run_rulep n rs extra acc))
    fun accepts n rs extra acc = Option.isSome o (run_rule n rs extra acc)

    fun messagep getc =
      (PC.token (not o Char.isSpace))
      getc

    fun messagesp getc =
      (++ ((messagep +> ?? (PC.char #"\n")) $> #1))
      getc

    fun rules_messagesp getc =
      (((rulesp +> PC.char #"\n" +> messagesp))
      $> (fn (rs, (_, ms)) => (rs, ms)))
      getc

    val rules_messages = prun rules_messagesp

  end

  (* debugging *)
  fun to_re rs n =
    case IntRedBlackMap.lookup (rs, n)
      of Char c => String.str c
       | Alt (r1s, r2s) =>
           let
             val left = String.concat (List.map (to_re rs) r1s)
             val right = String.concat (List.map (to_re rs) r2s)
           in
             case (List.length r1s, List.length r2s)
               of (0, 0) => ""
                | (0, _) => right
                | (_, 0) => left
                | (_, _) => "(" ^ left ^ "|" ^ right ^ ")"
           end

  structure RE = RegExpFn(structure P = AwkSyntax
                          structure E = BackTrackEngine)

  fun re_accepts n rs =
    let val regexp = RE.compileString ("^" ^ (to_re rs n) ^ "$")
    in Option.isSome o (RE.find regexp Substring.getc) o Substring.full
    end

  fun find_differences (rs, ms) =
    let
      val by_parser = List.map (Data.accepts 0 rs (Lambda.k NONE) IntRedBlackMap.empty) ms
      val by_re = List.map (re_accepts 0 rs) ms
    in
      List.filter (fn (_, p, r) => p <> r)
      (ListPair.mapEq (fn ((i, p), r) => (i, p, r))
      (List'.with_indices by_parser, by_re))
    end

  val printMap =
    IntRedBlackMap.appi (fn (k, v) => print ((Int.toString k) ^ "," ^ (Int.toString v) ^ "\n"))
  (* end debugging *)

  fun part1' (rs, ms) = List'.count_matching (Data.accepts 0 rs (Lambda.k NONE) IntRedBlackMap.empty) ms
  val part1 = Option.map part1' o Data.rules_messages o Readers.all o Readers.file

  fun part2' (rs, ms) =
    let
      (* val rs' = IntRedBlackMap.insert (rs, 8, Alt ([42], [42, 8])) *)
      (* val rs'' = IntRedBlackMap.insert (rs', 11, Alt ([42, 31], [42, 11, 31])) *)
      (* 8: 42+
       * 11: 42^n 31^n ; n ≥ 1
       * but we can't find a parser-combinator combination that correctly parses
       * 42+ 42^n 31^n
       *
       * instead, lets parse 42+ 31+ and then check the counts afterwards *)
      local
        open Readers.ParserOps
        infixr 3 $>
        infixr 3 +>
        infixr 3 >>
        infixr 3 ||
      in
        fun extra (acc, n) =
          case n
            of 0 => SOME
                (fn getc =>
                  (((++ (Data.run_rulep 42 rs extra acc))
                    +> (++ (Data.run_rulep 31 rs extra acc)))
                  $> (fn (lefts, rights) =>
                        IntRedBlackMap.unionWith op+
                        ( List.foldl (IntRedBlackMap.unionWith op+) IntRedBlackMap.empty lefts
                        , List.foldl (IntRedBlackMap.unionWith op+) IntRedBlackMap.empty rights)))
                  getc)
            | _ => NONE
      end
      fun accepts m =
        case Data.run_rule 0 rs extra IntRedBlackMap.empty m
          of NONE => false
           | SOME acc =>
               let
                 val num42s = Option.getOpt (IntRedBlackMap.find (acc, 42), 0)
                 val num31s = Option.getOpt (IntRedBlackMap.find (acc, 31), 0)
               in
                 num42s > num31s
               end
    in
     List'.count_matching accepts ms
    end
  val part2 = Option.map part2' o Data.rules_messages o Readers.all o Readers.file

end
