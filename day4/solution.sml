structure Solution = struct

  structure Passport = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun pp' getc =
      (++ (Readers.Dict.kvp +> (||| [PC.char #" ", PC.char #"\n"]))
      $> (Dict.fromList o (List.map #1)))
      getc
    val pp =
      (List.mapPartial (prun pp'))
      o (List.map (String.concatWith "\n"))
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

    fun yr getc =
      ((digitp +> digitp +> digitp +> digitp +> anything)
      >> (fn (d1, (d2, (d3, (d4, rest)))) => stop rest (mkint [d1, d2, d3, d4])))
      getc

    datatype height = CM of int | IN of int
    fun hgt getc =
      ((decp +> (||| [PC.string "cm", PC.string "in"]) +> anything)
      >> (fn (num, (kind, rest)) => stop rest (case kind
                                                 of "cm" => CM num
                                                  | "in" => IN num)))
      getc

    fun hcl getc =
      ((PC.char #"#" +> hexdigitp +> hexdigitp +> hexdigitp +> hexdigitp +> hexdigitp +> hexdigitp +> anything)
      >> (fn (_, (d1, (d2, (d3, (d4, (d5, (d6, rest))))))) => stop rest (mkhex [d1, d2, d3, d4, d5, d6])))
      getc

    fun ecl getc =
      (||| (List.map PC.string [ "amb"
                               , "blu"
                               , "brn"
                               , "gry"
                               , "grn"
                               , "hzl"
                               , "oth" ]))
      getc

    fun pid getc =
      ((digitp +> digitp +> digitp +> digitp +> digitp +> digitp +> digitp +> digitp +> digitp +> anything)
      >> (fn (d1, (d2, (d3, (d4, (d5, (d6, (d7, (d8, (d9, rest))))))))) => stop rest (mkint [d1, d2, d3, d4, d5, d6, d7, d8, d9])))
      getc

  end

  val requiredSubset = Dict.KeySet.fromList (List.map Atom.atom [ "byr"
                                                                , "iyr"
                                                                , "eyr"
                                                                , "hgt"
                                                                , "hcl"
                                                                , "ecl"
                                                                , "pid" ])

  fun isValid pp = Dict.KeySet.isSubset (requiredSubset, Dict.keys pp)

  fun part1' pps = List'.count_matching isValid pps
  val part1 = part1' o Passport.pp o Readers.all o Readers.file

  fun fail_is_false p f x =
    case Readers.ParserOps.prun p x
      of NONE => false
       | SOME s => f s

  val byrValid = ("byr", fail_is_false Passport.yr (Range.includes {min=1920, max=2002}))
  val iyrValid = ("iyr", fail_is_false Passport.yr (Range.includes {min=2010, max=2020}))
  val eyrValid = ("eyr", fail_is_false Passport.yr (Range.includes {min=2020, max=2030}))
  val hgtValid = ("hgt"
                 , fail_is_false Passport.hgt
                 ( fn x => case x
                             of Passport.CM n => Range.includes {min=150, max=193} n
                              | Passport.IN n => Range.includes {min=59, max=76} n))
  val hclValid = ("hcl", fail_is_false Passport.hcl (Lambda.k true))
  val eclValid = ("ecl", fail_is_false Passport.ecl (Lambda.k true))
  val pidValid = ("pid", fail_is_false Passport.pid (Lambda.k true))

  val checks = [ byrValid
               , iyrValid
               , eyrValid
               , hgtValid
               , hclValid
               , eclValid
               , pidValid ]

  fun run_check pp (key, validator) =
      case Dict.find (pp, Atom.atom key)
        of NONE => false
         | SOME s => validator s

  fun allValid pp =
    isValid pp
    andalso
    List.all Lambda.id (List.map (run_check pp) checks)

  val part2' = List'.count_matching allValid
  val part2 = part2' o Passport.pp o Readers.all o Readers.file

end
