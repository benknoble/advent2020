structure Solution = struct

  structure Passport = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>

    fun pp' getc =
      (++ (Readers.Dict.kvp +> (||| [Readers.PC.char #" ", Readers.PC.char #"\n"]))
      $> (Dict.fromList o (List.map #1)))
      getc
    val pp =
      (List.mapPartial (Readers.prun pp'))
      o (List.map (String.concatWith "\n"))
      o (List.foldl (fn (line, acc) =>
          if line = ""
          then []::acc
          else case acc
                 of h::t => if List.null h
                            then [line ^ "\n"]::t
                            else (line::h)::t
                  | [] => [[line ^ "\n"]]
        ) [])
      o (String.fields (Lambda.is #"\n"))
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

end
