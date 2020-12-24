signature DAY2 = sig
  type rule = {range: Range.range, letter: char, password: string}
  val read_password_rules': string -> rule list
  val read_password_rules: string -> rule list

  val valid1: rule -> bool
  val part1': rule list -> int
  val part1: string -> int

  val valid2: rule -> bool
  val part2': rule list -> int
  val part2: string -> int
end

structure Solution: DAY2 = struct

  type rule = {range: Range.range, letter: char, password: string}

  val read_password_rules' =
    (map (fn [range, letter, pwd] =>
      { range = Option.valOf (Range.fromString #"-" range)
      , letter = Option.valOf (Char.fromString letter)
      , password = pwd}))
    o (map (Readers.on_chars [#":", #" "]))
    o Readers.lines

  val read_password_rules = read_password_rules' o Readers.all o Readers.file

  fun valid1 {range, letter, password} =
    ((Range.includes range)
    o (List'.count_matching (Lambda.is letter))
    o String.explode)
    password

  val part1' = List'.count_matching valid1
  val part1 = part1' o read_password_rules

  fun valid2 {range={min, max}, letter, password} =
    let
      val password = String.explode password
      val minth = List.nth (password, min-1)
      val maxth = List.nth (password, max-1)
    in
      (minth = letter andalso maxth <> letter)
      orelse
      (minth <> letter andalso maxth = letter)
    end

  val part2' = List'.count_matching valid2
  val part2 = part2' o read_password_rules

end
