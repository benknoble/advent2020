structure Solution = struct

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
