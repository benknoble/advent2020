structure Solution = struct

  val read_password_rules' =
    (map (fn [range, letter, pwd] =>
      { range = Option.valOf (Range.fromString #"-" range)
      , letter = Option.valOf (Char.fromString letter)
      , password = pwd}))
    o (map (Readers.on_chars [#":", #" "]))
    o Readers.lines

  val read_password_rules = read_password_rules' o Readers.all o Readers.file

  fun valid {range, letter, password} =
    ((Range.includes range)
    o (List'.count_matching (fn c' => c' = letter))
    o String.explode)
    password

  val part1' = List'.count_matching valid
  val part1 = part1' o read_password_rules

end
