signature DAY1 = sig
  val target: int
  val solve: int -> int list -> int
  val part1': int list -> int
  val part1: string -> int
  val part2': int list -> int
  val part2: string -> int
end

structure Solution: DAY1 = struct
  val target = 2020
  fun solve n entries =
    let
      val sums = List'.combs_of op+ 0 (List'.rep n entries)
      val matches_target =
        (* if this fails, there's an issue *)
        Option.valOf (List.find (fn {res, ...} => res = target) sums)
    in
      List.foldl op* 1 (#parts matches_target)
    end
  val part1' = solve 2
  val part1 = part1' o Readers.file_to_int_list
  val part2' = solve 3
  val part2 = part2' o Readers.file_to_int_list
end
