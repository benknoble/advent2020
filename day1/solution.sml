structure Solution = struct
  val target = 2020
  val part1 =
    let
      fun part1' entries =
        let
          val sums = ListXProd.map (fn (a,b) => {a=a, b=b, sum=a+b}) (entries, entries)
          val matches_target =
            (* if this fails, there's an issue *)
            Option.valOf (List.find (fn {sum, ...} => sum = target) sums)
        in
          (#a matches_target) * (#b matches_target)
        end
    in
      part1' o Readers.file_to_int_list
    end
end
