structure Range = struct
  fun fromString c str =
    let val ints = map Int.fromString (Readers.on_char c str)
    in case ints
         of [SOME min, SOME max] => SOME {min=min, max=max}
          | _ => NONE
    end

  fun toList {min, max} = List.drop (List.tabulate (max + 1, Lambda.id), min)

  fun includes {min, max} n = min <= n andalso n <= max
end
