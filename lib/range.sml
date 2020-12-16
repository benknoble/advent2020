structure Range = struct
  type range = {min: int, max: int}

  fun fromString c str =
    let val ints = map Int.fromString (Readers.on_char c str)
    in case ints
         of [SOME min, SOME max] => SOME {min=min, max=max}
          | _ => NONE
    end

  fun toList {min, max} = List.drop (List.tabulate (max + 1, Lambda.id), min)

  fun includes {min, max} n = min <= n andalso n <= max

  datatype bsp = Lower | Upper
  fun bsp start xs =
    let
      fun pow2 x = List.foldl (fn (_, acc) => 2 * acc) 1 (List'.rep x 1)
      val r = {min=0, max=pow2 (List.length xs) - 1}
      val bsp' = List.foldl (fn (x, {min, max}) =>
                              case x
                                of Lower => {min=min, max=min + Real.floor (real (max - min) / 2.0)}
                                 | Upper => {max=max, min=min + Real.ceil (real (max - min) / 2.0 )})
      val {min, max} = bsp' r xs
    in
      min + start
    end

end
