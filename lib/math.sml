structure Math' = struct

  fun extended_euclidean (a, b) =
    let
      fun loop (r, s, t) (r', s', t') =
        if r = 0
        then
          (r', s', t', s, t)
        else
          let val q = r' div r
          in loop (r' - q * r, s' - q * s, t' - q * t) (r, s, t)
          end
    in
      loop (b, 0, 1) (a, 1, 0)
    end

  fun modular_inverse (a, b) =
    let val (_, s, _, _, _) = extended_euclidean (a, b)
    in
      if s >= 0
      then s
      else s + b
    end

  fun crt (congruences: (int * int) list) =
    let
      val ms = List.map #2 congruences
      val ais = List.map #1 congruences
      val m = List'.prod ms
      val bs = List.map (fn m_i => m div m_i) ms
      val bs' = ListPair.map modular_inverse (bs, ms)
      val bbs' = ListPair.map op* (bs, bs')
      val abbs' = ListPair.map op* (ais, bbs')
    in
      (List'.sum abbs') mod m
    end

end
