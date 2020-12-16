structure List' = struct
  fun rep n x = List.tabulate (n, Lambda.k x)

  fun combs xss =
    case xss
      of [] => []
       | [xs] => [xs]
       | xs::ys::tail =>
           List.foldl
           (ListXProd.map op::)
           (ListXProd.map (fn (a, b) => [a,b]) (xs, ys))
           tail

  fun combs_of comb z xss =
    List.map
    (fn parts => {parts=parts, res=(List.foldl comb z parts)})
    (combs xss)

  val sum = List.foldl op+ 0
  val prod = List.foldl op* 1
  fun count_matching f = List.length o (List.filter f)

  fun taking_while f =
    List.rev
    o (fn (chain, acc) => (List.rev chain)::acc)
    o (List.foldl
      (fn (x, (chain, acc)) => if f x
                               then (x::chain, acc)
                               else ([], (List.rev chain)::acc))
      ([], []))

  fun with_indices xs =
    ListPair.zip (List.tabulate (List.length xs, Lambda.id), xs)

  fun transpose xss =
    case xss
      of [] => []
       | []::_ => [] (* assumes they are all empty at this point *)
       | _ => (List.map List.hd xss) :: (transpose (List.map List.tl xss))

end
