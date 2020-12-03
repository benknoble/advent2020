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
  fun count_matching f = List.length o (List.filter f)
end
