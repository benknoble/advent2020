structure List' = struct
  fun rep n x = List.tabulate (n, Lambda.k x)

  fun combs xss =
    case xss
      of [] => []
       | [xs] => [xs]
       | xs::ys::tail =>
           List.foldl
           (ListXProd.map (fn (a, b) => a::b))
           (ListXProd.map (fn (a, b) => [a,b]) (xs, ys))
           tail

  fun combs_of comb z xss =
    List.map
    (fn parts => {parts=parts, res=(List.foldl comb z parts)})
    (combs xss)

end
