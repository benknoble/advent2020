signature LIST' = sig
  val rep: int -> 'a -> 'a list
  val combs: 'a list list -> 'a list list
  val combs_of: ('a * 'b -> 'b) -> 'b -> 'a list list -> {parts: 'a list, res: 'b} list
  val sum: int list -> int
  val prod: int list -> int
  val count_matching: ('a -> bool) -> 'a list -> int
  val taking_while: ('a -> bool) -> 'a list -> 'a list list
  val with_indices: 'a list -> (int * 'a) list
  val transpose: 'a list list -> 'a list list
  val takeWhile: ('a -> bool) -> 'a list -> 'a list * 'a list
  val rotateLeft: 'a list -> 'a list
  val index_of: ''a list -> ''a -> int option
end

structure List': LIST' = struct
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

  fun takeWhile f xs =
    let
      fun loop (left, right) =
        case right
          of [] => (List.rev left, right)
           | h::t => if f h then loop (h::left, t) else (List.rev left, right)
    in
      loop ([], xs)
    end

  fun rotateLeft xs =
    if List.null xs then xs else (tl xs @ [hd xs])

  fun index_of xs x =
    (Option.map #1
    o List.find (Lambda.is x o #2)
    o with_indices)
    xs

end
