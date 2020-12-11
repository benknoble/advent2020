structure Solution = struct

  type adapter = int
  type chain = adapter list
  val start = 0

  val mk_chain = ListMergeSort.sort op>
  fun mk_seat_chain adapters =
    if List.null adapters
    then []
    else
      let
        val chain = mk_chain adapters
        val seat = (List.last chain) + 3
      in
        start::chain @ [seat]
      end

  datatype difference = One | Two | Three | Other

  fun add_d ds =
    case ds
      of (One, One) => Two
       | (One, Two) => Three
       | (Two, One) => Three
       | _ => Other
  infix 6 add_d

  fun differences chain =
    case chain
      of [] => []
       | [_] => []
       | _::t =>
           ListPair.map
           (fn (prev, cur) => case cur - prev
                                of 1 => One
                                 | 2 => Two
                                 | 3 => Three
                                 | _ => Other)
           (chain, t)

  val valid_differences = List.all (not o (Lambda.is Other))
  val valid_chain = valid_differences o differences

  val count_differences =
    (List.foldl
      (fn (d, (ones, twos, threes)) => case d
                                         of One => (ones+1, twos, threes)
                                          | Two => (ones, twos+1, threes)
                                          | Three => (ones, twos, threes+1)
                                          | Other => (ones, twos, threes))
      (0, 0, 0))
    o differences

  (* assumes valid chain *)
  fun part1' chain =
    let val (ones, _, threes) = count_differences chain
    in ones * threes
    end
  val part1 =
    (Option.map part1')
    o (Option.filter valid_chain)
    o mk_seat_chain
    o Readers.file_to_int_list

  (* memoized tribonacci *)
  local
    val fromList = List.foldl IntRedBlackMap.insert' IntRedBlackMap.empty
    val memo = ref (fromList [(0, 0), (1, 1), (2, 1), (3, 2), (4, 4), (5, 7)])
  in
    fun trib n =
      case IntRedBlackMap.find (!memo, n)
        of NONE =>
             let val result = trib (n-1) + trib (n-2) + trib (n-3)
             in memo := IntRedBlackMap.insert (!memo, n, result); result
             end
         | SOME t => t
  end

  fun valid_combs' ds = trib (List.length ds + 1)

  val valid_combs =
    List'.prod
    o (List.map valid_combs')
    o (List'.taking_while (not o (Lambda.is Three)))
    o differences

  val part2' = valid_combs
  val part2 =
    (Option.map part2')
    o (Option.filter valid_chain)
    o mk_seat_chain
    o Readers.file_to_int_list

end
