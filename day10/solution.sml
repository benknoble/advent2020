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

  val valid_chain = (List.all (not o (Lambda.is Other))) o differences

  val count_differences =
    (List.foldl
      (fn (d, (ones, twos, threes)) => case d
                                         of One => (ones+1, twos, threes)
                                          | Two => (ones, twos+1, threes)
                                          | Three => (ones, twos, threes+1)
                                          | Other => (ones, twos, threes))
      (0, 0, 0))
    o differences

  fun part1' chain =
    let val (ones, _, threes) = count_differences chain
    in ones * threes
    end
  val part1 =
    (Option.map part1')
    o (Option.filter valid_chain)
    o mk_seat_chain
    o Readers.file_to_int_list

end
