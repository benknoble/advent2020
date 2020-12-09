structure Solution = struct
  (* #1 = queue holding window contents
   *      from it we calculate the "head" of the window to remove by dequeueing,
   *      and the new window by enqueueing #3
   * #2 = set of window contents (for fast membership checking)
   * #3 = current value to validate *)
  type window = int Fifo.fifo * IntRedBlackSet.set * int
  type data = int list
  type decode_data = window * data

  fun validw (_, set, curr) =
    IntRedBlackSet.exists
    (fn i => (IntRedBlackSet.member (IntRedBlackSet.delete (set, i), curr - i)))
    set

  fun next ((fifo, set, curr), data) =
    Option.map (fn (hd, tl) =>
      let
        val (fifo'', prev) = Fifo.dequeue fifo
        val set' = IntRedBlackSet.add ((IntRedBlackSet.delete (set, prev)), curr)
        val fifo' = Fifo.enqueue (fifo'', curr)
      in ((fifo', set', hd), tl)
      end)
    (List.getItem data)

  datatype valid = Valid | Invalid of int

  fun valid (w, d) =
    if validw w
    then case next (w, d)
           of NONE => Valid
            | SOME dd' => valid dd'
    else Invalid (#3 w)

  fun init data =
    let
      val first25 = List.take (data, 25)
      val curr::rest = List.drop (data, 25)
    in
      ( ( List.foldl (fn (a, q) => Fifo.enqueue (q, a)) Fifo.empty first25
        , IntRedBlackSet.fromList first25
        , curr)
      , rest)
    end

  val part1' = valid o init
  val part1 = part1' o Readers.file_to_int_list
end
