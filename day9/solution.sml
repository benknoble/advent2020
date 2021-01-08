signature DAY9 = sig
  type window
  type data = int list
  type decode_data

  val validw: window -> bool
  val next: decode_data -> decode_data option

  datatype valid = Valid | Invalid of int
  val valid: decode_data -> valid
  val init: data -> decode_data

  val part1': data -> valid
  val part1: string -> valid

  val css: int list -> int -> int list option

  val part2': data -> int option
  val part2: string -> int option
end

structure Solution: DAY9 = struct
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

  fun css data target =
    let
      fun css' xs i sum partials =
        Option.mapPartial (fn (hd, tl) =>
          let
            val sum' = sum + hd
            val i' = i + 1
          in
            case IntRedBlackMap.find (partials, sum' - target)
              of NONE => css' tl i' sum' (IntRedBlackMap.insert (partials, sum', i))
               | SOME j => SOME (List.take (List.drop (data, j + 1), i - j))
          end)
        (List.getItem xs)
    in
      css' data 0 0 (IntRedBlackMap.insert (IntRedBlackMap.empty, 0, ~1))
    end

  fun part2' data =
    let
      val invalid = case part1' data
                      of Valid => NONE
                       | Invalid invalid => SOME invalid
      val subseq = Option.mapPartial (css data) invalid
      val min = Option.mapPartial (Option.map (fn (hd, tl) => List.foldl Int.min hd tl) o List.getItem) subseq
      val max = Option.mapPartial (Option.map (fn (hd, tl) => List.foldl Int.max hd tl) o List.getItem) subseq
    in
      Option.mapPartial (fn min => Option.map (fn max => min + max) max) min
    end

  val part2 = part2' o Readers.file_to_int_list
end
