signature DAY23 = sig
  type cups

  val %= : 'a Array.array * (int * 'a) -> unit
  val % : 'a Array.array * int -> 'a

  val mk_circular: int list -> int Array.array
  val mk_extended: int -> int list -> int Array.array

  val cup_init: int option -> int list -> cups

  structure Cups: sig
    val cupsp: int option -> (cups, 'strm) ParserComb.parser
    val cups: int option -> string -> cups option
  end

  val take3: cups -> int * int * int
  val dest: int * int * int -> int -> int -> int
  val place: int -> int * int * int -> cups -> unit
  val move: cups -> cups
  val moveN: int -> cups -> cups

  val cupsToList: cups -> int list
  val move_one_to_front: int list -> int list

  val part1': cups -> string
  val part1: string -> string option

  val part2': cups -> int
  val part2: string -> int option
end

structure Solution: DAY23 = struct

  (* cup -> next cup
   * current cup
   * Vectors are too slow, so we'll use arrays *)
  type cups = int Array.array * int

  fun %= (a, (i, x)) = Array.update (a, i, x)
  infix 3 %=

  fun % (a, i) = Array.sub (a, i)
  infix 3 %

  fun mk_circular xs =
    let
      val current = hd xs
      val length = List.length xs
    in
      Array.tabulate (length + 1, fn i =>
        if i = 0
        then current
        else (* 0 <= i <= length *)
          let val ii = Option.valOf (List'.index_of xs i)
          in List.nth (xs, (ii + 1) mod length)
          end)
    end

  fun mk_extended n xs =
    let
      val current = hd xs
      val length = List.length xs
    in
      Array.tabulate (n+1, fn i =>
        if i = 0 orelse i = n
        then current
        else if i <= length
        then
          let val ii = Option.valOf (List'.index_of xs i)
          in
            if ii < length - 1
            then List.nth (xs, ii + 1)
            else length + 1
          end
        else i + 1)
    end

  fun cup_init n xs: cups =
    let val current = hd xs
    in case n
         of NONE => (mk_circular xs, current)
          | SOME n' => (mk_extended n' xs, current)
    end

  structure Cups = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun cupsp n getc =
      ((++ (digitp $> (Option.valOf o Int.fromString o String.str)))
      $> (cup_init n))
      getc

    fun cups n = prun (cupsp n)
  end

  fun take3 (cups, current) =
    let
      val one = cups % current
      val two = cups % one
      val three = cups % two
    in
      (one, two, three)
    end

  fun dest (one, two, three) n current =
    let
      fun prev candidate =
        if candidate = 1
        then n
        else candidate - 1
      fun loop candidate =
        if one = candidate orelse two = candidate orelse three = candidate
        then loop (prev candidate)
        else candidate
    in
      loop (prev current)
    end

  fun place dest (one, _, three) (cups, current) =
    let
      val four = cups % three
      val next = cups % dest
    in
      cups %= (current, four);
      cups %= (three, next);
      cups %= (dest, one)
    end

  fun move (cups, current) =
    let
      val taken = take3 (cups, current)
      val dest = dest taken (Array.length cups - 1) current
    in
      place dest taken (cups, current);
      (cups, cups % current)
    end

  fun moveN n cups =
    if n = 0 then cups
    else moveN (n-1) (move cups)

  fun cupsToList (cups, current) =
    let
      fun loop (prev, acc) =
        if prev = current andalso not (List.null acc)
        then List.rev acc
        else loop ((cups % prev), prev::acc)
    in
      loop (current, [])
    end

  fun move_one_to_front cups =
    let
      fun loop cups =
        if hd cups = 1
        then cups
        else loop (List'.rotateLeft cups)
    in
      loop cups
    end

  fun part1' cups =
    let
      val cups' = moveN 100 cups
      val ans = tl (move_one_to_front (cupsToList cups'))
    in
      String.concat (List.map Int.toString ans)
    end

  val part1 = Option.map part1' o Cups.cups NONE o Readers.all o Readers.file

  fun part2' cups =
    let
      val n = 1000000
      val cups' = cup_init (SOME n) (cupsToList cups)
      val (cups'', _) = moveN 10000000 cups'
      val first = cups'' % 1
      val second = cups'' % first
    in
      first * second
    end

  val part2 = Option.map part2' o Cups.cups NONE o Readers.all o Readers.file

end
