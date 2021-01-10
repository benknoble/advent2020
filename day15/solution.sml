signature DAY15 = sig
  structure Seed: sig
    val seedp: (int list, 'strm) ParserComb.parser
    val seed: string -> int list option
  end

  val mem_game_step: int * (int * int IntRedBlackMap.map) -> int * int IntRedBlackMap.map
  val mem_game: int list -> int -> int * int IntRedBlackMap.map

  val part1': int list -> int * int IntRedBlackMap.map
  val part1: string -> (int * int IntRedBlackMap.map) option

  val part2': int list -> int * int IntRedBlackMap.map
  val part2: string -> (int * int IntRedBlackMap.map) option
end

structure Solution: DAY15 = struct

  structure Seed = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun seedp getc =
      ((decp +> (++ (PC.char #"," +> decp) $> List.map #2))
      $> op::)
      getc

    val seed = prun seedp
  end

  fun mem_game_step (turn, (prev, seen)) =
    let
      val prev_turn = turn - 1
      val next = case IntRedBlackMap.find (seen, prev)
                   of NONE => 0
                    | SOME s => prev_turn - s
    in
      (next, IntRedBlackMap.insert (seen, prev, prev_turn))
    end

  fun mem_game seed n =
    let
      val seen =
        List.foldl
        (fn ((i, s), acc) => IntRedBlackMap.insert (acc, s, i+1))
        IntRedBlackMap.empty
        (List'.with_indices seed)
      val most_recent = List.last seed
      val n_start = List.length seed + 1
      fun loop turn curr =
        if turn = n
        then mem_game_step (turn, curr)
        else loop (turn+1) (mem_game_step (turn, curr))
    in
      loop n_start (most_recent, seen)
    end

  fun part1' seed = mem_game seed 2020
  val part1 = (Option.map part1') o Seed.seed o Readers.all o Readers.file

  fun part2' seed = mem_game seed 30000000
  val part2 = (Option.map part2') o Seed.seed o Readers.all o Readers.file
end
