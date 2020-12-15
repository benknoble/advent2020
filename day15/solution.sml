structure Solution = struct

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

  fun mem_game_step (turn, (played, prev, seen)) =
    let
      val prev_turn = turn - 1
      val next = case IntRedBlackMap.find (seen, prev)
                   of NONE => 0
                    | SOME s => prev_turn - s
    in
      (next::played, next, IntRedBlackMap.insert (seen, prev, prev_turn))
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
      val ns = Range.toList {min=n_start, max=n}
    in
      List.foldl mem_game_step (List.rev seed, most_recent, seen) ns
    end

  fun part1' seed = mem_game seed 2020
  val part1 = (Option.map part1') o Seed.seed o Readers.all o Readers.file

end
