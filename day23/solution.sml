structure Solution = struct

  (* length, left, current, right
   * such that List.length (length @ [current] @ right) = length *)
  type cups = int * int list * int * int list

  fun cup_init xs: cups = (List.length xs, [], hd xs, tl xs)

  structure Cups = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun cupsp getc =
      ((++ (digitp $> (Option.valOf o Int.fromString o String.str)))
      $> cup_init)
      getc

    val cups = prun cupsp
  end

  fun take3 (left, current, right) =
    let val right_length = List.length right
    in
      if right_length >= 3
      then (List.take (right, 3), left, List.drop (right, 3))
      else
        let
          val from_left = List.take (left, 3 - right_length)
          val left' = List.drop (left, 3 - right_length)
        in
          (right @ from_left, left', [])
        end
    end

  fun dest (n, current, skip) =
    let
      fun target offset =
        let val target' = current - offset
        in if target' = 0 then n else target' mod n
        end
      fun contains_candidate c = List.exists (Lambda.is c) skip
    in
      (Option.valOf
      o List.find (not o contains_candidate)
      o List.map target)
      [1, 2, 3, 4]
    end

  fun place dest picked_up (left, right) =
    if List.exists (Lambda.is dest) left
    then
      let
        val (left_before, _::left_after) = List'.takeWhile (not o (Lambda.is dest)) left
      in
        (left_before @ (dest::picked_up) @ left_after, right)
      end
    else (* assume right *)
      let
        val (right_before, _::right_after) = List'.takeWhile (not o (Lambda.is dest)) right
      in
        (left, right_before @ (dest::picked_up) @ right_after)
      end

  fun rebalance (n, left, current) =
    let
      val right = List.take (left, n div 2)
      val left' = List.drop (left, n div 2)
    in
      (n, left', current, right)
    end

  fun move (n, left, current, right) =
    let
      val (picked_up, left', right') = take3 (left, current, right)
      val dest = dest (n, current, picked_up)
      val (left'', right'') = place dest picked_up (left', right')
      val left''' = left'' @ [current]
    in
      case right''
        of [] => rebalance (n, tl left''', hd left'')
         | [h] => rebalance (n, left''', h)
         | h::t => (n, left''', h, t)
    end

  fun moveN n cups =
    if n = 0 then cups
    else moveN (n-1) (move cups)

  fun toList (_, left, current, right) = left @ [current] @ right

  fun move_one_to_front cups =
    let
      val cups' = toList cups
      fun loop cups =
        if hd cups = 1
        then cups
        else loop (List'.rotateLeft cups)
    in
      loop cups'
    end

  fun part1' cups =
    let
      val cups' = moveN 100 cups
      val ans = tl (move_one_to_front cups')
    in
      String.concat (map Int.toString ans)
    end

  val part1 = Option.map part1' o Cups.cups o Readers.all o Readers.file

end
