structure Solution = struct

  type deck = int list
  datatype winner = Player1 | Player2
  datatype game = Game of deck * deck
                | Over of deck * winner

  structure Decks = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun deckp getc =
      (++ ((decp +> ?? (PC.char #"\n")) $> #1))
      getc

    fun gamep getc =
      ((PC.string "Player 1:\n"
        +> deckp
        +> PC.char #"\n"
        +> PC.string "Player 2:\n"
        +> deckp)
      $> (fn (_, (p1, (_, (_, p2)))) => Game (p1, p2)))
      getc

    val game = prun gamep
  end

  fun p1wins (p1_card, p1) (p2_card, p2) =
    Game (p1 @ [p1_card, p2_card],  p2)

  fun p2wins (p1_card, p1) (p2_card, p2) =
    Game (p1, p2 @ [p2_card, p1_card])

  fun round_winner (p1_card, _) (p2_card, _) =
    case Int.compare (p1_card, p2_card)
      of LESS => Player2
       | GREATER => Player1
       | EQUAL => raise Fail "no rules for ties"

  fun round_next p1 p2 =
    case round_winner p1 p2
      of Player1 => p1wins p1 p2
       | Player2 => p2wins p1 p2

  fun step game =
    case game
      of Game (p1, p2) =>
           (case (List.getItem p1, List.getItem p2)
             of (SOME _, NONE) => Over (p1, Player1)
              | (NONE, SOME _) => Over (p2, Player2)
              | (NONE, NONE) => raise Fail "no one has any cards"
              | (SOME p1', SOME p2') => round_next p1' p2')
       | Over _ => game

  fun play game =
    case game
      of Over _ => game
       | Game _ => play (step game)

  fun score game =
    case game
      of Over (wdeck, _) =>
           (List'.sum
           o List.map (fn (i,c) => (i + 1) * c)
           o List'.with_indices
           o List.rev)
           wdeck
       | Game _ => raise Fail "game not over"

  val part1' = score o play
  val part1 = Option.map part1' o Decks.game o Readers.all o Readers.file

  fun deck_compare (d1, d2) =
    List.collate Int.compare (d1, d2)

  structure GameMap = RedBlackMapFn(struct
    type ord_key = game
    fun compare (g1, g2) =
      case (g1, g2)
        of (Game _, Over _) => GREATER
         | (Over _, Game _) => LESS
         | (Over (d1, w1), Over (d2, w2)) =>
             (case deck_compare (d1, d2)
                of EQUAL =>
                     (case (w1, w2)
                        of (Player1, Player1) => EQUAL
                         | (Player2, Player2) => EQUAL
                         | (Player1, Player2) => LESS
                         | (Player2, Player1) => GREATER)
                 | x => x)
         | (Game (p11, p12), Game (p21, p22)) =>
             (case deck_compare (p11, p21)
                of EQUAL => deck_compare (p12, p22)
                 | x => x)
  end)
  structure GameMap = WithMapUtilsFn(structure M = GameMap)
  structure GameSet = GameMap.KeySet

  fun deck_take_n (n, deck) = List.take (deck, n)

  fun can_recurse (p1_card, p1) (p2_card, p2) =
    List.length p1 >= p1_card
    andalso
    List.length p2 >= p2_card

  fun stepR prev game =
    case game
      of Game (p1, p2) =>
           if GameSet.member (prev, game)
           then Over (p1, Player1)
           else
             (case (List.getItem p1, List.getItem p2)
                   of (SOME _, NONE) => Over (p1, Player1)
                    | (NONE, SOME _) => Over (p2, Player2)
                    | (NONE, NONE) => raise Fail "no one has any cards"
                    | (SOME p1', SOME p2') =>
                        if can_recurse p1' p2'
                        then
                          let
                            val p1'' = deck_take_n p1'
                            val p2'' = deck_take_n p2'
                            val game' = Game (p1'', p2'')
                          in
                            case playR GameSet.empty game'
                              of Over (_, Player1) => p1wins p1' p2'
                               | Over (_, Player2) => p2wins p1' p2'
                               | Game _ => raise Fail "game not over"
                          end
                        else round_next p1' p2')
       | Over _ => game
  and playR prev game =
    case game
      of Over _ => game
       | Game _ => playR (GameSet.add (prev, game)) (stepR prev game)

  val part2' = score o (playR GameSet.empty)
  val part2 = Option.map part2' o Decks.game o Readers.all o Readers.file

end
