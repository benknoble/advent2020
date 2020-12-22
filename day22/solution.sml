structure Solution = struct

  type deck = int Fifo.fifo
  datatype winner = Player1 | Player2
  datatype game = Game of deck * deck
                | Over of deck * winner

  fun mk_deck xs =
    List.foldl (fn (x, acc) => Fifo.enqueue (acc, x)) Fifo.empty xs

  structure Decks = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun deckp getc =
      ((++ ((decp +> ?? (PC.char #"\n")) $> #1))
      $> mk_deck)
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
    Game (Fifo.enqueue (Fifo.enqueue (p1, p1_card), p2_card),  p2)

  fun p2wins (p1_card, p1) (p2_card, p2) =
    Game (p1, Fifo.enqueue (Fifo.enqueue (p2, p2_card), p1_card))

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
           (case (Fifo.next p1, Fifo.next p2)
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
           o List.rev
           o Fifo.contents)
           wdeck
       | Game _ => raise Fail "game not over"

  val part1' = score o play
  val part1 = Option.map part1' o Decks.game o Readers.all o Readers.file

end
