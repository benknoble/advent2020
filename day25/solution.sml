signature DAY25 = sig
  val modulus: int
  val base: int

  val *% : int * int -> int

  val search_loop_size: int -> int
  val transform: int -> int -> int
  val private_key: int * int -> int

  structure PubKeys: sig
    val pubkeyp: (int, 'strm) ParserComb.parser
    val pubkeysp: (int * int, 'strm) ParserComb.parser
    val pubkeys: string -> (int * int) option
  end

  val part1': int * int -> int
  val part1: string -> int option
end

structure Solution: DAY25 = struct
  val modulus = 20201227
  val base = 7

  infix *%
  fun x *% y = (x * y) mod modulus

  fun search_loop_size target =
    let
      fun loop n value =
        if value = target
        then n
        else loop (n + 1) (value *% base)
    in
      loop 0 1
    end

  fun transform subject n =
    let
      fun loop n value  =
        if n = 0 then value else loop (n - 1) (value *% subject)
    in
      loop n 1
    end

  fun private_key (card_pub_key, door_pub_key) =
    let
      val card_loop_size = search_loop_size card_pub_key
      val door_loop_size = search_loop_size door_pub_key
      val card_priv_key = transform door_pub_key card_loop_size
      val door_priv_key = transform card_pub_key door_loop_size
    in
      if card_priv_key = door_priv_key
      then card_priv_key
      else raise Fail ("keys not equal "
                      ^ (Int.toString card_priv_key)
                      ^ " "
                      ^ (Int.toString door_priv_key))
    end

  structure PubKeys = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun pubkeyp getc = ((decp +> PC.char #"\n") $> #1) getc
    fun pubkeysp getc = (pubkeyp +> pubkeyp) getc
    val pubkeys = prun pubkeysp
  end

  val part1' = private_key
  val part1 = Option.map part1' o PubKeys.pubkeys o Readers.all o Readers.file

end
