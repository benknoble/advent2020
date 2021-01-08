signature DAY14 = sig
  type value
  type mem
  datatype mask_bit = X | One | Zero
  type pos_mask
  type mask

  val from_bin: int list -> IntInf.int
  val onem: pos_mask -> IntInf.int
  val zerom: pos_mask -> IntInf.int

  val mask_value: value -> mask -> value
  val mask_address: IntInf.int -> mask -> IntInf.int list

  datatype instruction = Mask of mask
                       | MemWrite of int * value

  type program
  type process

  val init_mem: mem
  val init_mask: mask

  type maskf = mask -> mem -> int -> value -> mask * mem
  val maskf1: maskf
  val maskf2: maskf

  val step: maskf -> instruction * (mask * mem) -> (mask * mem)
  val load: program -> process
  val run: maskf -> process -> (mask * mem)

  structure Docking: sig
    val memwritep: (instruction, 'strm) ParserComb.parser
    val maskp: (instruction, 'strm) ParserComb.parser
    val progp: (program, 'strm) ParserComb.parser
    val prog: string -> program option
  end

  val part1': program -> IntInf.int
  val part1: string -> IntInf.int option

  val part2': program -> IntInf.int
  val part2: string -> IntInf.int option
end

structure Solution: DAY14 = struct

  type value = IntInf.int
  type mem = value IntRedBlackMap.map
  datatype mask_bit = X | One | Zero
  type pos_mask = (int * mask_bit) list
  type mask = {zeros: pos_mask, ones: pos_mask, xs: pos_mask}

  val from_bin = List.foldl (fn (x, acc) => IntInf.pow (2, x) + acc) 0
  val onem: pos_mask -> IntInf.int =
    from_bin o (List.map #1)
  val zerom: pos_mask -> IntInf.int  =
    IntInf.notb o from_bin o (List.map #1)

  fun mask_value value ({zeros, ones, ...}: mask) =
    let
      val onesm = onem ones
      val zerosm = zerom zeros
    in
      IntInf.andb (zerosm,
        IntInf.orb (onesm, value))
    end

  fun mask_address addr ({ones, xs, ...}: mask) =
    let
      val onesm = onem ones
      val addr' = IntInf.orb (onesm, addr)
      (* doubles a single addr into a list of two *)
      fun mask_float pos addr =
        [ IntInf.orb (onem [pos], addr) (* set a one *)
        , IntInf.andb (zerom [pos], addr) ] (* set a zero *)
    in
      List.foldl
      (* List.concat (List.map f vs) is the idiom for flat-map
       * since mask_float doubles a value, mapping it over a list creates
       * sublists of length 2
       * flat-mapping it turns a list of length n into a list of length 2^n
       * starting from the first addr' we compute all 2^n (n = List.length xs)
       * possible addresses from the floating bits *)
      (fn (x, cur) => List.concat (List.map (mask_float x) cur))
      [addr']
      xs
    end

  datatype instruction = Mask of mask
                       | MemWrite of int * value
  type program = instruction list
  type process = mask * mem * program

  val init_mem: mem = IntRedBlackMap.empty
  val init_mask: mask = {zeros=[], ones=[], xs=[]}

  type maskf = mask -> mem -> int -> value -> mask * mem

  fun maskf1 mask mem k v =
    (mask, IntRedBlackMap.insert (mem, k, mask_value v mask))

  fun maskf2 mask mem k v =
    let val ks = mask_address (IntInf.fromInt k) mask
    in
      ( mask
      , List.foldl (fn (k, mem) => IntRedBlackMap.insert (mem, IntInf.toInt k, v)) mem ks)
    end

  fun step maskf (inst, (mask, mem)) =
    case inst
      of Mask mask' => (mask', mem)
       | MemWrite (k, v) => maskf mask mem k v

  fun load prog: process = (init_mask, init_mem, prog)
  fun run maskf (mask, mem, program) = List.foldl (step maskf) (mask, mem) program

  structure Docking = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun memwritep getc =
      ((PC.string "mem[" +> decp +> PC.string "] = " +> decp +> PC.char #"\n")
      $> (fn (_, (k, (_, (v, _)))) => MemWrite (k, IntInf.fromInt v)))
      getc

    fun maskp getc =
      ((PC.string "mask = "
        +> (++ (||| [ PC.char #"X" $> Lambda.k X
                    , PC.char #"1" $> Lambda.k One
                    , PC.char #"0" $> Lambda.k Zero ]))
        +> PC.char #"\n")
      $> (Mask
          o (fn pms =>
              let
                val (zeros, rest) = List.partition (Lambda.is Zero o #2) pms
                val (ones, xs) = List.partition (Lambda.is One o #2) rest
              in
                {zeros=zeros, ones=ones, xs=xs}
              end)
          o List'.with_indices
          o List.rev
          o #1
          o #2))
      getc

    fun progp getc = (++ (memwritep || maskp)) getc

    val prog = prun progp

  end

  val part1' =
    (IntRedBlackMap.foldl op+ 0)
    o #2
    o (run maskf1)
    o load
  val part1 = (Option.map part1') o Docking.prog o Readers.all o Readers.file

  val part2' =
    (IntRedBlackMap.foldl op+ 0)
    o #2
    o (run maskf2)
    o load
  val part2 = (Option.map part2') o Docking.prog o Readers.all o Readers.file

end
