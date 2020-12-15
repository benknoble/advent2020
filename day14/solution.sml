structure Solution = struct

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

  datatype instruction = Mask of mask
                       | MemWrite of int * value
  type program = instruction list
  type process = mask * mem * program

  val init_mem: mem = IntRedBlackMap.empty
  val init_mask: mask = {zeros=[], ones=[], xs=[]}

  fun maskf1 mask mem k v =
    (mask, IntRedBlackMap.insert (mem, k, mask_value v mask))

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

end
