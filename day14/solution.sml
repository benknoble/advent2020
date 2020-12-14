structure Solution = struct

  type value = IntInf.int
  type mem = value IntRedBlackMap.map
  type mask = IntInf.int * IntInf.int

  fun apply_mask value (zeros, ones) =
    IntInf.andb (IntInf.orb (value, ones), zeros)

  datatype instruction = Mask of mask
                       | MemWrite of int * value
  type program = instruction list
  type process = mask * mem * program

  val init_mem: mem = IntRedBlackMap.empty

  fun step (inst, (mask, mem)) =
    case inst
      of Mask mask' => (mask', mem)
       | MemWrite (k, v) =>
           (mask, IntRedBlackMap.insert (mem, k, apply_mask v mask))

  fun load prog: process = ((0, 0), init_mem, prog)
  fun run (mask, mem, program) = List.foldl step (mask, mem) program

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

    datatype mask_bit = X | One | Zero

    val from_bin = List.foldl (fn (x, acc) => IntInf.pow (2, x) + acc) 0

    fun maskp getc =
      ((PC.string "mask = "
        +> (++ (||| [ PC.char #"X" $> Lambda.k X
                    , PC.char #"1" $> Lambda.k One
                    , PC.char #"0" $> Lambda.k Zero ]))
        +> PC.char #"\n")
      $> (Mask
          o (fn (zeros, ones) =>
              ( IntInf.notb (from_bin (List.map #1 zeros))
              , from_bin (List.map #1 ones)))
          o List.partition ((Lambda.is Zero) o #2)
          o List.filter (not o (Lambda.is X) o #2)
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
    o run
    o load
  val part1 = (Option.map part1') o Docking.prog o Readers.all o Readers.file

end
