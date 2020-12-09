structure Code = struct

  datatype instruction = Acc of int
                       | Jmp of int
                       | Nop of int
                       | HaltOp
  type code = instruction IntRedBlackMap.map (* more efficient than traversing a list *)
  type pc = IntRedBlackMap.Key.ord_key (* = int *)

  fun swap pos new code =
    IntRedBlackMap.insert
    ( code
    , pos
    , (new (IntRedBlackMap.lookup (code, pos))))

  type acc = int
  datatype process = Running of acc * pc * code
                   | Halt of acc
                   | PcErr of pc * code

  fun decode (_, pc, code) =
    if pc = IntRedBlackMap.numItems code
    then SOME HaltOp
    else IntRedBlackMap.find (code, pc)

  fun acc n (acc, pc, code) = Running (acc + n, pc + 1, code)
  fun jmp n (acc, pc, code) = Running (acc, pc + n, code)
  fun nop (acc, pc, code) = Running (acc, pc + 1, code)
  fun halt (acc, _, _) = Halt acc

  fun eval inst p =
    case inst
      of Acc n => acc n p
       | Jmp n => jmp n p
       | Nop _ => nop p
       | HaltOp => halt p

  fun step proc =
    case proc
      of Running (p as (acc, pc, code)) =>
           (case decode p
              of NONE => PcErr (pc, code)
               | SOME inst => eval inst p)
       | _ => proc

  fun load code = Running (0, 0, code)
  fun run proc =
    case proc
      of Running _ => run (step proc)
       | _ => proc
  val interpret = run o load

  fun stopped proc =
    case proc
      of Running _ => false
       | Halt _ => true
       | PcErr _ => true

  structure Reader = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun instp getc =
      ((||| [ PC.string "acc" $> Lambda.k Acc
            , PC.string "jmp" $> Lambda.k Jmp
            , PC.string "nop" $> Lambda.k Nop ]
        +> decp)
      $> (fn (inst, n) => inst n))
      getc

    fun codep getc =
      (++ (instp +> PC.char #"\n")
      $> (#2
          o (List.foldl (fn (i, (idx, code)) =>
              (idx + 1, IntRedBlackMap.insert (code, idx, i)))
            (0, IntRedBlackMap.empty))
          o (List.map #1)))
      getc

    val code = prun codep

  end

end
