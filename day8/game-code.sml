structure Code = struct

  datatype instruction = Acc of int
                       | Jmp of int
                       | Nop
  type code = instruction IntRedBlackMap.map (* more efficient than traversing a list *)
  type pc = IntRedBlackMap.Key.ord_key (* = int *)

  type acc = int
  datatype process = Running of acc * pc * code
                   | PcErr of pc * code

  fun decode (_, pc, code) = IntRedBlackMap.find (code, pc)

  fun acc n (acc, pc, code) = (acc + n, pc + 1, code)
  fun jmp n (acc, pc, code) = (acc, pc + n, code)
  fun nop (acc, pc, code) = (acc, pc + 1, code)

  fun eval inst p =
    case inst
      of Acc n => acc n p
       | Jmp n => jmp n p
       | Nop => nop p

  fun step proc =
    case proc
      of Running (p as (acc, pc, code)) =>
           (case decode p
              of NONE => PcErr (pc, code)
               | SOME inst => Running (eval inst p))
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
       | PcErr _ => true

  structure Reader = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun instp getc =
      ((||| (List.map PC.string ["acc", "jmp", "nop"]) +> decp)
      $> (fn (inst, n) => case inst
                            of "acc" => Acc n
                             | "jmp" => Jmp n
                             | "nop" => Nop))
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