signature DAY8 = sig
  val break: Code.process -> Code.acc option
  val part1': Code.process -> Code.acc option
  val part1: string -> Code.acc option

  datatype result = InfiniteLoop | Halt of Code.acc
  val run: Code.process -> result option

  val tryswaps: Code.code -> Code.pc -> Code.acc option
  val check: Code.code -> Code.pc -> Code.instruction -> Code.acc option

  val part2': Code.code -> Code.acc option
  val part2: string -> Code.acc option
end

structure Solution: DAY8 = struct
  fun break proc =
    let
      fun break' proc (visited, prev_acc) =
        case Code.step proc
          of proc' as (Code.Running (acc, pc, _)) =>
               if IntRedBlackSet.member (visited, pc)
               then prev_acc
               else break' proc' ((IntRedBlackSet.add (visited, pc)), SOME acc)
           | _ => NONE
    in
      break' proc (IntRedBlackSet.empty, NONE)
    end

  fun part1' proc = break proc

  val part1 = part1' o Code.load o Option.valOf o Code.Reader.code o Readers.all o Readers.file


  datatype result = InfiniteLoop | Halt of Code.acc
  fun run proc =
    let
      fun run' proc visited =
        case Code.step proc
          of proc' as (Code.Running (acc, pc, _)) =>
               if IntRedBlackSet.member (visited, pc)
               then SOME InfiniteLoop
               else run' proc' (IntRedBlackSet.add (visited, pc))
           | Code.Halt acc => SOME (Halt acc)
           | _ => NONE
    in
      run' proc IntRedBlackSet.empty
    end


  fun tryswaps code pos =
    (* 0 is a placeholder; doesn't matter what the accumulator is for
     * decode *)
    case Code.decode (0, pos, code)
      of NONE => NONE
       | SOME Code.HaltOp => NONE
       | SOME (Code.Nop n) => check code pos (Code.Jmp n)
       | SOME (Code.Jmp n) => check code pos (Code.Nop n)
       | _ => tryswaps code (pos + 1)
  and check code pos new =
    case run (Code.load (Code.swap pos (Lambda.k new) code))
      of NONE => tryswaps code (pos + 1)
       | SOME InfiniteLoop => tryswaps code (pos + 1)
       | SOME (Halt n) => SOME n

  fun part2' code = tryswaps code 0

  val part2 = part2' o Option.valOf o Code.Reader.code o Readers.all o Readers.file
end
