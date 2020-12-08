structure Solution = struct
  fun part1' proc =
    let
      fun break proc (visited, prev_acc) =
        case Code.step proc
          of proc' as (Code.Running (acc, pc, _)) =>
               if IntRedBlackSet.member (visited, pc)
               then prev_acc
               else break proc' ((IntRedBlackSet.add (visited, pc)), SOME acc)
           | _ => NONE
    in
      break proc (IntRedBlackSet.empty, NONE)
    end

  val part1 = part1' o Code.load o Option.valOf o Code.Reader.code o Readers.all o Readers.file

  fun part2' code =
    let
      datatype result = InfiniteLoop | Halt of int
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
        let
          fun check new n =
            case run (Code.load (Code.swap pos (Lambda.k (new n)) code))
              of NONE => tryswaps code (pos + 1)
               | SOME InfiniteLoop => tryswaps code (pos + 1)
               | SOME (Halt n) => SOME n
        in
          (* 0 is a placeholder; doesn't matter what the accumulator is for
           * decode *)
          case Code.decode (0, pos, code)
            of NONE => NONE
             | SOME Code.HaltOp => NONE
             | SOME (Code.Nop n) => check Code.Jmp n
             | SOME (Code.Jmp n) => check Code.Nop n
             | _ => tryswaps code (pos + 1)
        end
    in
      tryswaps code 0
    end

  val part2 = part2' o Option.valOf o Code.Reader.code o Readers.all o Readers.file
end
