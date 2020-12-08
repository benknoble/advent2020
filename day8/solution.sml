structure Solution = struct
  fun part1' proc =
    let
      fun break proc (visited, prev_acc) =
        case Code.step proc
          of proc' as (Code.Running (acc, pc, _)) =>
               if IntRedBlackSet.member (visited, pc)
               then prev_acc
               else break proc' ((IntRedBlackSet.add (visited, pc)), SOME acc)
           | _ => prev_acc
    in
      break proc (IntRedBlackSet.empty, NONE)
    end

  val part1 = part1' o Code.load o Option.valOf o Code.Reader.code o Readers.all o Readers.file
end
