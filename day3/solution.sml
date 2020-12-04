structure Solution = struct
  datatype terrain = Space | Tree

  val toboggan_map_reader =
    let val terrain_map = CharMap.insert (CharMap.insert (CharMap.empty, #"#", Tree), #".", Space)
    in
      Readers.Map.fromString
      PointMap.insert
      PointMap.empty
      (fn c => CharMap.find (terrain_map, c))
    end

  val find_top_right =
    List.foldl
    (fn (cur, acc) => if #x cur >= #x acc andalso #y cur >= #y acc
                      then cur
                      else acc)
    Point.origin

  fun horizontally_wrapped_pos top_right p =
    Point.map2p (fn (px, tx) => px mod (tx + 1)) #1 p top_right

  fun terrain_along_slope m slope start =
    let
      val top_right = find_top_right (PointMap.listKeys m)
      val wrap = horizontally_wrapped_pos top_right
      fun terrain_along_slope' acc pos =
        case PointMap.find (m, pos)
          of NONE => acc
           | SOME t => terrain_along_slope' (t::acc) (wrap (Point.move pos slope))
    in
      List.rev (terrain_along_slope' [] start)
    end

  val read_map = toboggan_map_reader o Readers.all o Readers.file

  fun count_trees slope m =
    List'.count_matching (Lambda.is Tree)
    (terrain_along_slope m slope Point.origin)

  val part1' = count_trees (Point.new 3 1)
  val part1 = part1' o read_map

  fun part2' m =
    let
      val slopes =
        [ Point.new 1 1
        , Point.new 3 1
        , Point.new 5 1
        , Point.new 7 1
        , Point.new 1 2 ]
      val trees = List.map (fn s => count_trees s m) slopes
    in
      List'.prod trees
    end
  val part2 = part2' o read_map

end
