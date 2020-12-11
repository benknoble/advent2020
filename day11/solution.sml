structure Solution = struct

  datatype seat = Floor | Empty | Occupied

  val seat_map_reader =
    let val seat_map = CharMap'.fromList [ (#".", Floor)
                                         , (#"L", Empty)
                                         , (#"#", Occupied) ]
    in
      Readers.Map.fromString
      PointMap.insert
      PointMap.empty
      (fn c => CharMap'.find (seat_map, c))
    end
  val read_map = seat_map_reader o Readers.all o Readers.file

  fun prints seats =
    let
      fun to_c s = case s
                     of Floor => #"."
                      | Empty => #"L"
                      | Occupied => #"#"
      val ps = PointMap.listKeys seats
      val {x=max_x, y=max_y} =
        List.foldl (fn (p1, p2) => case Point.compare (p1, p2)
                                     of EQUAL => p1
                                      | LESS => p2
                                      | GREATER => p1)
        Point.origin
        ps
      val xs = Range.toList {min=0, max=max_x}
      val ys = Range.toList {min=0, max=max_y}
      val str =
        String.concatWith "\n"
        (List.map (fn y =>
          String.implode
          (List.map (fn x => to_c (PointMap.lookup (seats, Point.new x y)))
          xs))
        ys)
    in
      print (str ^ "\n")
    end

  val directions =
    List.map Point.new' [ (~1, ~1)
                        , (~1, 0)
                        , (~1, 1)
                        , (0, ~1)
                        , (0, 1)
                        , (1, ~1)
                        , (1, 0)
                        , (1, 1) ]

  fun neighbors seats p =
    let
      val nps = List.map (Point.move p) directions
    in
      List.mapPartial (fn p => PointMap.find (seats, p)) nps
    end

  val count_occupied = List'.count_matching (Lambda.is Occupied)

  fun next seats =
    PointMap.mapi (fn (p, s) =>
      case s
        of Floor => Floor
         | Empty => if count_occupied (neighbors seats p) = 0
                    then Occupied
                    else Empty
         | Occupied => if count_occupied (neighbors seats p) >= 4
                       then Empty
                       else Occupied)
    seats

  fun equivalent_seat_maps (m1, m2) =
    PointMap.collate (fn (s1, s2) => if s1 = s2 then EQUAL
                                     else case (s1, s2)
                                            of (Floor, _) => LESS
                                             | (Empty, Floor) => GREATER
                                             | (Empty, _) => LESS
                                             | (Occupied, _) => GREATER)
    (m1, m2)
    =
    EQUAL

  fun part1' seats =
    let
      fun until_stable seats prev =
        if equivalent_seat_maps (seats, prev)
        then seats
        else
          let val next = next seats
          in
            (* prints next; *)
            (* print "\n\n"; *)
            (* OS.Process.sleep (Time.fromMilliseconds 100); *)
            until_stable next seats
          end
    in
      count_occupied (PointMap.listItems (until_stable seats PointMap.empty))
    end

  val part1 = part1' o read_map

end
