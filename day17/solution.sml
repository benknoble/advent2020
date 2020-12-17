structure Solution = struct

  datatype cube = Active | Inactive
  val cube_reader =
    let
      val cube_map = CharMap'.fromList [(#"#", Active), (#".", Inactive)]
      val reader_2d =
        Readers.Map.fromString
        PointMap.insert
        PointMap.empty
        (fn c => CharMap'.find (cube_map, c))
    in
      Point3Map.fromList
      o (List.map (fn ({x, y}, c) => (Point3.new x y 0, c)))
      o PointMap.listItemsi
      o reader_2d
    end
  val read_map = cube_reader o Readers.all o Readers.file

  val directions =
    let
      val offsets = [~1, 0, 1]
    in
      List.filter (not o Lambda.is Point3.origin)
      (ListXProd.map
      (fn (x, (y, z)) => Point3.new x y z)
      (offsets, (ListXProd.map Lambda.id (offsets, offsets))))
    end

  fun neighbors p =
    List.map (Point3.move p) directions

  fun count_active cubes =
    List'.count_matching (Lambda.is Active)
    o (List.mapPartial (fn p => Point3Map.find (cubes, p)))

  fun next_active cubes p =
    if Range.includes {min=2, max=3} (count_active cubes (neighbors p))
    then Active
    else Inactive

  fun next_inactive cubes p =
    if Range.includes {min=3, max=3} (count_active cubes (neighbors p))
    then Active
    else Inactive

  fun next cubes =
    Point3Map.foldli (fn (p, c, cubes') =>
      List.foldl
      (fn (p', cubes'') =>
        Point3Map.insert ( cubes''
                         , p'
                         , case Point3Map.find (cubes, p')
                             of SOME Active => next_active cubes p'
                              | SOME Inactive => next_inactive cubes p'
                              (* inactive, but wasn't already in the map *)
                              | NONE => next_inactive cubes p'))
      cubes'
      (p::(neighbors p)))
    Point3Map.empty
    cubes

  fun run n cubes =
    if n = 0
    then cubes
    else run (n-1) (next cubes)

  val part1' = List'.count_matching (Lambda.is Active) o Point3Map.listItems o run 6
  val part1 = part1' o read_map

end
