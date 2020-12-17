structure Solution = struct

  datatype cube = Active | Inactive
  fun cube_reader n =
    let
      val cube_map = CharMap'.fromList [(#"#", Active), (#".", Inactive)]
      val reader_2d =
        Readers.Map.fromString
        PointMap.insert
        PointMap.empty
        (fn c => CharMap'.find (cube_map, c))
    in
      PointNMap.fromList
      o (List.map (fn ({x, y}, c) => (PointN.new n [x, y], c)))
      o PointMap.listItemsi
      o reader_2d
    end
  fun read_map n = (cube_reader n) o Readers.all o Readers.file

  fun directions n =
    let
      val offsets = [~1, 0, 1]
      fun loop n acc =
        if n = 1
        then acc
        else loop (n-1) (ListXProd.map op:: (offsets, acc))
    in
      List.filter (not o Lambda.is (PointN.origin n))
      (loop n (List'.transpose [offsets]))
    end

  fun neighbors n p =
    List.map (PointN.move p) (directions n)

  fun count_active cubes =
    List'.count_matching (Lambda.is Active)
    o (List.mapPartial (fn p => PointNMap.find (cubes, p)))

  fun next_active n cubes p =
    if Range.includes {min=2, max=3} (count_active cubes (neighbors n p))
    then Active
    else Inactive

  fun next_inactive n cubes p =
    if Range.includes {min=3, max=3} (count_active cubes (neighbors n p))
    then Active
    else Inactive

  fun next n cubes =
    PointNMap.foldli (fn (p, c, cubes') =>
      List.foldl
      (fn (p', cubes'') =>
        PointNMap.insert ( cubes''
                         , p'
                         , case PointNMap.find (cubes, p')
                             of SOME Active => next_active n cubes p'
                              | SOME Inactive => next_inactive n cubes p'
                              (* inactive, but wasn't already in the map *)
                              | NONE => next_inactive n cubes p'))
      cubes'
      (p::(neighbors n p)))
    PointNMap.empty
    cubes

  fun run dim n cubes =
    if n = 0
    then cubes
    else run dim (n-1) (next dim cubes)

  val part1' = List'.count_matching (Lambda.is Active) o PointNMap.listItems o run 3 6
  val part1 = part1' o (read_map 3)

end
