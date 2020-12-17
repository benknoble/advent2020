structure Point = struct
  type point = {x : int, y : int}
  val origin : point = {x=0, y=0}

  fun new (x : int) (y : int) : point = {x=x, y=y}
  fun new' (x, y) = new x y

  fun map (f : int * int -> 'a) (p : point) : 'a =
    let val {x, y} = p
    in f (x, y)
    end

  fun compare (p1 : point, p2 : point) : order =
    map (fn (x, y) =>
    map (fn (x', y') => case Int.compare (x, x')
                          of EQUAL => Int.compare(y, y')
                           | ord => ord)
    p2)
    p1

  fun map2
    (fx : int * int -> 'a)
    (fy : int * int -> 'b)
    (p1 : point)
    (p2 : point)
    : 'a * 'b =
    let
      val {x, y} = p1
      val {x=x', y=y'} = p2
    in
      (fx (x, x')
      ,fy (y, y'))
    end

  fun map2p fx fy p1 p2 =
    let val (x', y') = map2 fx fy p1 p2
    in new x' y'
    end

  fun manhattan (p1 : point) (p2 : point) : int =
    let val lineLength = abs o (op -)
        val (dx, dy) = map2 lineLength lineLength p1 p2
    in dx + dy
    end

  val dist = manhattan
  val distToOrigin = dist origin

  fun move (p : point) (dir : point) : point =
    let val (x', y') = map2 (op +) (op +) p dir
    in new x' y'
    end

  fun slope (p1 : point) (p2 : point) : real =
    let val (dx, dy) = map2 (op -) (op -) p1 p2
    in real dy / real dx
    end

  fun onSameSlopeToOrigin (p1 : point) (p2 : point) : bool =
    map (fn (x1, y1) =>
    map (fn (x2, y2) =>
      (* y - y1 = (Δy/Δx) (x - x1) + c ; let y,x,c=0 and solve *)
      y1 * (x2 - x1) = x1 * (y2 - y1))
    p2)
    p1

  (* the cantor pairing function
   * https://en.wikipedia.org/wiki/Pairing_function *)
  val pi : point -> int = map (fn (x, y) => ((x + y) * (x + y + 1)) div 2 + y)
  fun pi' (n : int) : point =
    let
      val w = floor ((Math.sqrt (real (8 * n + 1)) - 1.0) / 2.0)
      val t = (w * w + w) div 2
      val y = n - t
      val x = w - y
    in
      new x y
    end

  fun pointsInSquare (size : int) (topleft : point) : point list =
    List.concat
    (map (fn (x, y) =>
        List.tabulate (size, fn x' =>
        List.tabulate (size, fn y' =>
          new (x + x') (y - (size-1) + y'))))
        topleft)

  datatype quad = I | II | III | IV | XP | XN | YP | YN | O
  val quadOf : point -> quad =
    map (fn (x, y) => case (Int.sign x, Int.sign y)
                        of (0,0) => O
                         | (1,0) => XP
                         | (1,1) => I
                         | (0,1) => YP
                         | (~1,1) => II
                         | (~1,0) => XN
                         | (~1,~1) => III
                         | (0,~1) => YN
                         | (1,~1) => IV)

  fun inSameQuadrant (p1 : point) (p2 : point) : bool = quadOf p1 = quadOf p2

end

structure PointN = struct
  type pointN = int list

  val dim = List.length

  fun origin n = List'.rep n 0

  fun new n pis =
    let val len = List.length pis
    in
      if len > n
      then raise ListPair.UnequalLengths
      else pis @ (List'.rep (n - len) 0)
    end

  fun map f p = Option.map (fn (h, t) => List.foldl f h t) (List.getItem p)

  fun map' fs p = ListPair.mapEq (fn (f, pi) => f pi) (fs, p)

  fun map2 fs p1 p2 =
    ListPair.mapEq (fn (f, (p1i, p2i)) => f (p1i, p2i))
    (fs, ListPair.zipEq (p1, p2))

  fun map2' f p1 p2 = map2 (List'.rep (List.length p1) f) p1 p2

  val move = map2' op+

  val compare = List.collate Int.compare
end

structure PointSet = RedBlackSetFn(struct
  type ord_key = Point.point
  val compare = Point.compare
end)

structure PointMap = RedBlackMapFn(struct
  type ord_key = Point.point
  val compare = Point.compare
end)
structure PointMap = WithMapUtilsFn(structure M = PointMap)

structure PointNMap = RedBlackMapFn(struct
  type ord_key = PointN.pointN
  val compare = PointN.compare
end)
structure PointNMap = WithMapUtilsFn(structure M = PointNMap)
structure PointNSet = PointNMap.KeySet
