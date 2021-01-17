signature DAY24 = sig
  datatype color = Black | White
  datatype dir = E | SE | SW | W | NW | NE

  val flip: color -> color

  type ref_loc
  type real_loc

  val dir_to_point: dir -> Point.point
  val ref_to_real: ref_loc -> real_loc

  type floor

  val %= : floor * (Point.point * color) -> floor
  val % : floor * Point.point -> color

  structure Tiles: sig
    val dirp: (dir, 'strm) ParserComb.parser
    val ref_locp: (ref_loc, 'strm) ParserComb.parser
    val tilesp: (ref_loc list, 'strm) ParserComb.parser
    val tiles: string -> ref_loc list option
  end

  val flip_tiles: ref_loc list -> floor

  val count_black: floor -> int

  val part1': ref_loc list -> int
  val part1: string -> int option

  val adjacent_dirs: Point.point list
  val neighbors: Point.point -> Point.point list
  val all_neighbors: floor -> PointSet.set
  val count_neighbors: color -> floor -> Point.point -> int

  val step: floor -> floor
  val stepN: int -> floor -> floor

   val part2': ref_loc list -> int
   val part2: string -> int option
end

structure Solution: DAY24 = struct

  datatype color = Black | White
  datatype dir = E | SE | SW | W | NW | NE

  fun flip tile =
    case tile
      of Black => White
       | White => Black

  type ref_loc = dir list
  type real_loc = Point.point

  fun dir_to_point dir =
    case dir
      of E => Point.new 2 0
       | SE => Point.new 1 ~1
       | SW => Point.new ~1 ~1
       | W => Point.new ~2 0
       | NW => Point.new ~1 1
       | NE => Point.new 1 1

  val ref_to_real =
    List.foldl (fn (dir, acc) => Point.move acc (dir_to_point dir)) Point.origin

  type floor = color PointMap.map

  infix 3 %=
  fun m %= (i, c) =
    PointMap.insert (m, i, c)

  infix 3 %
  fun m % i =
    case PointMap.find (m, i)
      of NONE => White
       | SOME c => c

  structure Tiles = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun dirp getc =
      (||| [ PC.string "ne" $> Lambda.k NE
           , PC.string "nw" $> Lambda.k NW
           , PC.string "se" $> Lambda.k SE
           , PC.string "sw" $> Lambda.k SW
           , PC.string "e" $> Lambda.k E
           , PC.string "w" $> Lambda.k W ])
      getc

    fun ref_locp getc = (++ dirp) getc
    fun tilesp getc = (++ ((ref_locp +> ?? (PC.char #"\n")) $> #1)) getc
    val tiles = prun tilesp
  end

  val flip_tiles =
    List.foldl (fn (loc, acc) =>
      let val real_loc = ref_to_real loc
      in acc %= (real_loc, flip (acc % real_loc))
      end)
    PointMap.empty

  val count_black = PointMap.numItems o PointMap.filter (Lambda.is Black)

  val part1' = count_black o flip_tiles
  val part1 = Option.map part1' o Tiles.tiles o Readers.all o Readers.file

  val adjacent_dirs = List.map dir_to_point [E, SE, SW, W, NW, NE]
  fun neighbors p = List.map (Point.move p) adjacent_dirs

  fun all_neighbors floor =
    (PointSet.fromList o List.concat o List.map neighbors o PointMap.listKeys)
    floor

  fun count_neighbors x floor =
    List'.count_matching (Lambda.is x)
    o List.map (fn p => floor % p)
    o neighbors

  fun step floor =
    PointSet.foldl (fn (p, floor') =>
      floor' %= (p, let val black_neighbors = count_neighbors Black floor p
                    in case floor % p
                         of Black =>
                              if black_neighbors = 0 orelse black_neighbors > 2
                              then White
                              else Black
                          | White =>
                              if black_neighbors = 2
                              then Black
                              else White
                    end))
    PointMap.empty
    (all_neighbors floor)

  fun stepN n floor =
    if n = 0
    then floor
    else stepN (n-1) (step floor)

  val part2' = count_black o (stepN 100) o flip_tiles
  val part2 = Option.map part2' o Tiles.tiles o Readers.all o Readers.file

end
