structure Solution = struct

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

  val part1' =
    PointMap.numItems
    o PointMap.filter (Lambda.is Black)
    o List.foldl
      (fn (loc, acc) =>
        let val real_loc = ref_to_real loc
        in acc %= (real_loc, flip (acc % real_loc))
        end)
      PointMap.empty
  val part1 = Option.map part1' o Tiles.tiles o Readers.all o Readers.file

end
