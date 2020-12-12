structure Solution = struct

  datatype direction = North | South | East | West
  datatype rotation = Quarter | Half | ThreeQuarter
  datatype action = N of int
                  | S of int
                  | E of int
                  | W of int
                  | L of rotation
                  | R of rotation
                  | F of int

  type boat = direction * Point.point
  type nav = action list

  val init = (East, Point.origin)

  fun rotateL d r =
    case (d, r)
      of (North, Quarter) => West
       | (North, Half) => South
       | (North, ThreeQuarter) => East
       | (East, Quarter) => North
       | (East, Half) => West
       | (East, ThreeQuarter) => South
       | (South, Quarter) => East
       | (South, Half) => North
       | (South, ThreeQuarter) => West
       | (West, Quarter) => South
       | (West, Half) => East
       | (West, ThreeQuarter) => North

  fun rotateR d r =
    rotateL d (case r
                 of Quarter => ThreeQuarter
                  | ThreeQuarter => Quarter
                  | Half => Half)

  fun step (d, p) a =
    case a
      of N y => (d, Point.move p (Point.new 0 y))
       | S y => (d, Point.move p (Point.new 0 (~y)))
       | E x => (d, Point.move p (Point.new x 0))
       | W x => (d, Point.move p (Point.new (~x) 0))
       | L r => (rotateL d r, p)
       | R r => (rotateR d r, p)
       | F n => (d, case d
                      of North => Point.move p (Point.new 0 n)
                       | South => Point.move p (Point.new 0 (~n))
                       | East => Point.move p (Point.new n 0)
                       | West => Point.move p (Point.new (~n) 0))

  val run = List.foldl (fn (a, b) => step b a)

  structure Navigation = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun n getc = (PC.string "N" $> Lambda.k N) getc
    fun s getc = (PC.string "S" $> Lambda.k S) getc
    fun e getc = (PC.string "E" $> Lambda.k E) getc
    fun w getc = (PC.string "W" $> Lambda.k W) getc
    fun f getc = (PC.string "F" $> Lambda.k F) getc

    fun nsewf getc =
      ((||| [n, s, e, w, f] +> decp)
      $> (fn (f, i) => f i))
      getc

    fun l getc = (PC.string "L" $> Lambda.k L) getc
    fun r getc = (PC.string "R" $> Lambda.k R) getc

    fun rotationp getc =
      (||| [ PC.string "90" $> Lambda.k Quarter
           , PC.string "180" $> Lambda.k Half
           , PC.string "270" $> Lambda.k ThreeQuarter ])
      getc

    fun lr getc =
      (((l || r) +> rotationp)
      $> (fn (f, r) => f r))
      getc

    fun navp getc =
      (++ ((lr || nsewf) +> ?? (PC.char #"\n"))
      $> List.map #1)
      getc

    val nav = prun navp
  end

  fun part1' nav =
    let val (_, pos) = run init nav
    in Point.manhattan pos Point.origin
    end

  val part1 = (Option.map part1') o Navigation.nav o Readers.all o Readers.file

end
