signature DAY7 = sig
  type bag
  type contained_in
  type contained_graph
  type contains
  type contains_graph

  val bag: string -> bag
  val contained: int * string -> contained_in
  val contained_graph: (bag * contained_in list) list -> contained_graph
  val contains_graph: (bag * contains list) list -> contains_graph

  val contained_to_contains: contained_graph -> contains_graph

  val neighbors: contained_graph -> bag -> contained_in list
  structure ContainedDfs: DFS where type graph = contained_graph

  structure Rules: sig
    val colorp: (string, 'strm) ParserComb.parser
    val bagp: (bag, 'strm) ParserComb.parser
    val nbagp: (contains list, 'strm) ParserComb.parser
    val rulep: ((bag * contained_in list) list, 'strm) ParserComb.parser
    val rulesp': (contained_graph, 'strm) ParserComb.parser
    val rulesp: string -> contained_graph option
  end

  val test_read: string -> (string * (int * string) list) list option

  val part1': contained_graph -> int
  val part1: string -> int

  val requires': contains_graph -> int * bag -> int
  val requires: contains_graph -> int * bag -> int

  val part2': contains_graph -> int
  val part2: string -> int
end

structure Solution: DAY7 = struct
  type bag = Atom.atom
  type contained_in = int * bag
  (* b1 -> [(n, b2), …] means n b1s are contained in b2, &c. *)
  type contained_graph = contained_in list AtomMap.map
  type contains = int * bag
  (* b1 -> [(n, b2), …] means n b2s are contained in b1, &c. *)
  type contains_graph = contains list AtomMap.map

  val bag = Atom.atom
  fun contained (n, b) = (n, bag b)
  fun contained_graph kvs =
    List.foldl
    (fn ((k, v), m) => AtomMap.insertWith List.@ (m, k, v))
    AtomMap.empty
    kvs
  val contains_graph =
    (* by a quirk, they have the same representation, so this is fine *)
    contained_graph

  val contained_to_contains: contained_graph -> contains_graph =
    contains_graph
    o List.concat
    o (List.map (fn (contained, count_containers) =>
        List.map (fn (n, tainer) => (tainer, [(n, contained)])) count_containers))
    o AtomMap.listItemsi

  fun neighbors g n = case AtomMap.find (g, n)
                        of NONE => []
                         | SOME ns => ns
  structure ContainedDfs = DfsFn(struct
    type edge = int
    type graph = contained_graph
    structure Node = AtomMap.Key
    val neighbors = neighbors
    val nodes = AtomMap.listKeys
  end)

  structure Rules = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun colorp getc = (PC.token (not o Char.isSpace)) getc

    fun bagp getc =
      (((skip_ws colorp) +> (skip_ws colorp) +> (skip_ws (PC.string "bag")) +> ?? (PC.char #"s"))
      $> (fn (c1, (c2, _)) => bag (c1 ^ " " ^ c2)))
      getc

    fun nbagp getc =
      ((((++ (skip_ws decp +> skip_ws bagp +> ?? (PC.char #",")))
          || (skip_ws (PC.string "no other bags") $> Lambda.k []))
        +> (PC.char #"."))
      $> ((List.map (fn (n, (b, _)) => (n, b))) o #1))
      getc

    fun rulep getc =
      ((bagp +> skip_ws (PC.string "contain") +> nbagp +> ?? (PC.char #"\n"))
      $> (fn (container, (_, (count_containeds, _))) =>
            (* tained -> [(n, container)]; see contained_graph *)
            List.map (fn (n, tained) => (tained, [(n, container)])) count_containeds))
      getc

    fun rulesp' getc = (++ rulep $> (contained_graph o List.concat)) getc
    val rulesp = prun rulesp'
  end

  (* just useful to make sure I got parsing right *)
  val test_read =
    Option.map
      ((List.map (fn (k, vs) =>
        (Atom.toString k, List.map (fn (k, v) => (k, Atom.toString v)) vs)))
      o AtomMap.listItemsi)
    o Rules.rulesp
    o Readers.all
    o Readers.file

  val part1' = ContainedDfs.NodeEdgeSet.numItems o (fn g => ContainedDfs.dfs g (Atom.atom "shiny gold"))
  val part1 = part1' o Option.valOf o Rules.rulesp o Readers.all o Readers.file


  (* g really needs to be a contains_graph, or this won't make sense *)
  fun requires' g (n, bag) =
    (n + n * (List'.sum (List.map (requires' g) (neighbors g bag))))
  (* requires' overcounts: it includes the count of each bag that it adds,
   * including the start, while we often want to know how many bags are required
   * *aside* from the n of bag we already have.
   *
   * subtracting n in requires' won't work: then you just get n*(…) and it
   * under-counts (especially for the contained bags)
   *
   * so we subtract it here *)
  fun requires g (n, bag) = requires' g (n, bag) - n

  fun part2' g = requires g (1, Atom.atom "shiny gold")
  val part2 = part2' o contained_to_contains o Option.valOf o Rules.rulesp o Readers.all o Readers.file

end
