structure Solution = struct
  type bag = Atom.atom
  type contained_in = int * bag
  (* b1 -> [(n, b2), …] means n b1s are contained in b2, &c. *)
  type contained_graph = contained_in list AtomMap.map
  type contains = int * bag
  (* b1 -> [(n, b2), …] means n b2s are contained in b1, &c. *)
  type contains_graph = contains list AtomMap.map

  val bag = Atom.atom
  fun contained (n, b) = (n, bag b)
  fun contained_graph (kvs: (bag * contains list) list): contained_graph =
    List.foldl
    (fn ((k, v), m) => AtomMap.insertWith List.@ (m, k, v))
    AtomMap.empty
    kvs
  val contains_graph: (bag * contains list) list -> contains_graph =
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

  fun part2' (g: contains_graph) =
    let
      fun requires' (n, bag) =
        let
          val neighbors = neighbors g bag
        in
          (n + n * (if List.null neighbors
                    then 0
                    else List'.sum (List.map requires' neighbors)))
        end
      fun requires (n, bag) = requires' (n, bag) - n
    in
      requires (1, Atom.atom "shiny gold")
    end

  val part2 = part2' o contained_to_contains o Option.valOf o Rules.rulesp o Readers.all o Readers.file

end
