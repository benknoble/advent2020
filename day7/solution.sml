structure Solution = struct
  type bag = Atom.atom
  type contained_in = int * bag
  (* b1 -> [(n, b2), …] means n b1s are contained in b2, &c. *)
  type contained_graph = contained_in list AtomMap.map

  val bag = Atom.atom
  fun contained (n, b) = (n, bag b)
  fun contained_graph kvs =
    List.foldl
    (fn ((k, v), m) => AtomMap.insertWith List.@ (m, k, v))
    AtomMap.empty
    kvs

  structure Dfs = DfsFn(struct
    type edge = int
    type graph = contained_graph
    structure Node = AtomMap.Key
    fun neighbors g n = case AtomMap.find (g, n)
                          of NONE => []
                           | SOME ns => ns
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

  val part1' = Dfs.NodeSet.numItems o (fn g => Dfs.dfs g (Atom.atom "shiny gold"))
  val part1 = part1' o Option.valOf o Rules.rulesp o Readers.all o Readers.file

end
