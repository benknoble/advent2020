structure Solution = struct

  type ingredient = Atom.atom
  type allergen = Atom.atom
  type ingredients = AtomSet.set
  type allergens = AtomSet.set
  type food = {ingredients: ingredients, allergens: allergens}
  type foods = food list

  structure Foods = struct
    open Readers.ParserOps
    infixr 3 $>
    infixr 3 +>
    infixr 3 >>
    infixr 3 ||

    fun ingredientp getc = (PC.token Char.isAlpha $> Atom.atom) getc
    fun ingredientsp getc = (++ (skip_ws ingredientp) $> AtomSet.fromList) getc

    fun allergenp getc = (PC.token Char.isAlpha $> Atom.atom) getc
    fun allergensp getc =
      ((allergenp +> ?+ ((PC.string ", " +> allergenp) $> #2))
      $> (AtomSet.fromList o op::))
      getc

    fun foodp getc =
      ((ingredientsp +> PC.string " (contains " +> allergensp +> PC.string ")")
      $> (fn (ingredients, (_, (allergens, _))) => {ingredients=ingredients, allergens=allergens}))
      getc

    fun foodsp getc = (++ ((foodp +> ?? (PC.char #"\n")) $> #1)) getc

    val foods = prun foodsp
  end

  val ingredients: foods -> ingredients =
    List.foldl AtomSet.union AtomSet.empty o List.map #ingredients

  val allergens: foods -> allergens =
    List.foldl AtomSet.union AtomSet.empty o List.map #allergens

  fun ingredients_possibly_containing allergen: foods -> ingredients =
    Option.valOf (* we know there will be at least one *)
    o Option.map (fn (h, t) => List.foldl AtomSet.intersection h t)
    o List.getItem
    o List.map #ingredients
    o List.filter (fn {allergens, ...} => AtomSet.member (allergens, allergen))

  fun allergens_to_possible_ingredients foods =
    AtomSet.foldl
    (fn (allergen, acc) =>
      AtomMap.insert (acc, allergen, ingredients_possibly_containing allergen foods))
    AtomMap.empty
    (allergens foods)

  val might_be_allergens =
    AtomMap.foldl AtomSet.union AtomSet.empty
    o allergens_to_possible_ingredients

  (* val printSet = List.map Atom.toString o AtomSet.toList *)

  fun cannot_be_allergens foods =
    AtomSet.difference (ingredients foods, might_be_allergens foods)

  fun count_uses ingredient: foods -> int =
    List'.count_matching (fn {ingredients, ...} => AtomSet.member (ingredients, ingredient))

  fun part1' foods =
    (List'.sum
    o List.map (fn i => count_uses i foods)
    o AtomSet.toList
    o cannot_be_allergens)
    foods

  val part1 = Option.map part1' o Foods.foods o Readers.all o Readers.file

  fun solve_eqns m =
    if AtomMap.all (Lambda.is 1 o AtomSet.numItems) m
    then SOME (AtomMap.map AtomSet.minItem m)
    else
      let
        val fixed =
          AtomMap.foldl AtomSet.union AtomSet.empty
          (AtomMap.filter (Lambda.is 1 o AtomSet.numItems) m)
      in
        if AtomSet.isEmpty fixed
        then NONE
        else
          let
            val sans_fixed =
              AtomMap.map
              (fn xs => if AtomSet.numItems xs = 1
                        then xs
                        else AtomSet.difference (xs, fixed))
              m
          in
            if AtomMap.exists AtomSet.isEmpty sans_fixed
            then NONE
            else solve_eqns sans_fixed
          end
      end

  val order_allergen_ingredients: ingredient AtomMap.map -> (allergen * ingredient) list =
    ListMergeSort.sort (Lambda.is GREATER o Atom.lexCompare o (fn ((k1, _), (k2, _)) => (k1, k2)))
    o AtomMap.listItemsi

  val part2' =
    Option.map (String.concatWith ","
                o List.map (Atom.toString o #2)
                o order_allergen_ingredients)
    o solve_eqns
    o allergens_to_possible_ingredients

  val part2 = Option.mapPartial part2' o Foods.foods o Readers.all o Readers.file

end
