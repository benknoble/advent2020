functor WithMapUtilsFn(structure M: ORD_MAP) = struct
  open M

  structure KeySet = RedBlackSetFn(Key)

  fun keys d = (KeySet.fromList o listKeys) d

  fun fromList kvs = List.foldl insert' empty kvs
end

structure CharMap = RedBlackMapFn(struct
  type ord_key = char
  val compare = Char.compare
end)
structure CharMap = WithMapUtilsFn(structure M = CharMap)

structure Dict = WithMapUtilsFn(structure M = AtomMap)
