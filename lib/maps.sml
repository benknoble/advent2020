signature ORD_MAP_UTILS = sig
  include ORD_MAP
  structure KeySet: ORD_SET sharing type Key.ord_key = Key.ord_key
  val keys: 'a map -> KeySet.set
  val fromList: (Key.ord_key * 'a) list -> 'a map
end

functor WithMapUtilsFn(structure M: ORD_MAP): ORD_MAP_UTILS = struct
  open M

  structure KeySet = RedBlackSetFn(Key)

  fun keys d = (KeySet.fromList o listKeys) d

  fun fromList kvs = List.foldl insert' empty kvs
end

structure CharMap' = RedBlackMapFn(struct
  type ord_key = char
  val compare = Char.compare
end)
structure CharMap' = WithMapUtilsFn(structure M = CharMap')

structure Dict = WithMapUtilsFn(structure M = AtomMap)
