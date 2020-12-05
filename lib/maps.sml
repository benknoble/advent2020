structure CharMap = RedBlackMapFn(struct
  type ord_key = char
  val compare = Char.compare
end)

structure KeySet = AtomSet
structure Dict = struct
  open AtomMap

  fun keys d = (KeySet.fromList o listKeys) d
end
