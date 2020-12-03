structure CharMap = RedBlackMapFn(struct
  type ord_key = char
  val compare = Char.compare
end)
