signature LAMBDA = sig
  val id: 'a -> 'a
  val k: 'a -> 'b -> 'a
  val is: ''a -> ''a -> bool
end

structure Lambda: LAMBDA = struct
  fun id x = x
  fun k x _ = x
  fun is x y = x = y
end
