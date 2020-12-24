signature TIME_IT = sig
  val t: (unit -> 'a) -> 'a
end

structure TimeIt: TIME_IT = struct
  fun t f =
    let
      val timer = Timer.startRealTimer ()
      val res = f ()
      val time = Timer.checkRealTimer timer
      val ms = Time.toMilliseconds time
      val us = Time.toMicroseconds time
      val toprint =
        if ms = 0
        then (LargeInt.toString us) ^ " us\n"
        else (LargeInt.toString ms) ^ " ms\n"
    in
      print toprint;
      res
    end
end
