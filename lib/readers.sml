structure Readers = struct

  fun collect_t t_to_string = List.mapPartial t_to_string
  fun on_char c = String.tokens (fn c' => c' = c)
  fun on_chars cs = String.tokens (fn c => List.exists (fn c' => c' = c) cs)
  val on_spaces = String.tokens Char.isSpace
  val lines = on_char #"\n"
  val all = TextIO.inputAll
  val file = TextIO.openIn

  val file_to_int_list = (collect_t Int.fromString) o on_spaces o all o file

end
