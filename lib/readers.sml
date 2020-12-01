structure Readers = struct

  fun collect_t t_to_string = List.mapPartial t_to_string
  val on_spaces = String.tokens Char.isSpace
  val all = TextIO.inputAll
  val file = TextIO.openIn

  val file_to_int_list = (collect_t Int.fromString) o on_spaces o all o file

end
