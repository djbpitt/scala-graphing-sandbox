package reptilian


/** *Dump suffix array and lcp array with initial tokens */
def dump_suffix_array(suffix_array: Array[Int], lcp_array: Vector[Int], token_array: Vector[Token]): Unit =
  println("Start dump suffix and LCP array")
  val suffix_array_output = suffix_array
    .map(e => token_array.slice(e, e + 35 min token_array.size))
    .map(_.map(_.t))
    .map(_.mkString(""))
    .zipWithIndex
    .map((string, index) => s"$string : $index : ${lcp_array(index)}\n")
  // Diagnostic: save suffix array and lcp array information
  val sa_output_path = os.pwd / "src" / "main" / "output" / "suffix_array.txt"
  os.write.over(sa_output_path, suffix_array_output)
