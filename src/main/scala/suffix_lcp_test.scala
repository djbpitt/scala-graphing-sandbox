import reptilian.{calculate_lcp_array, vectorize}
import org.hammerlab.suffixes.dc3.make as calculate_suffix_array

@main def test_suffix_lcp():Unit =
// simple example to test LCP construction
  val token_array = Array("b", "a", "n", "a", "n", "a", "b", "a","n", "$")
  val (vectorization, voc_size) = vectorize(token_array)
  val suffix_array = calculate_suffix_array(vectorization, voc_size)
  println(suffix_array.mkString(", "))
  println(suffix_array sameElements Array(9, 5, 7, 3, 1, 6, 0, 8, 4, 2))

  for suffix_start <- suffix_array do
    println(token_array.slice(suffix_start, suffix_start+15 min token_array.length).mkString(" "))

  val lcp_array = calculate_lcp_array(token_array, suffix_array)
  println(lcp_array.mkString(" "))
  println(lcp_array sameElements Array(-1, 0, 1, 2, 3, 0, 3, 0, 1, 2))

