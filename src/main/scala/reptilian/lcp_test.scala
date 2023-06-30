package reptilian

import org.hammerlab.suffixes.dc3.make as calculate_suffix_array

import scalatags.Text.all.raw

import scala.util.matching.Regex

@main def testmain(): Unit =

  val witnesses = List(
    "Or, secondly, that each breed, even the purest, has within a dozen\t\tor\t,\tat most\t,\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t\tor\t,\tat most\t,\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t,\tor\t\tat most\t\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t,\tor\t\tat most\t\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t,\tor\t\tat most\t\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen	,	or		at most		within a score"
  )

  val token_pattern: Regex = raw"(\w+|[^\w\s])\s*".r // From CollateX Python, syntax adjusted for Scala
  val tokenizer = make_tokenizer(token_pattern)

  val token_array = tokenize(tokenizer)(witnesses)
  println(token_array)

  val (vectorization, voc_size) = vectorize(token_array)
  val suffix_array = calculate_suffix_array(vectorization, voc_size)
  val lcp_array = calculate_lcp_array(token_array, suffix_array)
//  println(suffix_array)
//  println(lcp_array)

  // Dump suffix array and lcp array with initial tokens
  suffix_array
    .map(e => token_array.slice(e, e + 35 min token_array.size))
    .map(_.map(_.t))
    .map(_.mkString(" "))
    .zipWithIndex
    .foreach((string, index) => println(s"$string : $index : ${lcp_array(index)}"))
