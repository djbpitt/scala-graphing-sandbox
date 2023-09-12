package net.collatex.reptilian

import org.hammerlab.suffixes.dc3.make as calculate_suffix_array

import scalatags.Text.all.raw

import scala.util.matching.Regex

@main def testMain(): Unit =

  val witnesses = List(
    "Or, secondly, that each breed, even the purest, has within a dozen\t\tor\t,\tat most\t,\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t\tor\t,\tat most\t,\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t,\tor\t\tat most\t\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t,\tor\t\tat most\t\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen\t,\tor\t\tat most\t\twithin a score",
    "Or, secondly, that each breed, even the purest, has within a dozen	,	or		at most		within a score"
  )

  val witnesses01 = List(
    "When, on the one hand, we see domesticated animals and plants, though often weak and sickly, yet breeding quite freely under confinement; and when, on the other hand, we see individuals, though taken young from a state of nature, perfectly tamed, long-lived, and healthy (of which I could give numerous instances), yet having their reproductive system so seriously affected by unperceived causes as to fail in acting, we need not be surprised at this system, when it does act under confinement, acting not quite regularly, and producing offspring not perfectly like their parents or variable.",
    "When, on the one hand, we see domesticated animals and plants, though often weak and sickly, yet breeding quite freely under confinement; and when, on the other hand, we see individuals, though taken young from a state of nature, perfectly tamed, long-lived, and healthy (of which I could give numerous instances), yet having their reproductive system so seriously affected by unperceived causes as to fail in acting, we need not be surprised at this system, when it does act under confinement, acting not quite regularly, and producing offspring not perfectly like their parents.",
    "When, on the one hand, we see domesticated animals and plants, though often weak and sickly, yet breeding quite freely under confinement; and when, on the other hand, we see individuals, though taken young from a state of nature, perfectly tamed, long-lived, and healthy (of which I could give numerous instances), yet having their reproductive system so seriously affected by unperceived causes as to fail in acting, we need not be surprised at this system, when it does act under confinement, acting not quite regularly, and producing offspring not perfectly like their parents.",
    "When, on the one hand, we see domesticated animals and plants, though often weak and sickly, yet breeding quite freely under confinement; and when, on the other hand, we see individuals, though taken young from a state of nature, perfectly tamed, long-lived, and healthy (of which I could give numerous instances), yet having their reproductive system so seriously affected by unperceived causes as to fail in acting, we need not be surprised at this system, when it does act under confinement, acting not quite regularly, and producing offspring not perfectly like their parents.",
    "When, on the one hand, we see domesticated animals and plants, though often weak and sickly, yet breeding freely under confinement; and when, on the other hand, we see individuals, though taken young from a state of nature, perfectly tamed, long-lived, and healthy (of which I could give numerous instances), yet having their reproductive system so seriously affected by unperceived causes as to fail to act, we need not be surprised at this system, when it does act under confinement, acting irregularly, and producing offspring somewhat unlike their parents.",
    "When, on the one hand, we see domesticated animals and plants, though often weak and sickly, breeding freely under confinement; and when, on the other hand, we see individuals, though taken young from a state of nature perfectly tamed, long-lived and healthy (of which I could give numerous instances), yet having their reproductive system so seriously affected by unperceived causes as to fail to act, we need not be surprised at this system, when it does act under confinement, acting irregularly, and producing offspring somewhat unlike their parents."
  )

  val token_pattern: Regex = raw"(\w+|[^\w\s])\s*".r // From CollateX Python, syntax adjusted for Scala
  val tokenizer = makeTokenizer(token_pattern)

  val token_array = tokenize(tokenizer)(witnesses01)
//  println(token_array)

  val (vectorization, voc_size) = vectorize(token_array)
  val suffix_array = calculate_suffix_array(vectorization, voc_size)
  val lcp_array1 = calculate_lcp_array_kasai(token_array.map(_.n), suffix_array)
//  println(suffix_array)
//  println(lcp_array)
//  println(vectorization.mkString(" "))

  // Dump suffix array and lcp array with initial tokens
  suffix_array
    .map(e => token_array.slice(e, e + 35 min token_array.size))
    .map(_.map(_.t))
    .map(_.mkString(" "))
    .zipWithIndex
    .foreach((string, index) => println(s"$string : $index : ${lcp_array1(index)}"))

//  val blocks = createAlignedBlocks(token_array, 6)
//  blocks.foreach(println)
//  blocks
//    .map(_.show(token_array))
//    .foreach(println)

  // Represent blocks as token string
//  val blocks_slice_tokens = blocks
//    .map(e => token_array.slice(suffix_array(e.start), suffix_array(e.start) + e.length))
//  val block_count = blocks
//    .map(e => e.end - e.start + 1)
//  println("Start dump blocks within the slice")
//  blocks_slice_tokens
//    .map(_.map(e => e.n)
//      .mkString(" "))
//    .zip(block_count)
//    .foreach(println)