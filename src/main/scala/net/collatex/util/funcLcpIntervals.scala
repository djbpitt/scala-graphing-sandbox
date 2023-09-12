package net.collatex.util

import net.collatex.reptilian.Block

// lets try to implement it in a functional style
// work in progress
def split_lcp_array_into_intervals(lcp: Array[Int]): Unit =
  val x = lcp.zipWithIndex
  println(x.mkString("Array(", ", ", ")"))

  val f = (x1:List[Block], y1: (Int, Int)) =>
    println(s"$x1 $y1")
    //NOTE: can I extract LCP value outside of the match? Probably
    (x1, y1._1) match
      case (Nil, lcp_value) if lcp_value > 0 => List(Block(y1._2, -1, lcp_value))
      case (Block(_,_, current_value)::tail, lcp_value) if lcp_value > current_value => List() //TODO add to list
      case _ => x1

  val y = x.foldLeft[List[Block]](List[Block]())(f)
  for xx <- y do
    println(xx)



@main def functional_lcp_intervals(): Unit =
  val lcp = Array[Int](-1, 0, 2, 3, 3, 3, 5, 2)
  split_lcp_array_into_intervals(lcp)
