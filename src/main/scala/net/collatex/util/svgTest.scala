package net.collatex.util

import scala.xml.Elem
import scala.xml.XML.save
import annotation.tailrec
import scala.jdk.CollectionConverters.*

object WitnessColors extends Enumeration {
  type witnessColor = Value
  val Red, Orange, Yellow, Green, Blue, Indigo, lightgray = Value
}
import WitnessColors._

object Sigla extends Enumeration {
  type siglum = Value
  val W59, W60, W61, W66, W69, W72 = Value
}
import Sigla._

val nodes = Vector(
  Node(readings = Map(W66 -> "causes of variability .", W69 -> "causes of variability .", W72 -> "causes of variability .")),
  Node(readings = Map(W59 -> "when we", W60 -> "when we", W61 -> "when we", W66 -> "when we", W69 -> "when we", W72 -> "when we")),
  Node(readings = Map(W59 -> "look to", W60 -> "look to", W61 -> "look to", W66 -> "look to", W69 -> "compare", W72 -> "compare")),
  Node(readings = Map(
    W59 -> "the individuals of the same variety or sub - variety of our older cultivated plants and animals , one of the first points which strikes us",
    W60 -> "the individuals of the same variety or sub - variety of our older cultivated plants and animals , one of the first points which strikes us",
    W61 -> "the individuals of the same variety or sub - variety of our older cultivated plants and animals , one of the first points which strikes us",
    W66 -> "the individuals of the same variety or sub - variety of our older cultivated plants and animals , one of the first points which strikes us",
    W69 -> "the individuals of the same variety or sub - variety of our older cultivated plants and animals , one of the first points which strikes us",
    W72 -> "the individuals of the same variety or sub - variety of our older cultivated plants and animals , one of the first points which strikes us"
  ))
)

@tailrec
def processReadingGroups(rgs: Map[String, Map[Sigla.Value, String]], pos: Int, acc: Vector[Elem]): Vector[Elem] =
  if rgs.isEmpty then
    acc
  else
    val currentRg = rgs.head
    val newItem: Elem = <rect></rect>
    processReadingGroups(rgs.tail, pos + currentRg._2.size, acc :+ newItem)

val svg: Elem =
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 180">
    {
    val nodesWithIndex = nodes.zipWithIndex
    for (n, i) <- nodesWithIndex yield
      val translateInstruction = "translate(" + i + ", 10)"
      val readingGroups = n.readings.groupBy((_, text) => text)
      val rects = processReadingGroups(readingGroups, 0, Vector.empty)
      <g transform={translateInstruction}> { rects } </g>
    }
  </svg>

@main def testSvg(): Unit =
  val pp = new scala.xml.PrettyPrinter(78, 2)
  val x = pp.format(svg)
  println(x)

case class Node(readings: Map[Sigla.Value, String])