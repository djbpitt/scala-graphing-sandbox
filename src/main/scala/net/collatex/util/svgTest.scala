package net.collatex.util

import scala.xml.Elem
import scala.xml.XML.save
import annotation.tailrec
import scala.jdk.CollectionConverters.*
import net.collatex.reptilian.{AlignmentTreeNode, IndelNode, ReadingNode, VariationNode}

object WitnessColors extends Enumeration {
  type witnessColor = Value
  val Peru, Orange, Yellow, LimeGreen, DodgerBlue, Violet = Value
}
import WitnessColors._

object Sigla extends Enumeration {
  type siglum = Value
  val W59, W60, W61, W66, W69, W72 = Value
}
import Sigla._

val nodes: Vector[AlignmentTreeNode] = Vector(
  ReadingNode(witnessReadings = Map("W59" -> (0, 1), "W60" -> (2, 3), "W61" -> (4, 5), "W66" -> (6, 7), "W69" -> (8, 9), "W72" -> (10, 11))),
  IndelNode(witnessReadings = Map("W66" -> (6, 7), "W69" -> (8, 9), "W72" -> (10, 11))),
  ReadingNode(witnessReadings = Map("W59" -> (0, 1), "W60" -> (2, 3), "W61" -> (4, 5), "W66" -> (6, 7), "W69" -> (8, 9), "W72" -> (10, 11)))
)

@tailrec
def processReadingGroups(rgs: Map[String, Map[Sigla.Value, String]], pos: Int, acc: Vector[Elem]): Vector[Elem] =
  if rgs.isEmpty then
    acc
  else
    val currentRg = rgs.head
    val newItem: Elem = <rect></rect>
    processReadingGroups(rgs.tail, pos + currentRg._2.size, acc :+ newItem)

/** Compute <path> elements from preceding node to current node
 *
 * One <path> per witness
 * Witness groupings and order (order of groups and order of witnesses within groups) of both nodes must be known
 * All witnesses are present in all output, either as explicit or, if absent from node, as inferred
 *
 * @param source as Node
 * @param target as Node
 * @return <g> with <path> children, one for each witness
 */

def computePath(source: AlignmentTreeNode, target: AlignmentTreeNode): Elem =
  ???

/* Create SVV for output
 *
 * Input is sequence of nodes
 * Output has two parts:
 *   1. Sigmoid connections from preceding node to current node (absent for first node)
 *      <g> with <path> children
 *   2. Current node <g> with <rect> children
 *
 */
val svg: Elem =
  /* Constants */
  val witW = 6
  val witH = 10
  val verticalNodeSpacing = 3 * witH // height of node plus twice height of node for sigmoid connectors
  val totalWitCount = Sigla.values.size
  /* End of constants*/
  /* Vertical line between present and absent witnesses first */
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 180">
    <g transform="translate(10)">
      <line
      transform={"translate(0, -" + ((verticalNodeSpacing - witH) / 2).toString + ")"}
      x1={(totalWitCount * witW + witW / 2).toString}
      y1="0"
      x2={(totalWitCount * witW + witW / 2).toString}
      y2={(nodes.size * verticalNodeSpacing).toString}
      stroke="gray"/>{val nodesWithIndex = nodes.zipWithIndex
    for (n, i) <- nodesWithIndex yield {
      val translateInstruction = "translate(0, " + (i * verticalNodeSpacing).toString + ")"
      val contents: Vector[Elem] = n match
        case ReadingNode(witnessReadings) =>
          val rects: Vector[Elem] =
            witnessReadings.keys.toSeq.sorted // Sort alphabetically by siglum
              .zipWithIndex
              .toVector
              .flatMap((siglum, offset) => {
                val xPos: String = (offset * witW).toString
                val fill: String = WitnessColors(offset).toString
                Vector(<rect x={xPos} y="0" width={witW.toString} height={witH.toString} fill={fill}/>,
                  <text x={(xPos.toInt + witW / 2).toString}
                        y={(witH / 2).toString}
                        text-anchor="middle"
                        dominant-baseline="central"
                        font-size={(witH / 2.5).toString}>
                    {siglum.drop(1)}
                  </text>)
              })
          rects
        case IndelNode(witnessReadings) =>
          val readingRects: Vector[Elem] =
            witnessReadings.keys.toSeq.sorted
              .zipWithIndex
              .toVector
              .flatMap((siglum, offset) => {
                val xPos: String = (offset * witW).toString
                val fill: String = WitnessColors(Sigla.withName(siglum).id).toString
                Vector(<rect x={xPos} y="0" width={witW.toString} height={witH.toString} fill={fill}/>,
                  <text x={(xPos.toInt + witW / 2).toString}
                        y={(witH / 2).toString}
                        text-anchor="middle"
                        dominant-baseline="central"
                        font-size={(witH / 2.5).toString}>
                    {siglum.drop(1)}
                  </text>)
              })
          val missingRects: Vector[Elem] =
            Sigla.values.filterNot(e => witnessReadings.keySet.contains(e.toString))
              .zipWithIndex
              .toVector
              .flatMap((siglum, offset) => {
                val xPos: String = ((offset + Sigla.values.size + 1) * witW).toString // include spacer
                val fill: String = WitnessColors(Sigla.withName(siglum.toString).id).toString
                Vector(<rect x={xPos} y="0" width={witW.toString} height={witH.toString} fill={fill}/>,
                  <text x={(xPos.toInt + witW / 2).toString}
                        y={(witH / 2).toString}
                        text-anchor="middle"
                        dominant-baseline="central"
                        font-size={(witH / 2.5).toString}>
                    {siglum.toString.drop(1)}
                  </text>)
              })
          readingRects :++ missingRects
        case _ => Vector(<rect></rect>)
      <g transform={translateInstruction}>
        {contents}
      </g>
    }}
    </g>
  </svg>

@main def testSvg(): Unit =
  val pp = new scala.xml.PrettyPrinter(78, 2)
  val x = pp.format(svg)
  println(x)
  save("svgTest.svg", svg)
