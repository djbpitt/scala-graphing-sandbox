package net.collatex.util

import scala.xml.Elem
import scala.xml.XML.save
import annotation.tailrec
import scala.jdk.CollectionConverters.*
import net.collatex.reptilian.{AlignmentTreeNode, IndelNode, ReadingNode, VariationNode}

val witnessToColor: Map[String, String] = Map(
  "w59" -> "peru",
  "w60" -> "orange",
  "w61" -> "yellow",
  "w66" -> "limegreen",
  "w69" -> "dodgerblue",
  "w72" -> "violet"
)

/* Fake data for testing / demo
*
* Includes fake token array (below) because witness readings are grouped, which will matter for variation nodes */
val nodes: Vector[AlignmentTreeNode] = Vector(
  ReadingNode(witnessReadings = Map("w59" -> (0, 1), "w60" -> (1, 2), "w61" -> (2, 3), "w66" -> (3, 4), "w69" -> (4, 5), "w72" -> (5, 6))),
  IndelNode(witnessReadings = Map("w66" -> (6, 7), "w69" -> (7, 8), "w72" -> (8, 9))),
  ReadingNode(witnessReadings = Map("w59" -> (9, 10), "w60" -> (10, 11), "w61" -> (11, 12), "w66" -> (12, 13), "w69" -> (13, 14), "w72" -> (14, 15)))
)

// Fake token array enforcing shared raedings for reading and indel nodes
val tokenArray: Vector[String] = Vector("a", "a", "a", "a", "a", "a", "b", "b", "b", "c", "c", "c", "c", "c", "c")

/** Process single group of shared readings
 *
 * Reading nodes have one such group
 * Indel nodes have two, one of which is a shared absence of readings
 * Variation nodes have a variable number
 *
 * Cells within the group are positioned automatically from a starting offset supplied on invocation:
 *
 * For reacing nodes: start at 0
 * For indel nodes: reading start at 0, absence of readings starts beyond vertical dividing line
 * For variation nodes: spacing is handled when function is invoked, inserted spacers between groups
 *
 * TODO: Process all groups at once (except, perhaps, absence-of-reading groups for indel  nodes) so that
 *   intergroup spacing can be automated
 *
 * @param rdgGrp vector of strings representing sigla
 * @param pos offset of reading within group
 * @param witDims map with width and height of cells, used for positioning
 * @return vector of <rect> and <text> elements, which is flatmapped by caller
 */
def processReadingGroup(rdgGrp: Vector[String], pos: Int, witDims: Map[String, Int]): Vector[Elem] =

  @tailrec
  def nextRdg(rdgs: Vector[String], pos: Int, acc: Vector[Elem]): Vector[Elem] =
    if rdgs.isEmpty then acc
    else {
      val currentSiglum: String = rdgs.head
      val xPos: String = (pos * witDims("w")).toString
      val fill: String = witnessToColor(currentSiglum)
      val newNodes: Vector[Elem] = Vector(
          <rect x={xPos} y="0" width={witDims("w").toString} height={witDims("h").toString} fill={fill}/>,
        <text
        x={(xPos.toInt + witDims("w") / 2).toString}
        y={(witDims("h") / 2).toString}
        text-anchor="middle"
        dominant-baseline="central"
        font-size={(witDims("w") * .7).toString}>
          {currentSiglum.drop(1)}
        </text>)
      nextRdg(rdgs.tail, pos + 1, acc :++ newNodes)
    }

  nextRdg(rdgGrp, pos, Vector.empty) // start at supplied offset position


/* Create SVG for output
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
  val witDims: Map[String, Int] = Map("w" -> 6, "h" -> 10)
  val verticalNodeSpacing = 3 * witDims("h") // height of node plus twice height of node for sigmoid connectors
  val allSigla: Set[String] = witnessToColor.keySet // TODO: Derive from nodes, but AlignmentTreeNode doesn't have a witnessReadings property
  val totalWitCount: Int = allSigla.size
  /* End of constants*/
  /* Vertical line between present and absent witnesses first */
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 180">
    <g transform="translate(10)">
      <line
      transform={"translate(0, -" + ((verticalNodeSpacing - witDims("h")) / 2).toString + ")"}
      x1={(totalWitCount * witDims("w") + witDims("w") / 2).toString}
      y1="0"
      x2={(totalWitCount * witDims("w") + witDims("w") / 2).toString}
      y2={(nodes.size * verticalNodeSpacing).toString}
      stroke="gray"/>{nodes
      .zipWithIndex
      .map { (n, i) =>
        val translateInstruction = "translate(0, " + (i * verticalNodeSpacing).toString + ")"
        val contents: Vector[Elem] = n match
          case ReadingNode(witnessReadings) =>
            val readingGroups: Vector[Vector[String]] = witnessReadings // vector of vectors of sigla
              .groupBy((_, offsets) => tokenArray.slice(offsets._1, offsets._2)) // groupo by same reading text
              .map((text, attestations) => attestations.keys.toVector) // keep only sigla
              .toVector
            val rects: Vector[Elem] = readingGroups.flatMap(e => processReadingGroup(e.sorted, 0, witDims))
            rects
          case IndelNode(witnessReadings) =>
            val readingRects: Vector[Elem] =
              witnessReadings.keys.toSeq.sorted
                .zipWithIndex
                .toVector
                .flatMap((siglum, offset) => {
                  val xPos: String = (offset * witDims("w")).toString
                  val fill: String = witnessToColor(siglum)
                  Vector(<rect x={xPos} y="0" width={witDims("w").toString} height={witDims("h").toString} fill={fill}/>,
                    <text x={(xPos.toInt + witDims("w") / 2).toString}
                          y={(witDims("h") / 2).toString}
                          text-anchor="middle"
                          dominant-baseline="central"
                          font-size={(witDims("w") / 2.5).toString}>
                      {siglum.drop(1)}
                    </text>)
                })
            val missingRects: Vector[Elem] =
              allSigla.diff(witnessReadings.keySet).toSeq.sorted
                .zipWithIndex
                .toVector
                .flatMap((siglum, offset) => {
                  val xPos: String = ((offset + totalWitCount + 1) * witDims("w")).toString // include spacer
                  val fill: String = witnessToColor(siglum)
                  Vector(<rect x={xPos} y="0" width={witDims("w").toString} height={witDims("h").toString} fill={fill}/>,
                    <text x={(xPos.toInt + witDims("w") / 2).toString}
                          y={(witDims("h") / 2).toString}
                          text-anchor="middle"
                          dominant-baseline="central"
                          font-size={(witDims("h") / 2.5).toString}>
                      {siglum.drop(1)}
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
