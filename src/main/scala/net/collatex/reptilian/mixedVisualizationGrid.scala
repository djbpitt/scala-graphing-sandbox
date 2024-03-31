package net.collatex.reptilian

import scala.annotation.tailrec
import scala.xml.Elem

/** Create inner <g> elements with <text> only (no <rect>)
  *
  * @param input
  *   AlignmentPoint
  * @return
  *   <g> element
  */
private def createInnerGridGs(input: AlignmentPoint): Vector[Elem] =
  @tailrec
  def nextGroup( // recursively process all non-empty subgrouops
      groupsToProcess: Vector[SubGroup],
      groupCount: Int,
      cumWitnessCount: Int,
      acc: Vector[Elem]
  ): Vector[Elem] =
    if groupsToProcess.isEmpty then acc
    else
      val currentGroup = groupsToProcess.head
      val subGroupXPos =
        ((cumWitnessCount + groupCount) * witDims("w")).toString
      val newG = <g transform={"translate(" + subGroupXPos + ")"}>{
        createWitnessTexts(currentGroup)
      }</g>
      val newCumWitnessCount = cumWitnessCount + currentGroup.witnesses.size
      val newGroupCount = groupCount + 1
      nextGroup(
        groupsToProcess.tail,
        newGroupCount,
        newCumWitnessCount,
        acc :+ newG
      )

  val groupTexts = nextGroup(
    input.subGroups,
    0,
    0,
    Vector.empty
  ) // start of subgroup processing

  if input.missingGroup.nonEmpty
  then // process group of missing witnesses if present
    val missingTexts =
      <g transform={
        "translate(" + (verticalRuleXPos - (witDims("w") / 2)).toString + ")"
      }>
        {
        input.missingGroup.zipWithIndex.map((reading, offset) => plotText(reading, offset))
      }
      <line x1="0" y1="5" x2={
        (witDims("w") * input.missingGroup.size).toString
      } y2="5" stroke="black" stroke-width=".5"/>
      </g>
    groupTexts :+ missingTexts
  else groupTexts

/** Output <text> with witness sigla for a single subgroup of readigs
  *
  * @param group
  *   single subgroup of readings in alignment point
  * @return
  *   vector of one <text> per witness plus underscore <line>
  */
private def createWitnessTexts(group: SubGroup): Vector[Elem] =
  val texts = group.witnesses.zipWithIndex.map { (reading, offset) =>
    plotText(reading, offset)
  }
  val line = <line x1="0" y1="5" x2={
    (witDims("w") * group.size).toString
  } y2="5" stroke="black" stroke-width=".5"/>
  texts :+ line

/** Plot one <text>
  *
  * Called for both groups of readings and option group of missing witnesses
  *
  * @param reading
  *   WitnessReading, which contains siglum
  * @param offset
  *   x offset within grouop
  * @return
  *   vector of one <text>
  */
private def plotText(
    reading: WitnessReading,
    offset: Int
): Elem =
  val textXPos = offset * witDims("w")
  val text =
    <text
    x={(textXPos + witDims("w") / 2).toString}
    y={(witDims("h") / 2 - 1).toString}
    text-anchor="middle"
    font-size={(witDims("w") * .7).toString}>{
      reading.siglum.slice(8, 10)
    }</text>
  text

/** Create vector of all svg column cells
  *
  * @param nodes
  *   vector of AlignmentPoint instances
  * @return
  *   vector of <svg> elements
  */
private def createSvgGridColumnCells(
    nodes: Vector[AlignmentPoint]
): Vector[scala.xml.Elem] =
  val result = nodes.zipWithIndex map { (node, index) =>
    val nodeNo = (index + 1).toString // Output is one-based
    val innerGs = createInnerGridGs(node)
    <div id={"t" + nodeNo} style={
      "background-image: url('mixed-output-grid-backgrounds.svg#b" + nodeNo + "');"
    }>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 40">
        <g id={"v" + nodeNo}>{innerGs}</g>
      </svg>
    </div>
  }
  result

/** Create vector of all text column cells
  *
  * agreement and agreementIndel nodes are one-item lists, but nonetheless conceptually lists, and therefore tagged as
  * <ul> with single <li>
  *
  * <div> wrappers are added later
  *
  * @param nodes
  *   vector of NumberedNode instances
  * @param tokenArray
  *   vector of Token instances
  * @return
  *   vector of <ul> elements
  */
private def createTextGridColumnCells(
    nodes: Vector[NumberedNode],
    tokenArray: Vector[Token]
) =
  val result =
    nodes.map { e =>
      val rowContent = e.node match {
        case AgreementNode(witnessReadings) =>
          val (_, value) = witnessReadings.head
          val text = tokenArray
            .slice(value._1, value._2)
            .map(_.n)
            .mkString(" ")
          <ul><li><span class="sigla">all:</span> {text}</li></ul>
        case AgreementIndelNode(witnessReadings) =>
          val sigla = witnessReadings.keys
            .map(_.slice(8, 10))
            .toVector
            .sorted
            .mkString(" ")
          val (_, value) = witnessReadings.head
          val text = tokenArray
            .slice(value._1, value._2)
            .map(_.n)
            .mkString(" ")
          <ul><li><span class="sigla">{sigla}:</span> {text}</li></ul>
        case VariationNode(witnessReadings, witnessGroups) =>
          val readings = witnessGroups map { e =>
            val sigla = e.map(_.slice(8, 10)).sorted.mkString(" ")
            val start = witnessReadings(e.head)._1
            val end = witnessReadings(e.head)._2
            val text = tokenArray
              .slice(start, end)
              .map(_.n)
              .mkString(" ")
            <li><span class="sigla">{sigla}:</span> {text}</li>
          }
          <ul>{readings}</ul>
        case VariationIndelNode(witnessReadings, witnessGroups) =>
          val readings = witnessGroups map { e =>
            val sigla = e.map(_.slice(8, 10)).sorted.mkString(" ")
            val start = witnessReadings(e.head)._1
            val end = witnessReadings(e.head)._2
            val text = tokenArray
              .slice(start, end)
              .map(_.n)
              .mkString(" ")
            <li><span class="sigla">{sigla}:</span> {text}</li>
          }
          <ul>{readings}</ul>
      }
      rowContent
    }
  result

/** Create vector of alignment-node types, converted from PascalCase to camelCase, used as HTML @class value on row
  * <div>
  *
  * @param nodes
  *   vector of NumberedNode instances
  * @return
  *   vector of strings, representing alignment subtype
  */
private def getGridRowClasses(nodes: Vector[NumberedNode]): Vector[String] =
  nodes
    .map(_.node.getClass.toString.split("\\.").last.dropRight(4))
    .map(e => s"${e.head.toLower}${e.tail}")

private def createGridBackgroundFlows(
    nodes: Vector[AlignmentPoint]
) =
  val cellWidth = totalWitnessCount * 3 * witDims("w")
  val cellHeight = verticalNodeSpacing * witDims("h")
  val backgroundRect =
    <rect x="0" y="0" width={cellWidth.toString} height={
      cellHeight.toString
    } fill="gainsboro"/>
  def absoluteXPos(a: AlignmentPoint, w: WitnessReading): Double =
    if a.missingGroup.contains(w) then
      verticalRuleXPos + witDims("w") / 2 + a.missingGroup.indexOf(w) * witDims(
        "w"
      )
    else
      val g: SubGroup =
        a.subGroups.filter(_.witnesses.contains(w)).head // must be exactly one
      val offsetInGroup: Int = g.witnesses.indexOf(w)
      val groupOffset: Int = a.subGroups.indexOf(g)
      witDims("w") * (a.subGroups
        .slice(0, groupOffset)
        .map(_.size + 1)
        .sum // add one to each for spacer
        + offsetInGroup)

  val handleOffset = verticalNodeSpacing / 2
  val alignmentPointPairs = nodes.zip(nodes.tail) // pairs of alignment points
  val lastPath = <g id={s"b${alignmentPointPairs.size + 1}"}>
    <rect x="0" y="0" width={cellWidth.toString} height={
    cellHeight.toString
  } fill="gainsboro"/>
  </g>
  val allPaths = alignmentPointPairs.zipWithIndex flatMap { e =>
    val sourceY = 0
    val targetY = verticalNodeSpacing + witDims("h")
    val pathsForPair: Vector[Elem] = allSigla.map { f =>
      val color = s"url(#${witnessToColor(f)}Gradient)"
      val sourceX = absoluteXPos(e._1._1, WitnessReading(f)) + 3
      val targetX = absoluteXPos(e._1._2, WitnessReading(f)) + 3.0001
      val d =
        s"M $sourceX,$sourceY C $sourceX,${sourceY + handleOffset} $targetX,${targetY - handleOffset} $targetX,$targetY"
      <path d={d} stroke={color} stroke-width={
        witDims("w").toString
      } vector-effect="non-scaling-stroke" fill="none"/>
    }.toVector
    <g id={s"b${e._2 + 1}"}>
      {backgroundRect}{pathsForPair}
    </g>
  }

  allPaths :+ lastPath

/** Entry point to create html (main page) and svg (backgrounds sprites) for grid-based continuous flow visualization
  *
  * Depends on static css authored separately
  *
  * @param root
  *   root of alignment tree, which is then immediately flattened
  * @param tokenArray
  *   array of Token instances
  * @return
  *   tuple of html main document and svg document with background sprites
  */
def createFlowModelForGrid(root: ExpandedNode, tokenArray: Vector[Token]) =
  /*
   * Setup
   * */
  val nodeSequence: Vector[NumberedNode] = flattenNodeSeq(root)
  val alignmentPoints: Vector[AlignmentPoint] =
    createAlignmentPoints(nodeSequence, tokenArray)
  /*
   * Create grid content (one row per alignment point)
   * */
  val gridRowClasses: Vector[String] = getGridRowClasses(nodeSequence)
  val gridColumnCellsSvg: Vector[Elem] = createSvgGridColumnCells(
    alignmentPoints
  ) // <div>
  val gridColumnNodeNos = (1 to alignmentPoints.size).toVector
  val gridColumnCellsText: Vector[Elem] =
    createTextGridColumnCells(nodeSequence, tokenArray) // <td>
  val gridContent = gridRowClasses.indices map { e =>
    val c = gridRowClasses(e) // "class" is a reserved word
    val svg = gridColumnCellsSvg(
      e
    ) // already wrapped in <div> because needs background pointer
    val nodeNo = gridColumnNodeNos(e)
    val text = gridColumnCellsText(e)
    <div class={c}>
      {svg}
      <div><ul><li>{nodeNo}</li></ul></div>
      <div>{text}</div>
    </div>
  }
  /*
   * HTML grid output
   * */
  val html =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Alignments</title>
        <link rel="stylesheet" type="text/css" href="mixed-output-grid.css"/>
      </head>
      <body>
        <h1>Alignments</h1>
        <div>
          <div>
            <div>Flow</div>
            <div>Node</div>
            <div>Text</div>
          </div>
          {gridContent}
        </div>
      </body>
    </html>
  /*
   * Background sprites
   * */
  val linearGradientDefs = witnessToColor.values.map(createSingleColorGradient)
  val spritesContent = createGridBackgroundFlows(alignmentPoints)
  val spritesPage =
    <svg xmlns="http://www.w3.org/2000/svg"  height="100%" viewBox={
      s"0 0 100 40"
    } preserveAspectRatio="none">
      <defs>
        <style type="text/css">
          g {{ display: none; }}
          g:target {{ display: inline; }}
        </style>
        {linearGradientDefs}</defs>
      {spritesContent}
    </svg>
  /*
   * Return tuple of html main page and svg background sprites
   */
  (html, spritesPage)

private def createNonspriteSvgGridColumnCells(
    nodes: Vector[AlignmentPoint]
): Vector[scala.xml.Elem] =
  val result = nodes.zipWithIndex map { (node, index) =>
    val nodeNo = (index + 1).toString // Output is one-based
    val innerGs = createInnerGridGs(node)
    <div id={"t" + nodeNo}>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 40"
           preserveAspectRatio="none">
        <rect x="0" y="0" width="108.0" height="300.0" fill="gainsboro"/>
        <rect x="69" y="0" width="38.0" height="300.0" fill="gray"/>
        <line x1="69" y1="0" x2="69" y2="40" stroke="black" stroke-width=".5"/>
      </svg>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 40">
        <g id={"v" + nodeNo}>
          {innerGs}
        </g>
      </svg>
    </div>
  }
  result

def createNonspriteGrid(root: ExpandedNode, tokenArray: Vector[Token]): scala.xml.Node =
  /*
   * Setup
   * */
  val nodeSequence: Vector[NumberedNode] = flattenNodeSeq(root)
  val alignmentPoints: Vector[AlignmentPoint] =
    createAlignmentPoints(nodeSequence, tokenArray)
  /*
   * Create grid content (one row per alignment point)
   * */
  val gridRowClasses: Vector[String] = getGridRowClasses(nodeSequence)
  val gridColumnCellsSvg: Vector[Elem] = createNonspriteSvgGridColumnCells(
    alignmentPoints
  ) // <div>
  val gridColumnNodeNos = (1 to alignmentPoints.size).toVector
  val gridColumnCellsText: Vector[Elem] =
    createTextGridColumnCells(nodeSequence, tokenArray) // <td>
  val gridContent = gridRowClasses.indices map { e =>
    val c = gridRowClasses(e) // "class" is a reserved word
    val svg = gridColumnCellsSvg(
      e
    ) // already wrapped in <div> because needs background pointer
    val nodeNo = gridColumnNodeNos(e)
    val text = gridColumnCellsText(e)
    <div class={c}>
      {svg}
      <div><ul><li>{nodeNo}</li></ul></div>
      <div>{text}</div>
    </div>
  }
  /*
   * HTML grid output
   * */
  val gradients =
    <svg xmlns="http://www.w3.org/2000/svg">
      <defs>
        <linearGradient id="yellowGradient" x1="0%" x2="0%" y1="0%" y2="100%">
          <stop offset="0%" stop-color="yellow" stop-opacity="1"/>
          <stop offset="6%" stop-color="yellow" stop-opacity="1"/>
          <stop offset="20%" stop-color="yellow" stop-opacity=".6"/>
          <stop offset="35%" stop-color="yellow" stop-opacity=".4"/>
          <stop offset="50%" stop-color="yellow" stop-opacity=".3"/>
          <stop offset="65%" stop-color="yellow" stop-opacity=".4"/>
          <stop offset="80%" stop-color="yellow" stop-opacity=".6"/>
          <stop offset="94%" stop-color="yellow" stop-opacity="1"/>
          <stop offset="100%" stop-color="yellow" stop-opacity="1"/>
        </linearGradient>
        <linearGradient id="dodgerblueGradient" x1="0%" x2="0%" y1="0%" y2="100%">
          <stop offset="0%" stop-color="dodgerblue" stop-opacity="1"/>
          <stop offset="6%" stop-color="dodgerblue" stop-opacity="1"/>
          <stop offset="20%" stop-color="dodgerblue" stop-opacity=".6"/>
          <stop offset="35%" stop-color="dodgerblue" stop-opacity=".4"/>
          <stop offset="50%" stop-color="dodgerblue" stop-opacity=".3"/>
          <stop offset="65%" stop-color="dodgerblue" stop-opacity=".4"/>
          <stop offset="80%" stop-color="dodgerblue" stop-opacity=".6"/>
          <stop offset="94%" stop-color="dodgerblue" stop-opacity="1"/>
          <stop offset="100%" stop-color="dodgerblue" stop-opacity="1"/>
        </linearGradient>
        <linearGradient id="violetGradient" x1="0%" x2="0%" y1="0%" y2="100%">
          <stop offset="0%" stop-color="violet" stop-opacity="1"/>
          <stop offset="6%" stop-color="violet" stop-opacity="1"/>
          <stop offset="20%" stop-color="violet" stop-opacity=".6"/>
          <stop offset="35%" stop-color="violet" stop-opacity=".4"/>
          <stop offset="50%" stop-color="violet" stop-opacity=".3"/>
          <stop offset="65%" stop-color="violet" stop-opacity=".4"/>
          <stop offset="80%" stop-color="violet" stop-opacity=".6"/>
          <stop offset="94%" stop-color="violet" stop-opacity="1"/>
          <stop offset="100%" stop-color="violet" stop-opacity="1"/>
        </linearGradient>
        <linearGradient id="orangeGradient" x1="0%" x2="0%" y1="0%" y2="100%">
          <stop offset="0%" stop-color="orange" stop-opacity="1"/>
          <stop offset="6%" stop-color="orange" stop-opacity="1"/>
          <stop offset="20%" stop-color="orange" stop-opacity=".6"/>
          <stop offset="35%" stop-color="orange" stop-opacity=".4"/>
          <stop offset="50%" stop-color="orange" stop-opacity=".3"/>
          <stop offset="65%" stop-color="orange" stop-opacity=".4"/>
          <stop offset="80%" stop-color="orange" stop-opacity=".6"/>
          <stop offset="94%" stop-color="orange" stop-opacity="1"/>
          <stop offset="100%" stop-color="orange" stop-opacity="1"/>
        </linearGradient>
        <linearGradient id="peruGradient" x1="0%" x2="0%" y1="0%" y2="100%">
          <stop offset="0%" stop-color="peru" stop-opacity="1"/>
          <stop offset="6%" stop-color="peru" stop-opacity="1"/>
          <stop offset="20%" stop-color="peru" stop-opacity=".6"/>
          <stop offset="35%" stop-color="peru" stop-opacity=".4"/>
          <stop offset="50%" stop-color="peru" stop-opacity=".3"/>
          <stop offset="65%" stop-color="peru" stop-opacity=".4"/>
          <stop offset="80%" stop-color="peru" stop-opacity=".6"/>
          <stop offset="94%" stop-color="peru" stop-opacity="1"/>
          <stop offset="100%" stop-color="peru" stop-opacity="1"/>
        </linearGradient>
        <linearGradient id="limegreenGradient" x1="0%" x2="0%" y1="0%" y2="100%">
          <stop offset="0%" stop-color="limegreen" stop-opacity="1"/>
          <stop offset="6%" stop-color="limegreen" stop-opacity="1"/>
          <stop offset="20%" stop-color="limegreen" stop-opacity=".6"/>
          <stop offset="35%" stop-color="limegreen" stop-opacity=".4"/>
          <stop offset="50%" stop-color="limegreen" stop-opacity=".3"/>
          <stop offset="65%" stop-color="limegreen" stop-opacity=".4"/>
          <stop offset="80%" stop-color="limegreen" stop-opacity=".6"/>
          <stop offset="94%" stop-color="limegreen" stop-opacity="1"/>
          <stop offset="100%" stop-color="limegreen" stop-opacity="1"/>
        </linearGradient>
      </defs>
    </svg>
  val html =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Alignments</title>
        <link rel="stylesheet" type="text/css" href="nonsprite-grid.css"/>
      </head>
      <body>
        <h1>Alignments</h1>
        {gradients}
        <main>
          <div>
            <div>Flow</div>
            <div>Node</div>
            <div>Text</div>
          </div>
          {gridContent}
        </main>
      </body>
    </html>
  /*
   * Return html main page
   */
  html
