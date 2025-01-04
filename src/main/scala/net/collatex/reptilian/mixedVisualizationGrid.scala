package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token

import scala.annotation.{tailrec, unused}
import scala.xml.{Elem, Node, NodeSeq}
import math.Ordered.orderingToOrdered

/* Constants */
val witnessToColor: Map[Siglum, String] = Map(
  Siglum("darwin1859.txt") -> "peru",
  Siglum("darwin1860.txt") -> "orange",
  Siglum("darwin1861.txt") -> "yellow",
  Siglum("darwin1866.txt") -> "limegreen",
  Siglum("darwin1869.txt") -> "dodgerblue",
  Siglum("darwin1872.txt") -> "violet"
)
val allSigla: Set[Siglum] =
  witnessToColor.keySet // TODO: Derive from nodes, but AlignmentUnit doesn't have a witnessReadings property
    /* End of constants*/

    /* ====================================================================== */
    /* Horizontal ribbons                                                     */
    /* ====================================================================== */

    /* Constants for plotting */
val flowLength = 80d

/* Constants for computeTokenTextLength() */
val tnr16Metrics = xml.XML.loadFile("src/main/python/tnr_16_metrics.xml")
val tnrCharLengths = ((tnr16Metrics \ "character")
  .map(e => ((e \ "@str").text.head, (e \ "@width").toString.toDouble))
  ++ Seq(("\u000a".head, 0.0))).toMap

/** memoizeFnc()
  *
  * https://medium.com/musings-on-functional-programming/scala-optimizing-expensive-functions-with-memoization-c05b781ae826
  *
  * @param f
  *   Single-parameter function to memoize
  * @return
  *   Memoized version of f
  */
def memoizeFnc[K, V](f: K => V): K => V = {
  val cache = collection.mutable.Map.empty[K, V]
  k =>
    cache.getOrElse(
      k, {
        cache update (k, f(k))
        cache(k)
      }
    )
}

/** computeTokenTextLength()
  *
  * Relies on global trnCharLengths: Map[Char, Double] Memoized and then called as memoizedComputeTokenTextLength()
  *
  * @param in
  *   Input string
  * @return
  *   Double
  */
def computeTokenTextLength(in: String): Double =
  val result = in.map(e => tnrCharLengths(e)).sum
  result

/** retrieveWitnessReadings()
  *
  * @param n
  *   Node with witness readings
  * @return
  *   All witness readings on node as map from siglum (String) to vector of tokens
  */
def retrieveWitnessReadings(n: AlignmentPoint, gTa: Vector[TokenEnum]): Map[Siglum, Vector[TokenEnum]] =
  val witnessReadings = n.combineWitnessGroups.map((k, v) => k -> gTa.slice(v.start, v.until))
  witnessReadings

val memoizedComputeTokenTextLength = memoizeFnc(computeTokenTextLength)

/** computeReadingTextLength()
  *
  * Sum of widths of t values of tokens (using memoizedComputeTokenTextLength() in reading)
  *
  * TODO: Include width of siglum followed by colon
  *
  * @param in
  *   Vector[Token] tokens in reading
  * @return
  *   Size of reading (sum of lengths of t values of tokens plus intertoken spaces)
  */
def computeReadingTextLength(in: Vector[TokenEnum]): Double =
  in.foldLeft(0d)((e, f) => e + memoizedComputeTokenTextLength(f.t))

/** findMissingWitnesses()
  *
  * Sigla of missing witnesses
  *
  * @param n
  *   Node with witness readings
  * @return
  *   Vector of sigla of missing witnesses as strings
  */
def findMissingWitnesses(n: AlignmentPoint, sigla: Set[Siglum]): Vector[Siglum] =
  val missingWitnesses = sigla.diff(n.combineWitnessGroups.keySet).toVector.sorted
  missingWitnesses

def computeAlignmentNodeRenderingWidth(n: AlignmentPoint, gTa: Vector[TokenEnum]): Double =
  // FIXME: Temporarily add 28 to allow for two-character siglum plus colon plus space
  val maxAlignmentPointWidth = 1000000000000d // 160.0
  List(
    retrieveWitnessReadings(n, gTa).values.map(computeReadingTextLength).max + 28,
    maxAlignmentPointWidth
  ).min

def createHorizNodeData(
    nodeSequence: Vector[NumberedNode],
    sigla: Set[Siglum]
)(using tokenArray: Vector[TokenEnum]): Vector[HorizNodeData] =
  @tailrec
  def nextNode(nodes: Vector[NumberedNode], pos: Int, acc: Vector[HorizNodeData]): Vector[HorizNodeData] =
    if nodes.isEmpty then acc
    else
      val nodeType = nodes.head.node.getClass.getSimpleName
      val alignmentWidth = computeAlignmentNodeRenderingWidth(nodes.head.node, tokenArray)
      val xOffset = acc.lastOption match {
        case Some(e) => e.xOffset + e.alignmentWidth + flowLength
        case None    => 0d
      }
      val missing = findMissingWitnesses(nodes.head.node, sigla)
      val newNode = HorizNodeData(
        treeNumber = nodes.head.nodeNo,
        seqNumber = pos,
        nodeType = nodeType,
        alignmentWidth = alignmentWidth,
        xOffset = xOffset,
        /* Create one HorizNodeGroup for each group, where HorizNodeGroupMember is a
         * map from siglum to reading as string. Sort both groups and within groups. */
        groups = nodes.head.node.witnessGroups
          .map(f =>
            HorizNodeGroup(
              f.map((k, v) =>
                HorizNodeGroupMember(
                  k,
                  v.tString
                )
              ).toVector
                .sorted
            )
          )
          .toVector
          .sorted,
        missing = missing
      )
      nextNode(nodes.tail, pos + 1, acc :+ newNode)
  nextNode(nodeSequence, 1, Vector.empty[HorizNodeData])

/** selfCartesianProduct
  *
  * Excludes self-pairs, that is, (a, b) x (a, c) excludes (a, a)
  *
  * @param input
  *   : Iterable of anything
  * @tparam A
  *   Type of content items of input iterable
  * @return
  *   Set of sets, representing Cartesian product of input with itself without self-pairings
  */
@unused
def selfCartesianProduct[A](input: Iterable[A]) =
  val result = input
    .flatMap(e =>
      input
        .map(f => Set(e, f))
    )
    .filter(_.size > 1)
  result

@unused
def computeWitnessSimilarities(inputs: Vector[Iterable[Set[String]]]) =
  def nextPair(pair: Set[String], acc: Map[Set[String], Int]): Map[Set[String], Int] =
    val newAcc = acc ++ Map(pair -> (acc.getOrElse(pair, 0) + 1))
    newAcc
  @tailrec
  def nextInput(ins: Vector[Iterable[Set[String]]], acc: Map[Set[String], Int]): Map[Set[String], Int] =
    if ins.isEmpty then acc
    else
      val newAcc: Map[Set[String], Int] = ins.head
        .foldLeft(acc)((x, y) => nextPair(y, x))
      nextInput(ins.tail, newAcc)
  nextInput(inputs, Map[Set[String], Int]())

/** createHorizontalRibbons()
  *
  * Entry point for creating horizontal alignment ribbon plot
  *
  * @param root
  *   Expanded node root of entire alignment tree
  * @param tokenArray
  *   Global token array
  * @return
  *   <html> element in HTML namespace, with embedded SVG
  */
def createHorizontalRibbons(root: AlignmentRibbon, sigla: Set[Siglum])(using
                                                                       tokenArray: Vector[TokenEnum]
): scala.xml.Node =
  /** Constants */
  val ribbonWidth = 18
  val missingTop = allSigla.size * ribbonWidth * 2 + ribbonWidth / 2
  val witnessCount = sigla.size
  val nodeSequence: Vector[NumberedNode] = flattenNodeSeq(root)
  val horizNodes = createHorizNodeData(nodeSequence, sigla)
  /* Compute optimal witness order; finds distances but not order; not updated for new witnessGroups */
  /*
  val cartesianProducts: Vector[Iterable[Set[String]]] = nodeSequence.map(_.node) map {
    case AgreementNode(witnessReadings, witnessGroups)      => selfCartesianProduct(witnessReadings.keys)
    case AgreementIndelNode(witnessReadings, witnessGroups) => selfCartesianProduct(witnessReadings.keys)
    case VariationNode(witnessReadings, witnessGroups)      => witnessGroups.flatMap(e => selfCartesianProduct(e)).toSet
    case VariationIndelNode(witnessReadings, witnessGroups) => witnessGroups.flatMap(e => selfCartesianProduct(e)).toSet
  }
  /* Compute witness distances */
  val witnessSimilarities: Map[Set[String], Int] = computeWitnessSimilarities(cartesianProducts)
  val maxDistance = witnessSimilarities.values.max
  /* Look at the results; the sorting is for legibility, and not otherwise needed */
  val keys = witnessSimilarities.keySet.toSeq.flatten.distinct.sorted // sorted list of sigla
  println(keys)
  val keyCount = keys.size
  val m = Array.ofDim[Double](keyCount, keyCount) // will hold distance matrix of witnesses
  val cells = for
    i <- keys.zipWithIndex
    j <- keys.zipWithIndex if j != i
  yield Set(i, j) // all witness pairs except self-pairs
  val results = cells.map(e =>
    val loc1 = e.map(_._2).toVector
    val loc2 = Vector(loc1.last, loc1.head)
    val weight = witnessSimilarities(e.map(_._1))
    (
      loc1,
      loc2,
      maxDistance + 1 - weight.toDouble
    ) // subtract from max + 1 to convert shared readings to distance (invert)
  )
  for r <- results yield { // populate array
    val loc1 = r.head
    val loc2 = r(1)
    val weight = r.last
    m(loc1.head)(loc1.last) = weight
    m(loc2.head)(loc2.last) = weight
  }
  println(DataFrame.of(m)) // take a look

  val tour0 = LinKernighan.createRandomTour(keys.size, seed = 0L)
  val lk = LinKernighan(m, tour0)
  lk.run()
  println(lk.tour.map(e => keys(e).mkString).toVector)

  // cells.map(e => witnessSimilarities(e)).foreach(println)
   */
  /* End of computing optimal witness order */
  val totalHeight = ribbonWidth * (witnessCount * 3) - ribbonWidth / 2

  val witnessToGradient: Map[Siglum, String] = Map(
    Siglum("darwin1859.txt") -> "url(#peruGradient)",
    Siglum("darwin1860.txt") -> "url(#orangeGradient)",
    Siglum("darwin1861.txt") -> "url(#yellowGradient)",
    Siglum("darwin1866.txt") -> "url(#limegreenGradient)",
    Siglum("darwin1869.txt") -> "url(#dodgerblueGradient)",
    Siglum("darwin1872.txt") -> "url(#violetGradient)"
  )

  def formatSiglum(siglum: Siglum): String = siglum.value.slice(8, 10).mkString

  def plotRect(vOffset: Double, node: HorizNodeData, fillColor: String, top: Double) =
    <rect x="0"
       y={(vOffset * ribbonWidth + top).toString}
       width={node.alignmentWidth.toString}
       height={ribbonWidth.toString}
       fill={fillColor}
    />
  def plotReading(vOffset: Int, node: HorizNodeData, top: Double, hngm: (HorizNodeGroupMember, Int)) =
    <foreignObject x="1"
                   y={(vOffset * ribbonWidth + top - 2).toString}
                   width={node.alignmentWidth.toString}
                   height={ribbonWidth.toString}>
      <div xmlns="http://www.w3.org/1999/xhtml"><span class="sigla">{
      s"${formatSiglum(hngm._1.siglum)}: "
    }</span>
        {hngm._1.reading}</div>
    </foreignObject>

  /** plotOneAlignmentPoint()
    *
    * Plot backgrounds and text for single alignment point
    *
    * @param node
    *   HorizNodeData for one node to plot
    * @return
    *   <g> Alignment point
    */
  def plotOneAlignmentPoint(node: HorizNodeData): xml.Elem =
    def processGroups(groups: Vector[HorizNodeGroup]): Vector[Elem] =
      @tailrec
      def nextGroup(groups: Vector[HorizNodeGroup], top: Double, acc: Vector[Elem]): Vector[Elem] =
        if groups.isEmpty then acc
        else
          val groupHeight = (groups.head.members.size + 1) * ribbonWidth // Include space below group
          val newG =
            <g>{
              for e <- groups.head.members.zipWithIndex yield
                val fillColor = witnessToColor(e._1.siglum)
                val vOffset = e._2
                Seq(plotRect(vOffset, node, fillColor, top), plotReading(vOffset, node, top, e))
            }</g>
          nextGroup(groups.tail, top + groupHeight, acc :+ newG)
      nextGroup(groups, 0, Vector.empty[Elem])
    val groups = processGroups(node.groups)
    /* Missing witnesses */
    val missing = for e <- node.missing.zipWithIndex yield
      val fillColor = witnessToColor(e._1)
      val vOffset = e._2
      val top = missingTop
      Seq(
        plotRect(vOffset, node, fillColor, top),
        <foreignObject x="1"
                       y={(vOffset * ribbonWidth + top - 2).toString}
                       width={node.alignmentWidth.toString}
                       height={ribbonWidth.toString}>
          <div xmlns="http://www.w3.org/1999/xhtml">
            <span class="sigla">{s"(${formatSiglum(e._1)})"}</span>
          </div>
        </foreignObject>
      )
    <div class="ap">
      <svg preserveAspectRatio="none" xmlns="http://www.w3.org/2000/svg"
           width={node.alignmentWidth.toString}
           height={(totalHeight + 2).toString}
           class="alignment">
      {groups}{missing}
      </svg>
    </div>

  /** plotLeadingRibbons()
    *
    * Plot ribbons from preceding alignment point to current one
    *
    * @param currentNode
    *   HorizNodeData
    * @param precedingNode
    *   HorizNodeData
    * @return
    *   <g> containing ribbon splines between points
    */
  def plotLeadingRibbons(currentNode: HorizNodeData, precedingNode: HorizNodeData): xml.Elem =
    /* Coordinates for witnesses in groups */
    @tailrec
    def nextGroup(groups: Vector[HorizNodeGroup], top: Double, acc: Map[Siglum, Double]): Map[Siglum, Double] =
      if groups.isEmpty then acc
      else
        val newAcc = acc ++ groups.head.members.zipWithIndex.map(e => e._1.siglum -> (top + e._2 * ribbonWidth)).toMap
        nextGroup(groups.tail, top + (groups.head.members.size + 1) * ribbonWidth, newAcc)
    def missings(missings: Vector[Siglum], top: Double): Map[Siglum, Double] =
      missings.zipWithIndex.map(e => e._1 -> (top + e._2 * ribbonWidth)).toMap
    def computeRibbonYPos(node: HorizNodeData): Map[Siglum, Double] =
      nextGroup(node.groups, ribbonWidth / 2, Map.empty[Siglum, Double]) ++
        missings(
          node.missing,
          missingTop + ribbonWidth / 2
        )

    val leftYPosMap = computeRibbonYPos(precedingNode)
    val rightYPosMap = computeRibbonYPos(currentNode)
    val ribbons = sigla.toSeq.sorted.map(e =>
      val leftYPos = leftYPosMap(e)
      val rightYPos = rightYPosMap(e) + 0.0001
      <path d={
        s"M 0,$leftYPos L 10,$leftYPos C 40,$leftYPos 40,$rightYPos 70,$rightYPos L 80,$rightYPos"
      }
            stroke={witnessToGradient(e)}
            stroke-width={ribbonWidth.toString}
            fill="none"/>
    )
    <div class="flow">
      <svg preserveAspectRatio="none" xmlns="http://www.w3.org/2000/svg" width="80" height={
      (totalHeight + 2).toString
    }>{
      ribbons
    }</svg>
    </div>

  /** plotAllAlignmentPointsAndRibbons()
    *
    * Calls plotOneAlignmentPoint() and (except for first) plotLeadingRibbons() for each alignment point
    *
    * @param nodes
    *   Vector[HorizNodeData] nodes to plot
    * @return
    *   Vector[Elem] <g> elements for all nodes and internode flows
    */
  def plotAllAlignmentPointsAndRibbons(nodes: Vector[HorizNodeData]) =
    val result = nodes
      .sliding(2)
      .flatMap(e =>
        <div class="group" data-maxwidth={e.head.alignmentWidth.toString}>{
          Vector(
            plotOneAlignmentPoint(e.head),
            plotLeadingRibbons(e.last, e.head),
            plotGroupNodeWrappers(e.head)
          )
        }</div>
      ) ++ <div>{
      Vector(
        plotOneAlignmentPoint(nodes.last),
        plotGroupNodeWrappers(nodes.last)
      )
    }</div>
    result

  def plotGroupNodeWrappers(node: HorizNodeData): Elem =
    def wrapGroups(groups: Vector[HorizNodeGroup]): Elem =
      @tailrec
      def nextGroup(groups: Vector[HorizNodeGroup], top: Double, acc: Vector[Elem]): Elem =
        if groups.isEmpty then
          if node.missing.isEmpty then
            <div class="innerWrapper">
            <svg preserveAspectRatio="none" xmlns="http://www.w3.org/2000/svg" width={
              (node.alignmentWidth + 2).toString
            } height={(totalHeight + 2).toString}>{acc}</svg>
          </div>
          else
            <div class="innerWrapper">
              <svg preserveAspectRatio="none" xmlns="http://www.w3.org/2000/svg"
                   width={(node.alignmentWidth + 2).toString}
                   height={(totalHeight + 2).toString}>{
              acc :+
                <rect x="1"
                      y={(missingTop + 1).toString}
                      width={node.alignmentWidth.toString}
                      height={(node.missing.size * ribbonWidth).toString}
                      fill="none"
                      stroke="black"
                      stroke-width="2"
                      rx="3"
                />
            }</svg></div>
        else
          val newAcc = acc :+ <rect x="1"
                y={(top + 1).toString}
                width={node.alignmentWidth.toString}
                height={(groups.head.members.size * ribbonWidth).toString}
                fill="none"
                stroke="black"
                stroke-width="2"
                rx="3"/>
          val newTop = top + (groups.head.members.size + 1) * ribbonWidth
          nextGroup(groups.tail, newTop, newAcc)
      nextGroup(groups, 0, Vector.empty[Elem])
    wrapGroups(node.groups)

  val contents = plotAllAlignmentPointsAndRibbons(horizNodes)
  /* Create gradient stops for each color, using colorname + "Gradient" as @id of <linearGradient> wrapper */
  val gradients =
    <defs>{
      val colors: Vector[String] = Vector("yellow", "dodgerblue", "violet", "orange", "peru", "limegreen")
      val stops: Vector[(Int, Double)] =
        Vector((0, 1), (6, 1), (20, .6), (35, .4), (50, .3), (65, .4), (80, .6), (94, 1), (100, 1))
      for c <- colors yield <linearGradient id={s"${c}Gradient"} x1="0%" x2="100%" y1="0%" y2="0%">{
        for s <- stops yield <stop offset={s"${s._1}%"} stop-color={s"$c"} stop-opacity={s"${s._2}"}/>
      }</linearGradient>
    }</defs>
  val css = s"""header {
               |  display: flex;
               |  flex-direction: row;
               |  justify-content: space-between;
               |}
               |#set_all {
               |  display: flex;
               |  gap: .5em;
               |  flex-direction: column;
               |  margin: auto 0 auto 0;
               |  padding: .1em .3em .2em .2em;
               |  font-size: .8rem;
               |}
               |#wrapper {
               |  padding: 2px;
               |  overflow-x: scroll;
               |  display: flex;
               |  flex-direction: row;
               |  height: ${totalHeight + 2 + 4}px;
               |  width: 100%;
               |  background: linear-gradient(
               |    to bottom,
               |    gainsboro 0,
               |    gainsboro ${missingTop - ribbonWidth / 2}px,
               |    gray ${missingTop - ribbonWidth / 2}px,
               |    gray 100%
               |  );
               |}
               |div {
               |  position: relative;
               |}
               |.group {
               |  display: flex;
               |  flex-direction: row;
               |}
               |.innerWrapper {
               |  position: absolute;
               |  display: block;
               |  top: -1px;
               |  left: -1px;
               |  height: ${totalHeight + 2}px;
               |}
               |.sigla {
               |  font-weight: bold;
               |  font-size: small;
               |}
               |foreignObject > div {
               |  white-space: nowrap;
               |  overflow: hidden;
               |  text-overflow: ellipsis;
               |  font-family: "Times New Roman"; /* Match font metrics */
               |  font-size: 16px; /* needed by Safari */
               |  padding: .1em .1em 0 .1em;
               |  line-height: ${ribbonWidth - 1}px;
               |}""".stripMargin
  val js = """"use strict";
             |document.addEventListener("DOMContentLoaded", function () {
             |  const groups = document.getElementsByClassName("group");
             |  for (var i = 0, len = groups.length; i < len; i++) {
             |    groups[i].addEventListener("click", toggleSize);
             |  }
             |  document.getElementById("expand_all").addEventListener("click", expandAll);
             |  document.getElementById("truncate_all").addEventListener("click", truncateAll);
             |})
             |function toggleSize() {
             |  var newWidth, newWrapperWidth, apTargets, innerWrapTargets, i, len;
             |  if (this.dataset.maxwidth > 160) {
             |    apTargets = this.querySelectorAll("rect, foreignObject, div.ap > svg.alignment");
             |    // Determine width
             |    if (apTargets[0].getAttribute("width") == this.dataset.maxwidth) {
             |      newWidth = 160;
             |    } else {
             |      newWidth = this.dataset.maxwidth;
             |    }
             |    // Reset width of alignment point groups
             |    for (i = 0, len = apTargets.length; i < len; i++) {
             |      toggleOne(apTargets[i], newWidth);
             |    }
             |    // Reset width of wrapper rects
             |    innerWrapTargets = this.querySelectorAll("div.innerWrapper > svg");
             |    if (newWidth == 160) {
             |      newWrapperWidth = 162;
             |    } else {
             |      newWrapperWidth = parseFloat(newWidth) + 2;
             |    }
             |    for (i = 0, len = innerWrapTargets.length; i < len; i++) {
             |      toggleOne(innerWrapTargets[i], newWrapperWidth);
             |    }
             |  }
             |}
             |function truncateAll() {
             |  var i, j, groupLen, aps, apsLen, wrappers, wrapperLen, fullWidth, wrapperWidth;
             |  const allGroups = document.getElementsByClassName("group");
             |  for (i = 0, groupLen = allGroups.length; i < groupLen; i++) {
             |    if (allGroups[i].dataset.maxwidth > 160) {
             |    aps = allGroups[i].querySelectorAll("rect, foreignObject, div.ap > svg.alignment");
             |    wrappers = allGroups[i].querySelectorAll("div.innerWrapper > svg");
             |      for (j = 0, apsLen = aps.length; j < apsLen; j++) {
             |        toggleOne(aps[j], 160);
             |      }
             |      for (j = 0, wrapperLen = wrappers.length; j < wrapperLen; j++) {
             |        toggleOne(wrappers[j], 162);
             |      }
             |    }
             |  }
             |}
             |function expandAll() {
             |  var i, j, groupLen, aps, apsLen, wrappers, wrapperLen, fullWidth, wrapperWidth;
             |  const allGroups = document.getElementsByClassName("group");
             |  for (i = 0, groupLen = allGroups.length; i < groupLen; i++) {
             |    fullWidth = allGroups[i].dataset.maxwidth;
             |    if (fullWidth > 160) {
             |      wrapperWidth = parseFloat(fullWidth) + 2;
             |      aps = allGroups[i].querySelectorAll("rect, foreignObject, div.ap > svg.alignment");
             |      wrappers = allGroups[i].querySelectorAll("div.innerWrapper > svg");
             |      for (j = 0, apsLen = aps.length; j < apsLen; j++) {
             |        toggleOne(aps[j], fullWidth);
             |      }
             |      for (j = 0, wrapperLen = wrappers.length; j < wrapperLen; j++) {
             |        toggleOne(wrappers[j], wrapperWidth);
             |      }
             |    }
             |  }
             |}
             |function toggleOne(target, width) {
             |  target.setAttribute("width", width);
             |}""".stripMargin
  val html =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Alignments</title>
        <style type="text/css">{css}</style>
        <script type="text/javascript">{js}</script>
      </head>
      <body>
        <header>
          <h1>Alignments</h1>
          <div id="set_all">
            <button id="expand_all" type="button">Expand all</button>
            <button id="truncate_all" type="button">Truncate all</button>
          </div>
        </header>
        <main>
          <div id="wrapper">
            <svg xmlns="http://www.w3.org/2000/svg"
                 width="0"
                 height="0"
                 preserveAspectRatio="none">
              {gradients}
            </svg>{contents}
          </div>
        </main>
      </body>
    </html>
  html

case class HorizNodeData(
    treeNumber: Int,
    seqNumber: Int,
    nodeType: String,
    alignmentWidth: Double,
    xOffset: Double,
    groups: Vector[HorizNodeGroup],
    missing: Vector[Siglum]
)

case class HorizNodeGroup(members: Vector[HorizNodeGroupMember])
object HorizNodeGroup {
  given Ordering[HorizNodeGroup] =
    (a: HorizNodeGroup, b: HorizNodeGroup) => a.members.head.compare(b.members.head)
}

case class HorizNodeGroupMember(siglum: Siglum, reading: String)
object HorizNodeGroupMember {
  given Ordering[HorizNodeGroupMember] =
    (a: HorizNodeGroupMember, b: HorizNodeGroupMember) => a.siglum.compare(b.siglum)
}
