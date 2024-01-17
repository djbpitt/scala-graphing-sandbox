package net.collatex.reptilian

import scala.xml.Elem

def createSvgGridColumnCells(nodes: Vector[AlignmentPoint]): Vector[scala.xml.Elem] =
  val result = nodes.zipWithIndex map { (node, index) =>
    val nodeNo = (index + 1).toString // Output should be one-based
    <div id={"t" + nodeNo} style={"background-image: url('sprites.svg#b" + nodeNo + "');"}>
    </div>
  }
  result

def createFlowModelForGrid(root: ExpandedNode, tokenArray: Vector[Token]) =
  // For HTML output:
  // Create <svg> with groups of <text> element (no rectangles, but with underscore and background <rect>) for each subgroup
  // Create single-column alignment table for each alignment point
  // For sprites.svg
  // Create flows for each alignment point
  /* Setup */
  val nodeSequence = flattenNodeSeq(root)
  val alignmentPoints = createAlignmentPoints(nodeSequence, tokenArray)
  val gridColumnCells = createSvgGridColumnCells(alignmentPoints)
  gridColumnCells.foreach(println)


//  /* HTML grid output */
