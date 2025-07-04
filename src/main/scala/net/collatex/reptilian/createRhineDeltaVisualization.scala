package net.collatex.reptilian

import scala.annotation.tailrec
import scala.sys.process.*
import java.io.*

/** Create all nodes except Start and End as NodeProperties instances
  *
  * @param ar
  *   Alignment ribbon, same as for other visualizations.
  * @return
  *   Vector of NodeProperties instances
  */
def createNodes(ar: AlignmentRibbon): Vector[NodeProperties] = // Start and End are created elsewhere
  val nodeInfos: Vector[NodeProperties] =
    ar.children.zipWithIndex.flatMap { case (data, apId) =>
      data
        .asInstanceOf[AlignmentPoint]
        .witnessGroups
        .zipWithIndex
        .map((wr, gId) => NodeProperties(List(apId, ".", gId).mkString, wr.keySet, wr.head._2.tString))
    }.toVector
  nodeInfos

/** Create edges as a vector of EdgeProperties instances
  *
  * @param nodes
  *   Vector of NodeProperties instances; excludes Start and End
  * @param start
  *   Start (and End; both NodeProperties instances) are passed in separately because they would require special
  *   handling if included in the recursion. They are passed in, rather than created here, because they are used
  *   elsewhere, as well.
  * @param end
  *   See documentation of start, above.
  * @param displaySigla
  *   Used here only for its size to create seed rightmost for all witnesses. WitId (Int) values in AlignmentRibbon are
  *   replaced by user-supplied displaySigla (Siglum) values elsehwere.
  * @return
  *   Vector of EdgeProperties instances
  */
def createEdges(
    nodes: Vector[NodeProperties],
    start: NodeProperties,
    end: NodeProperties,
    displaySigla: List[Siglum]
): Vector[EdgeProperties] =
  @tailrec
  def nextNode(
      rgs: Vector[NodeProperties],
      rightmost: Map[WitId, NodeProperties],
      acc: Vector[EdgeProperties]
  ): Vector[EdgeProperties] =
    if rgs.isEmpty then {
      acc
    } else {
      val newNode = rgs.head
      val rightmostChanges = rgs.head.witnesses.map(k => k -> newNode).toSeq // Update rightMost map
      val newRightmost: Map[WitId, NodeProperties] = rightmost ++ rightmostChanges
      // To create edges:
      // 1. Retrieve rightmost nodes for all witnesses in new node
      // 1. Use sourceEdgeGroups to find witnesses associated with each source (for labeling)
      // 1. Retain only witness identifiers that are present in both source and target
      val newEdges = newNode.witnesses // all witnesses on newNode
        .map(e => e -> rightmost(e))
        .toMap // sources for new edges (may include duplicates)
        .groupMap(_._2)(_._1)
        .map { case (k, v) => k -> v.toSet }
        .map((k, v) => EdgeProperties(k.gId, newNode.gId, v.toSeq.sorted))
      val newAcc: Vector[EdgeProperties] = acc ++ newEdges
      nextNode(rgs.tail, newRightmost, newAcc)
    }
  val acc = nextNode(nodes :+ end, displaySigla.indices.map(e => e -> start).toMap, Vector())
  acc

/** Create Graphviz dot file from NodeProperties and EdgeProperties data
  *
  * @param nodes
  *   Vector of NodeProperties instances. Includes Start and End.
  * @param edges
  *   Vector of EdgeProperties instances
  * @param displaySigla
  *   User-supplied values (Siglum). EdgeProperties instances include WitId values; these are remapped to Siglum values
  *   in this function with edge labels are created.
  * @return
  *   Graphviz dot file as a String
  */
def createDot(
    nodes: Vector[NodeProperties],
    edges: Vector[EdgeProperties],
    displaySigla: List[Siglum]
): String =
  val dotStart = "digraph G {"
  val dotEnd = "}"
  val nodeLines =
    nodes
      .map(e =>
        val cleanedContent = e.content.replace("\"", "\\\"") // Escape quote (only?)
        List("  ", e.gId, " [label=\"", cleanedContent, "\"]").mkString
      )
  val edgeLines =
    edges
      .map(e =>
        val edgeLabel = Seq(
          " [label=\"",
          e.witnesses.map(f => displaySigla(f)).mkString(", "),
          "\"]"
        ).mkString
        List("  ", e.source, " -> ", e.target, edgeLabel).mkString
      )
  val dotBody = (nodeLines ++ edgeLines).mkString(";\n")
  List(dotStart, dotBody, dotEnd).mkString("\n")

/** Spawn process to transform Graphviz dot file to SVG (as String)
  *
  * Graphviz dot must be installed and either on system path or specified as a `GRAPHVIZ_DOT` environemnt variable
  *
  * The SVG is created as String, rather than an XML-related datatype, because it’s created by Graphviz dot as a String.
  *
  * @param dotFile
  *   Graphviz dot file as String
  * @return
  *   Rhine delta SVG visualization as String
  */
def createSvg(dotFile: String): Either[String, String] =
  val dotExecutable = sys.env.getOrElse(
    "GRAPHVIZ_DOT", // Use environment variable if set
    "dot" // Otherwise expect to find dot on system path
  )
  val process = Process(Seq(dotExecutable, "-Tsvg"))
  val output = new StringBuilder
  val error = new StringBuilder
  val io = new ProcessIO(
    in => {
      val writer = new PrintWriter(in)
      writer.write(dotFile)
      writer.close()
    },
    out => {
      val src = scala.io.Source.fromInputStream(out)
      output.appendAll(src.mkString)
      src.close()
    },
    err => {
      val src = scala.io.Source.fromInputStream(err)
      error.appendAll(src.mkString)
      src.close()
    }
  )
  val proc = process.run(io)
  val exitCode = proc.exitValue()
  if exitCode == 0 then Right(output.toString)
  else Left(s"Graphviz error: $error")

/** Create Rhine delta representation as SVG (entry point)
  *
  * @param ar
  *   AlignmentRibbon
  * @param displaySigla
  *   User-supplied sigla as List[Siglum]
  * @return
  *   Rhine delta SVG representation as String, created by Graphviz
  */
def createRhineDelta(ar: AlignmentRibbon, displaySigla: List[Siglum]): Either[String, String] =
  val allWitIds = displaySigla.indices.toSet
  val start = NodeProperties("-1.0", allWitIds, "Start")
  val end = NodeProperties(Int.MaxValue.toString, allWitIds, "End")
  val nodes = createNodes(ar) // Extract, label, and flatten reading groups into vector of NodeProperty
  val edges = createEdges(nodes, start, end, displaySigla) // Create edges as vector of EdgeProperty
  val dotFile = createDot(start +: nodes :+ end, edges, displaySigla)
  createSvg(dotFile)

// gId is stringified Int.Int, e.g. 2.5
// gId is for development, the intersection of the witnesses on the source and target of an edge is the edge label
type GId = String
case class NodeProperties(gId: GId, witnesses: Set[WitId], content: String)
case class EdgeProperties(source: GId, target: GId, witnesses: Seq[Int])
