package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import net.collatex.util.{Graph, Hypergraph}

import scala.xml.dtd.DocType

def createHtmlTable(rds: Seq[Seq[EdgeData]]): Unit = // inner sequences are tbody groups
  // Filename length cannot exceed 255 (APFS)
  val hgId = rds.flatMap(_.map(_.he).sorted.distinct).drop(1).mkString("-").slice(0, 200) // "starts" is always first
  def edgeEndpointsToString(ep: EdgeEndpoints): String = // format edge for html table
    s"${ep.source} → ${ep.target}"
  val css: String =
    """section {
      |  background-color: seashell;
      |  border: 2px black solid;
      |  width: fit-content;
      |  margin-left: .5em;
      |  padding: 0 .2em;
      |}
      |ul {
      |  padding-left: 1.5em;
      |}
      |table {
      |  background-color: seashell;
      |  border-collapse: collapse;
      |}
      |table,
      |thead,
      |tbody {
      |  border: 2px black solid;
      |}
      |th {
      |  border-left: 2px black solid;
      |  border-right: 2px black solid;
      |  border-top: 1px darkgray solid;
      |  border-bottom: 1px darkgray solid;
      |}
      |td {
      |  border: 1px darkgray solid;
      |}
      |th,
      |td {
      |  padding: 2px 3px;
      |}
      |tr:first-child > th:nth-child(2),
      |tr:not(:first-child) > th:first-child,
      |tr:first-child > td:nth-child(4),
      |tr:not(:first-child) > td:nth-child(3) {
      |  text-align: right;
      |}
      |.old {
      |  color: lightgray;
      |}
      |""".stripMargin
  val thead =
    <thead>
      <tr>
        <th>Label</th>
        <th>Witness</th>
        <th>Token range</th>
        <th>Source</th>
        <th>Target</th>
        <th>Edge</th>
      </tr>
    </thead>
  val tbodys =
    for rd <- rds yield
      val heHead =
        val th =
          if rd.size > 1 then <th rowspan={rd.size.toString}>
            {rd.head.he}
          </th>
          else <th>
            {rd.head.he}
          </th>
        val rowCells =
          Seq(
            <th>
              {rd.head.witness.toString}
            </th>
              <td>
                {rd.head.tokenRange.toString}
              </td>,
            <td>
              {rd.head.source.toString}
            </td>,
            <td>
              {rd.head.tmTarget.get._2}
            </td>,
            <td>
              {edgeEndpointsToString(rd.head.edge)}
            </td>
          )
        <tr>
          {Seq(th, rowCells)}
        </tr>
      val heTail = rd.zipWithIndex.tail
        .map((ed, offset) =>
          <tr>
            <th>
              {ed.witness}
            </th>
            <td>
              {ed.tokenRange.toString}
            </td>
            <td>
              {ed.source.toString}
            </td>
            <td>
              {ed.tmTarget.get._2}
            </td>{
            if rd.slice(0, offset).map(_.edge).contains(ed.edge) then <td class="old">
              {edgeEndpointsToString(ed.edge)}
            </td>
            else <td>
            {edgeEndpointsToString(ed.edge)}
          </td>
          }
          </tr>
        )
      val result = <tbody>
        {Seq(heHead, heTail)}
      </tbody>
      result
  val h = <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>
          {hgId}
        </title>
        <style>{css}</style>
      </head>
      <body>
        <table>
          {Seq(thead, tbodys)}
        </table>
      </body>
    </html>
  val doctypeHtml: scala.xml.dtd.DocType = DocType("html") // used for single-column and mixed output
  val dependencyTablePath =
    os.pwd / "src" / "main" / "outputs" /
      s"dependency-graph-table-$hgId.xhtml"
  scala.xml.XML.save(dependencyTablePath.toString, h, "UTF-8", true, doctypeHtml)

def dependencyGraphToDot(
    depGraph: Graph[NodeType],
    hg: Hypergraph[EdgeLabel, TokenRange]
): Unit =
  val gTa = hg.verticesIterator.next.ta
  val hgId = hg.hyperedgeLabels.map(_.toString).toSeq.sorted.mkString("-").slice(0, 200)
  val prologue = "digraph G {\n\t"
  val epilogue = "\n}"
  val edges = depGraph.toMap
    .map((k, v) => k -> v._2)
    .map((k, v) => v.map(target => k -> target))
    .flatten
  // println("Result")
  // edges.foreach(e => println(s"dot edge: $e"))
  val readings = edges
    .flatMap((k, v) => Set(k, v))
    .toSet
    .diff(Set(NodeType("starts"), NodeType("ends")))
    .map(k => k -> Vector("\"", k, ": ", hg.members(EdgeLabel(k)).head.tString, "\"").mkString)
    .toMap ++ Map(NodeType("starts") -> "starts", NodeType("ends") -> "ends")
  val dotEdges = edges
    .map((k, v) => Vector(k.toString, " -> ", v.toString).mkString)
    .mkString(";\n\t")
  val dotNodes = ";\n\t" + readings
    .map((k, v) => Vector(k, "[label=", v.replace("\n", " "), "]").mkString)
    .mkString(";\n\t")
  val dotPath =
    os.pwd / "src" / "main" / "outputs" /
      s"${System.currentTimeMillis()}-dependency-graph-$hgId.dot"
  val dotValue = prologue + dotEdges + dotNodes + epilogue
  os.write.over(dotPath, dotValue)
