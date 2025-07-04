package net.collatex.reptilian

import scala.util.Using
import scala.xml.*
import scala.xml.dtd.DocType
import java.nio.file.Paths

// JSON output
import ujson.*
import scala.reflect.ClassTag

// TEI XML output
import net.sf.saxon.s9api.{Processor, Serializer, XsltCompiler, XsltExecutable}
import scala.xml.{Elem, Node, PrettyPrinter}
import java.io.{PrintWriter, StringReader, StringWriter}
import javax.xml.transform.stream.StreamSource

/** Helper function (pads right with spaces) for plain text table output
  *
  * @param s
  *   string to pad
  * @param width
  *   desired width as Int
  * @return
  *   padded string
  */
def padCell(s: String, width: Int): String =
  s.padTo(width, ' ') // Left-align; pad right with spaces

/** Horizontal plain text table; rows as witnesses, columns as alignment points
  *
  * --format table | --format table-h (default, so also no --format specified)
  *
  * Displays witness sigla as row labels. Takes siglum width into account when padding.
  *
  * Files system output will append '-h.txt' to supplied 'outputBaseFilename'
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param gTa
  *   Global token array (Vector[TokenEnum]); compute readings with tString method
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '-h.txt' is appended to
  *   construct the output filename, e.g., 'foo' becomes 'foo-h.txt'.
  */
def emitTableHorizontal(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    gTa: Vector[TokenEnum],
    outputBaseFilename: Set[String]
): Unit =

  val allWitIds = displaySigla.indices
  val table = alignment.children.map { e =>
    allWitIds.map { f =>
      e.asInstanceOf[AlignmentPoint]
        .witnessReadings
        .getOrElse(f, TokenRange(0, 0, gTa)) // fake empty token range for empty cell
        .tString
    }
  }

  if table.isEmpty then
    System.err.println("Empty table (should not happen)")
    return

  val numRows = allWitIds.size
  val numCols = table.size

  val rotated = (0 until numRows).map { rowIdx =>
    displaySigla(rowIdx).value +: table.map(_(rowIdx))
  }

  val colWidths = (0 to numCols).map { colIdx =>
    rotated.map(row => row(colIdx).length).max
  }

  def renderTable(writer: PrintWriter): Unit =
    for row <- rotated do
      val padded = row.zip(colWidths).map { (cell, w) => padCell(cell, w) }
      writer.println(padded.mkString(" | "))

  if outputBaseFilename.isEmpty then Using.resource(new PrintWriter(Console.out)) { writer => renderTable(writer) }
  else
    val filename = outputBaseFilename.head + "-h.txt"
    val file = os.Path(filename, os.pwd)
    Using.resource(new PrintWriter(file.toIO)) { writer => renderTable(writer) }

/** Vertical plain text table; rows as alignment points, columns as witnesses
  *
  * --format table-v
  *
  * Displays witness sigla as header row. Takes siglum width into account when padding.
  *
  * Files system output will append '-v.txt' to supplied 'outputBaseFilename'
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param gTa
  *   Global token array (Vector[TokenEnum]); compute readings with tString method
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '-v.txt' is appended to
  *   construct the output filename, e.g., 'foo' becomes 'foo-v.txt'.
  */
def emitTableVertical(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    gTa: Vector[TokenEnum],
    outputBaseFilename: Set[String]
): Unit =

  val allWitIds = displaySigla.indices
  val table = alignment.children.map { e =>
    allWitIds.map { f =>
      e.asInstanceOf[AlignmentPoint]
        .witnessReadings
        .getOrElse(f, TokenRange(0, 0, gTa)) // fake empty token range for empty cell
        .tString
    }
  }

  if table.isEmpty then
    System.err.println("Empty table (should not happen)")
    return

  val header = displaySigla.map(_.value)
  val fullTable = header +: table

  val numCols = allWitIds.size
  val colWidths = (0 until numCols).map { colIdx =>
    fullTable.map(row => row(colIdx).length).max
  }

  def renderTable(writer: PrintWriter): Unit =
    for row <- fullTable do
      val padded = row.zip(colWidths).map { (cell, w) => padCell(cell, w) }
      writer.println(padded.mkString(" | "))

  if outputBaseFilename.isEmpty then Using.resource(new PrintWriter(Console.out)) { writer => renderTable(writer) }
  else
    val filename = outputBaseFilename.head + "-v.txt"
    val file = os.Path(filename, os.pwd)
    Using.resource(new PrintWriter(file.toIO)) { writer => renderTable(writer) }

/** Horizontal HTML table; rows as witnesses, columns as alignment points
  *
  * --format table-html-h
  *
  * Displays witness sigla as left column.
  *
  * Files system output will append '-h.html' to supplied 'outputBaseFilename'
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param gTa
  *   Global token array (Vector[TokenEnum]); compute readings with tString method
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '-v.txt' is appended to
  *   construct the output filename, e.g., 'foo' becomes 'foo-v.txt'.
  * @param htmlExtension
  *   'html' or 'xhtml'
  *
  * @return
  *   None; writes HTML document to filesystem or stdout
  */
def emitTableHorizontalHTML(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    gTa: Vector[TokenEnum],
    outputBaseFilename: Set[String], // validated: empty or exactly one value
    htmlExtension: Set[String] // validated: exactly one value ('html' or 'xhtml')
): Unit =

  val allWitIds = displaySigla.indices
  val tableContent = alignment.children.map { e =>
    allWitIds.map { f =>
      e.asInstanceOf[AlignmentPoint]
        .witnessReadings
        .getOrElse(f, TokenRange(0, 0, gTa)) // fake empty TokenRange
        .tString
    }
  }

  if tableContent.isEmpty then
    System.err.println("Empty table (should not happen)")
    return

  val numRows = allWitIds.size
  val numCols = tableContent.size // needed only if thead includes alignment-point numbering
  val rotated = (0 until numRows).map { rowIdx =>
    tableContent.map(_(rowIdx))
  }

  val htmlTable =
    <table>
      <!-- <thead>
        <tr>
          <th>Siglum</th>{
      (0 until numCols).map { i =>
        <th>AP
            {i + 1}
          </th>
      }
    }
        </tr>
      </thead> -->
      <tbody>
        {
      rotated.zip(displaySigla).map { (row, siglum) =>
        <tr>
            <th>
              {siglum.value}
            </th>{
          row.map(cell => <td>
              {cell}
            </td>)
        }
          </tr>
      }
    }
      </tbody>
    </table>

  val htmlDoc =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Alignment Table (Horizontal)</title>
        <style type="text/css">
          :root {{background-color: gainsboro;}}
          table, tr, th, td {{border: 1px solid gray;}}
          table {{border-collapse: collapse}}
          th, td {{padding: 2px 3px}}
        </style>
      </head>
      <body>
        <h2>Alignment Table (Witnesses as Rows)</h2>{htmlTable}
      </body>
    </html>

  val doctypeHtml = DocType("html")

  if outputBaseFilename.isEmpty then
    Using.resource(new PrintWriter(Console.out)) { writer =>
      XML.write(writer, htmlDoc, "UTF-8", xmlDecl = true, doctype = doctypeHtml)
    }
  else
    val filename = outputBaseFilename.head + "-h." + htmlExtension.head
    val file = os.Path(filename, os.pwd)
    Using.resource(new PrintWriter(file.toIO)) { writer =>
      XML.write(writer, htmlDoc, "UTF-8", xmlDecl = true, doctype = doctypeHtml)
    }

/** Vertical HTML table; rows as alignment points, columns as witnesses
  *
  * --format table-html-v
  *
  * Displays witness sigla as header row.
  *
  * Files system output will append '-v.html' to supplied 'outputBaseFilename'
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param gTa
  *   Global token array (Vector[TokenEnum]); compute readings with tString method
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '-v.txt' is appended to
  *   construct the output filename, e.g., 'foo' becomes 'foo-v.txt'.
  * @param htmlExtension
  *   'html' or 'xhtml'
  *
  * @return
  *   None; writes HTML document to filesystem or stdout
  */
def emitTableVerticalHTML(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    gTa: Vector[TokenEnum],
    outputBaseFilename: Set[String], // validated: empty or exactly one value
    htmlExtension: Set[String] // validated: exactly one value
): Unit =

  val allWitIds = displaySigla.indices
  val tableContent = alignment.children.map { e =>
    allWitIds.map { f =>
      e.asInstanceOf[AlignmentPoint]
        .witnessReadings
        .getOrElse(f, TokenRange(0, 0, gTa))
        .tString
    }
  }

  if tableContent.isEmpty then
    System.err.println("Empty table (should not happen)")
    return

  val htmlTable =
    <table>
      <thead>
        <tr>
          <!--<th>Alignment Point</th>-->{
      displaySigla.map(s => <th>
            {s.value}
          </th>)
    }
        </tr>
      </thead>
      <tbody>
        {
      tableContent.zipWithIndex.map { (row, idx) => // idx used only if APs are numbered
        <tr>
            <!--<td>AP
            {idx + 1}
          </td>-->{
          row.map(cell => <td>
              {cell}
            </td>)
        }
          </tr>
      }
    }
      </tbody>
    </table>

  val htmlDoc =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Alignment Table (Vertical)</title>
        <style type="text/css">
          :root {{background-color: gainsboro;}}
          table, tr, th, td {{border: 1px solid gray;}}
          table {{border-collapse: collapse}}
          th, td {{padding: 2px 3px}}
        </style>
      </head>
      <body>
        <h2>Alignment Table (Witnesses as Columns)</h2>{htmlTable}
      </body>
    </html>

  val doctypeHtml = DocType("html")

  if outputBaseFilename.isEmpty then
    Using.resource(new PrintWriter(Console.out)) { writer =>
      XML.write(writer, htmlDoc, "UTF-8", xmlDecl = true, doctype = doctypeHtml)
    }
  else
    val filename = outputBaseFilename.head + "-v." + htmlExtension.head
    val file = os.Path(filename, os.pwd)
    Using.resource(new PrintWriter(file.toIO)) { writer =>
      XML.write(writer, htmlDoc, "UTF-8", xmlDecl = true, doctype = doctypeHtml)
    }

/** Create alignment ribbon visualization (HTML and SVG)
  *
  * --format ribbon
  *
  * File system output will append value of htmlExtension ('html' or 'xhtml') to outputBaseFilename
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param displayColors
  *   List of colors is same order as 'displaySigla'
  * @param gTa
  *   Global token array (Vector[TokenEnum]); compute readings with tString method
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '.html' or '.xhtml' (taken from
  *   'htmlExtension' parameter) is appended to construct the output filename, e.g., 'foo' becomes 'foo.html' or
  *   'foo.xhtml'.
  * @param htmlExtension
  *   'html' or 'xhtml'
  *
  * @return
  *   None, Write HTML document (with embedded SVG) to filesystem or stdout
  */
def emitAlignmentRibbon(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    displayColors: List[String],
    gTa: Vector[TokenEnum],
    outputBaseFilename: Set[String], // either empty or single string (validated in parsArgs())
    htmlExtension: Set[String]
): Unit =
  val doctypeHtml: DocType = DocType("html")
  val horizontalRibbons = createHorizontalRibbons(alignment, displaySigla, displayColors, gTa)
  if outputBaseFilename.isEmpty then // Write to stdout
    Using.resource(new PrintWriter(Console.out)) { writer =>
      XML.write(writer, horizontalRibbons, "UTF-8", xmlDecl = true, doctype = doctypeHtml)
      writer.flush() // For Console.out, optional but harmless
    }
  else // Write to file
    val filename = outputBaseFilename.head + "." + htmlExtension.head // guaranteed single-item set
    val file = os.Path(filename, os.pwd) // Handles relative or absolute path
    Using.resource(new PrintWriter(file.toIO)) { writer =>
      XML.write(writer, horizontalRibbons, "UTF-8", xmlDecl = true, doctype = doctypeHtml)
      // No need to manually flush/close — Using handles it
    }

/** Create SVG representation of Rhine delta (formerly variant graph)
  *
  * --format svg
  *
  * Shows only 'n' value of tokens. For 't' values use '--format svg-rich'
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '.svg' is appended to construct
  *   the output filename, e.g., 'foo' becomes 'foo.svg'.
  *
  * @return
  *   None. Write SVG document to filesystem or stdout
  */
def emitSvgGraph(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    outputBaseFilename: Set[String] // either empty or single string (validated in parseArgs())
): Unit =
  createRhineDelta(alignment, displaySigla) match
    case Left(err) =>
      System.err.println(err)
    case Right(svg) =>
      if outputBaseFilename.isEmpty then println(svg) // write to stdout
      else
        val base = outputBaseFilename.head
        val fullPath = Paths.get(base + ".svg").toAbsolutePath
        Using.resource(new PrintWriter(fullPath.toFile, "UTF-8")) { writer =>
          writer.write(svg)
        }

def emitRichSvgGraph(): Unit =
  System.err.println("Rich SVG visualization has not yet been implemented")

/** GraphML output
  *
  * --format graphml
  *
  * Shows only 'n' value of tokens. Rank = position of alignment point. Id is "n" + rank + "." + (arbitrary) group
  * number within alignment point
  *
  * Uses Saxon XSLT only to fix pretty-printing
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '-graphml.xml' is appended to
  *   construct the output filename, e.g., 'foo' becomes 'foo-graphml.xml'.
  *
  * @return
  *   None. Write XML document to filesystem or stdout
  */
def emitGraphMl(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    outputBaseFilename: Set[String]
): Unit =
  val keys = Seq(
    <key id="d0" for="node" attr.name="number" attr.type="int"/>,
    <key id="d1" for="node" attr.name="tokens" attr.type="string"/>,
    <key id="d2" for="node" attr.name="rank" attr.type="int"/>,
    <key id="d3" for="edge" attr.name="number" attr.type="int"/>,
    <key id="d4" for="edge" attr.name="type" attr.type="string"/>,
    <key id="d5" for="edge" attr.name="witnesses" attr.type="string"/>
  )
  val nodes = alignment.children.zipWithIndex.toVector.map { (alignmentUnit, rank) =>
    val point = alignmentUnit.asInstanceOf[AlignmentPoint]
    val groups = point.witnessGroups.zipWithIndex.toVector.map { (group, gpIdx) =>
      val id = List("n", rank, ".", gpIdx).mkString
      val content = group.head._2.nString
      <node id={id}>
        <data key="d0">{id}</data>
        <data key="d2">{rank}</data>
        <data key="d1">{content}</data>
      </node>
    }
    groups
  }

  val xmlRoot: Elem =
    <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns 
     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
      {keys}
      <graph id="g0" 
             edgedefault="directed" 
             parse.nodeids="canonical" 
             parse.edgeids="canonical"
             parse.order="nodesfirst">{nodes}</graph>
    </graphml>

  val prettyPrinter = new PrettyPrinter(80, 2)
  val renderedBody = prettyPrinter.format(xmlRoot)

  val declaration = """<?xml version="1.0" encoding="UTF-8"?>"""

  val fullOutput = s"$declaration\n$renderedBody" // prepend string instead of XML.write to retain pretty-print

  if outputBaseFilename.isEmpty then println(fullOutput)
  else
    val filename = outputBaseFilename.head + "-graphml.xml"
    val file = os.Path(filename, os.pwd)
    os.write.over(file, fullOutput)

/** Helper to convert an `Any` value to a ujson.Value, handling common Scala/Java types */
def anyToUjsonValue(value: Any): Value = value match
  case null          => ujson.Null
  case s: String     => ujson.Str(s)
  case i: Int        => ujson.Num(i)
  case l: Long       => ujson.Num(l.toDouble) // Direct us of Long is deprecated
  case d: Double     => ujson.Num(d)
  case f: Float      => ujson.Num(f)
  case b: Boolean    => ujson.Bool(b)
  case seq: Seq[_]   => ujson.Arr(seq.map(anyToUjsonValue)*)
  case arr: Array[_] => ujson.Arr(arr.map(anyToUjsonValue)*)
  case map: Map[_, _] =>
    val jsonFields = map.collect { case (k: String, v) => k -> anyToUjsonValue(v) }
    ujson.Obj.from(jsonFields)
  case other =>
    ujson.Str(other.toString)

/** JSON output
  *
  * --format json
  *
  * JSON structure documented at TBA
  *
  * Uses reflection to accommodate Token properties other than t, n, w, g, if provided
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param gTa
  *   Global token array (Vector[TokenEnum]); compute readings with tString method
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If present, '-json' is appended to
  *   construct the output filename, e.g., 'foo' becomes 'foo.json'.
  */
def emitJson(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    gTa: Vector[TokenEnum],
    outputBaseFilename: Set[String]
): Unit =

  val allWitIds = displaySigla.indices
  val witnessesJson = displaySigla.map(_.value)

  val tableJson = alignment.children.toVector.map { alignmentUnit =>
    val point = alignmentUnit.asInstanceOf[AlignmentPoint]

    val tokenArrays: Seq[Value] = allWitIds.map { witId =>
      point.witnessReadings.get(witId) match
        case Some(tokenRange) =>
          val tokensJson = tokenRange.tokens.map { token =>
            val standardKeys = Seq("t", "n", "w", "g")

            val fieldMap: Map[String, Any] =
              token.getClass.getDeclaredFields.map { field =>
                field.setAccessible(true)
                field.getName -> field.get(token)
              }.toMap

            val knownFields: Seq[(String, Value)] = standardKeys.map {
              case "w" => "w" -> Str(displaySigla(witId).value)
              case k   => k -> anyToUjsonValue(fieldMap(k))
            }

            val additionalFields: Seq[(String, Value)] = fieldMap
              .filterNot { case (k, _) => standardKeys.contains(k) }
              .toSeq
              .sortBy(_._1)
              .map { case (k, v) => k -> anyToUjsonValue(v) }

            Obj.from(knownFields ++ additionalFields)
          }
          Arr(tokensJson*)
        case None => Arr()
    }
    Arr(tokenArrays*)
  }

  val jsonRoot = Obj(
    "witnesses" -> Arr(witnessesJson.map(Str(_))*),
    "table" -> Arr(tableJson*)
  )

  val renderedJson = ujson.write(jsonRoot, indent = 2)

  if outputBaseFilename.isEmpty then println(renderedJson)
  else
    val filename = outputBaseFilename.head + ".json"
    val file = os.Path(filename, os.pwd)
    os.write.over(file, renderedJson)

/** TEI parallel segmentation
  *
  * --format tei
  *
  * <rdgGrp> for shared 'n' property (missing witnesses have empty 'n' value)
  *
  * NB: If all present witnesses ('t' values) end in space, the space is moved out of the <rdg> and after the <app>,
  * corresponding to where a human tagger would have put a shared inter-token space character. This adds an extra space
  * in the case of missing witnesses, which can be flattened, if desired, in post-processing or by pretty-printing.
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If populated, must be singleton, and
  *   '-tei.xml' is appended to construct the output filename, e.g., 'foo' becomes 'foo-tei.xml'.
  *
  * @return
  *   None. Write XML document to filesystem or stdout
  */
def emitTeiXml(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    outputBaseFilename: Set[String]
): Unit = {

  val nsCx = "http://interedition.eu/collatex/ns/1.0"
  val nsTei = "http://www.tei-c.org/ns/1.0"
  val siglumOrder: Map[String, Int] = displaySigla.map(_.value).zipWithIndex.toMap
  val allWitIds: Set[Int] = displaySigla.indices.toSet

  val content: Seq[Node] = alignment.children.toSeq.flatMap { ap =>
    val groups: Set[WitnessReadings] = ap.asInstanceOf[AlignmentPoint].witnessGroups
    val presentWitIds = groups.flatMap(_.keys)
    val missingWitIds = allWitIds.diff(presentWitIds)

    val groupElems = groups.toList.map { group =>
      val readings = group.toList.groupBy(_._2.tString)
      val rdgs = readings.toList.map((tString, witEntries) =>
        val sortedSigla = witEntries
          .map((witId, _) => displaySigla(witId))
          .sortBy(s => siglumOrder(s.value))
        val witAttr = sortedSigla.map(s => "#" + s.value).mkString(" ")
        Elem(
          null,
          "rdg",
          new UnprefixedAttribute("wit", witAttr, xml.Null),
          TopScope,
          minimizeEmpty = true,
          Text(tString)
        )
      )
      val nString = group.headOption.map(_._2.nString).getOrElse("")
      Elem(null, "rdgGrp", new UnprefixedAttribute("n", nString, xml.Null), TopScope, minimizeEmpty = true, rdgs*)
    }

    val missingGrp =
      if (missingWitIds.nonEmpty) {
        val sortedMissing = missingWitIds.map(displaySigla(_)).toList.sortBy(s => siglumOrder(s.value))
        val witAttr = sortedMissing.map(s => "#" + s.value).mkString(" ")
        val missingRdg =
          Elem(null, "rdg", new UnprefixedAttribute("wit", witAttr, xml.Null), TopScope, minimizeEmpty = true)
        Some(
          Elem(null, "rdgGrp", new UnprefixedAttribute("n", "", xml.Null), TopScope, minimizeEmpty = true, missingRdg)
        )
      } else None

    val allGrps = (groupElems ++ missingGrp).sortBy { elem =>
      if (elem.attribute("n").exists(_.text.isEmpty)) Int.MaxValue
      else {
        val firstWit = (elem \ "rdg").headOption
          .flatMap(_.attribute("wit"))
          .map(_.text.split(" ").headOption.getOrElse(""))
          .getOrElse("")
        siglumOrder.getOrElse(firstWit.stripPrefix("#"), Int.MaxValue)
      }
    }

    val allRdgs = (allGrps \ "rdg").toList
    val allEndWithSpace = allRdgs.filter(_.text.nonEmpty).forall(_.text.endsWith(" "))

    if (allGrps.size == 1 && missingWitIds.isEmpty) {
      // Uniform reading, no variation, output as plain text
      val txt = allRdgs.headOption.map(_.text).getOrElse("")
      Seq(Text(txt))
    } else {
      // If all present readings end in space, remove it from rdg and move it after app
      val cleanedGroups = if (allEndWithSpace) {
        allGrps.map { grp =>
          val cleanedRdgs = (grp \ "rdg").map { rdg =>
            val cleanedText = if (rdg.text.nonEmpty && rdg.text.endsWith(" ")) rdg.text.dropRight(1) else rdg.text
            rdg.asInstanceOf[Elem].copy(child = Text(cleanedText))
          }
          grp.copy(child = cleanedRdgs)
        }
      } else allGrps

      val trailingSpace = if (allEndWithSpace) Some(Text(" ")) else None

      val appElem = Elem(null, "app", xml.Null, TopScope, minimizeEmpty = true, cleanedGroups*)
      trailingSpace match {
        case Some(space) => Seq(appElem, space)
        case None        => Seq(appElem)
      }
    }
  }

  val root = Elem(
    "cx",
    "apparatus",
    xml.Null,
    NamespaceBinding("cx", nsCx, NamespaceBinding(null, nsTei, TopScope)),
    minimizeEmpty = true,
    content*
  )

  val rawXmlString = s"""<?xml version="1.0" encoding="UTF-8"?>\n${root.toString()}"""

  val finalOutput = applyTeiPrettyPrint(rawXmlString)

  if (outputBaseFilename.isEmpty) println(finalOutput)
  else {
    val out = new PrintWriter(s"${outputBaseFilename.head}-tei.xml", "UTF-8")
    try out.println(finalOutput)
    finally out.close()
  }
}

/** Helper function invokes Saxon XSLT to create TEI XML output
  *
  * The XSLT is just an identity transformation with indent="yes"; the only thing we need from it is pretty-printing,
  * which Saxon does correctly where Scala doesn’t.
  *
  * @param xmlString
  *   Raw XML to format
  * @return
  *   Pretty-printed XML as String
  */
def applyTeiPrettyPrint(xmlString: String): String = {
  val processor = new Processor(false)
  val compiler: XsltCompiler = processor.newXsltCompiler()
  val xsltStream = getClass.getResourceAsStream("/tei-pretty-print.xsl")
  val executable: XsltExecutable = compiler.compile(new StreamSource(xsltStream))

  val builder = processor.newDocumentBuilder()
  val inputDoc = builder.build(new StreamSource(new StringReader(xmlString)))

  val transformer = executable.load()
  transformer.setInitialContextNode(inputDoc)

  val writer = new StringWriter()
  val serializer = processor.newSerializer(writer)
  serializer.setOutputProperty(Serializer.Property.INDENT, "yes")

  transformer.setDestination(serializer)
  transformer.transform()

  writer.toString
}

/** Custom CollateX XML structure
  *
  * --format xml
  *
  * For documentation see TBA.
  *
  * @param alignment
  *   AlignmentRibbon; children property is a ListBuffer of AlignmentPoint instances (but defined as AlignmentUnit)
  * @param displaySigla
  *   List of Sigla in output order (List[Siglum])
  * @param outputBaseFilename
  *   Base filename for file-system output. If empty, output goes to stdout. If populated, must be singleton, and '.xml'
  *   is appended to construct the output filename, e.g., 'foo' becomes 'foo.xml'.
  *
  * @return
  *   None. Write XML document to filesystem or stdout
  */
def emitXml(
    alignment: AlignmentRibbon,
    displaySigla: List[Siglum],
    outputBaseFilename: Set[String]
): Unit =

  val namespace = "http://interedition.eu/collatex/ns/1.0"

  val rows: Seq[Node] = alignment.children.toVector.map { alignmentUnit =>
    val point = alignmentUnit.asInstanceOf[AlignmentPoint]

    val cells = point.witnessReadings.toSeq.sortBy(_._1).map { (witId, tokenRange) =>
      <cell sigil={displaySigla(witId).value}>
        {tokenRange.tString}
      </cell>
    }

    <row>
      {cells}
    </row>
  }

  val xmlRoot: Elem =
    <alignment xmlns={namespace}>
      {rows}
    </alignment>

  val prettyPrinter = new PrettyPrinter(80, 2)
  val renderedBody = prettyPrinter.format(xmlRoot)

  val declaration = """<?xml version="1.0" encoding="UTF-8"?>"""

  val fullOutput = s"$declaration\n$renderedBody" // prepend string instead of XML.write to retain pretty-print

  if outputBaseFilename.isEmpty then println(fullOutput)
  else
    val filename = outputBaseFilename.head + ".xml"
    val file = os.Path(filename, os.pwd)
    os.write.over(file, fullOutput)
