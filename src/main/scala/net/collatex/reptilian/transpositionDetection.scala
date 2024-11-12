package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import net.collatex.util.{Graph, Hypergraph}

import scala.collection.immutable.TreeMap
import scala.xml.dtd.DocType
import scala.xml.{Null, Text}

/* Method
 *
 * When aligning two hyper-graphs we have to detect transpositions between the two graphs.
 * To detect transpositions we need have order, as we do for creating a variant graph or an
 * alignment table.
 *
 * We run into the problem that the Token ranges, and thus the Hyperedges in an alignment
 * hypergraph, are partially ordered. Token ranges only state something about a single witness,
 * which means that two token ranges within the same witness can be compared and their relative
 * order determined. Two token ranges of different witnesses however can't be compared.
 *
 * Partially ordered items cannot be sorted the traditional way because not all the items can be
 * compared. To sort them we have to create a dependency graph and then topologically sort the
 * nodes in the graph.
 *
 * 1. Create a dependency graph (DAG) for each of the hyper-graphs:
 *    a. Every hyperedge in the hypergraph becomes a node (use the hyperedge label as the
 *       identifier) in the dependency graph. Like a variant graph, a dependency graph needs
 *       a start and an end, which need to have the start and end offsets in the global token
 *       array of the starts and ends. This provides the lowest and highest offsets for each
 *       witness, which we need in order to know where they start and stop in the global token
 *       array. (NB: Create separators in the global token array to facilitate finding the
 *       seams between witnesses.
 *    b. Create 'fake' hyperedges for the start and end nodes and find the start and end
 *       coordinates in the global token array. Start and end nodes must contain all witnesses.
 *       Create two (not just one) hypergraphs: one with the fake start (but not end) hyperedges
 *       and the hyperedges of the graph and one with the fake end (but not start) hyperedge and
 *       the hyperedges of the graph. We use the one with ends to create the map and the one with
 *       the starts to create the edges for the dependency graph.
 *    c. Create the map: Go over the hyperedges + the fake end in each hypergraph to transform
 *       the hypergraphs to a TreeMap (sorted Map[Int, String]), where Int is the position where
 *       a token range starts in the global token array and String is the edge label where the
 *       data come from.
 *    d. Create the outgoing edges for the dependency graph: Go over the hyperedges + fake start
 *       and use the sorted keys to determine what the next item in the map is for the positions in
 *       each of the witnesses. There will be duplicate targets and we want one edge for multiple
 *       witnesses with the same target, so fetch for every witness and deduplicate before combining
 *       (next step).
 *    e. Combine edges into dependency graph using fold. Visualize for sanity checking.
 * 2. Topological sort the dependency graph
 * 3. Rank the hyperedges
 * 4. Create a traversal/decision graph for the traversal of the two sorted and ranked hyperedges
 * 5. Beam search the traversal graph to create the alignment (resolving transpositions)
 *
 * Later optimization: We can determine the relative order of two blocks for a hyperedge that
 * appears in both blocks.
 * */

// Data
// Separators in global token array have the form Token("Sepnn", "Sepnn", w, nn), where
// w = witness number of preceding witness and nn = global offset
given gTa: Vector[Token] =
  Vector(
    Token("natural  ", "natural", 0, 0),
    Token("selection ", "selection", 0, 1),
    Token(", ", ",", 0, 2),
    Token("as ", "as", 0, 3),
    Token("will ", "will", 0, 4),
    Token("hereafter ", "hereafter", 0, 5),
    Token("be ", "be", 0, 6),
    Token("explained ", "explained", 0, 7),
    Token(", ", ",", 0, 8),
    Token("will ", "will", 0, 9),
    Token("determine ", "determine", 0, 10),
    Token("how ", "how", 0, 11),
    Token("far ", "far", 0, 12),
    Token("the ", "the", 0, 13),
    Token("new ", "new", 0, 14),
    Token("characters ", "characters", 0, 15),
    Token("thus ", "thus", 0, 16),
    Token("arising ", "arising", 0, 17),
    Token("shall ", "shall", 0, 18),
    Token("be ", "be", 0, 19),
    Token("preserved ", "preserved", 0, 20),
    Token("Sep21", "Sep21", 0, 21),
    Token("natural ", "natural", 1, 22),
    Token("selection ", "selection", 1, 23),
    Token(", ", ",", 1, 24),
    Token("as ", "as", 1, 25),
    Token("will ", "will", 1, 26),
    Token("hereafter ", "hereafter", 1, 27),
    Token("be ", "be", 1, 28),
    Token("explained ", "explained", 1, 29),
    Token(", ", ",", 1, 30),
    Token("will ", "will", 1, 31),
    Token("determine ", "determine", 1, 32),
    Token("how ", "how", 1, 33),
    Token("far ", "far", 1, 34),
    Token("the ", "the", 1, 35),
    Token("new ", "new", 1, 36),
    Token("characters ", "characters", 1, 37),
    Token("thus ", "thus", 1, 38),
    Token("arising ", "arising", 1, 39),
    Token("shall ", "shall", 1, 40),
    Token("be ", "be", 1, 41),
    Token("preserved ", "preserved", 1, 42),
    Token("Sep43", "Sep43", 1, 43),
    Token("natural ", "natural", 2, 44),
    Token("selection ", "selection", 2, 45),
    Token(", ", ",", 2, 46),
    Token("as ", "as", 2, 47),
    Token("will ", "will", 2, 48),
    Token("hereafter ", "hereafter", 2, 49),
    Token("be ", "be", 2, 50),
    Token("explained ", "explained", 2, 51),
    Token(", ", ",", 2, 52),
    Token("will ", "will", 2, 53),
    Token("determine ", "determine", 2, 54),
    Token("how ", "how", 2, 55),
    Token("far ", "far", 2, 56),
    Token("the ", "the", 2, 57),
    Token("new ", "new", 2, 58),
    Token("characters ", "characters", 2, 59),
    Token("thus ", "thus", 2, 60),
    Token("arising ", "arising", 2, 61),
    Token("shall ", "shall", 2, 62),
    Token("be ", "be", 2, 63),
    Token("preserved ", "preserved", 2, 64),
    Token("Sep65", "Sep65", 2, 65),
    Token("natural ", "natural", 3, 66),
    Token("selection ", "selection", 3, 67),
    Token(", ", ",", 3, 68),
    Token("as ", "as", 3, 69),
    Token("will ", "will", 3, 70),
    Token("hereafter ", "hereafter", 3, 71),
    Token("be ", "be", 3, 72),
    Token("explained ", "explained", 3, 73),
    Token(", ", ",", 3, 74),
    Token("will ", "will", 3, 75),
    Token("determine ", "determine", 3, 76),
    Token("how ", "how", 3, 77),
    Token("far ", "far", 3, 78),
    Token("the ", "the", 3, 79),
    Token("new ", "new", 3, 80),
    Token("characters ", "characters", 3, 81),
    Token("thus ", "thus", 3, 82),
    Token("arising ", "arising", 3, 83),
    Token("shall ", "shall", 3, 84),
    Token("be ", "be", 3, 85),
    Token("preserved ", "preserved", 3, 86),
    Token(". ", ".", 3, 87),
    Token("character ", "character", 3, 88),
    Token("of ", "of", 3, 89),
    Token("domestic ", "domestic", 3, 90),
    Token("varieties ", "varieties", 3, 91),
    Token("; ", ";", 3, 92),
    Token("difficulty ", "difficulty", 3, 93),
    Token("of ", "of", 3, 94),
    Token("distinguishing ", "distinguishing", 3, 95),
    Token("between ", "between", 3, 96),
    Token("varieties ", "varieties", 3, 97),
    Token("and ", "and", 3, 98),
    Token("species ", "species", 3, 99),
    Token("; ", ";", 3, 100),
    Token("origin ", "origin", 3, 101),
    Token("of ", "of", 3, 102),
    Token("domestic ", "domestic", 3, 103),
    Token("varieties ", "varieties", 3, 104),
    Token("from ", "from", 3, 105),
    Token("one ", "one", 3, 106),
    Token("or ", "or", 3, 107),
    Token("more ", "more", 3, 108),
    Token("species ", "species", 3, 109),
    Token("Sep110", "Sep110", 3, 110),
    Token("there ", "there", 4, 111),
    Token("is ", "is", 4, 112),
    Token("not ", "not", 4, 113),
    Token("a ", "a", 4, 114),
    Token("shadow ", "shadow", 4, 115),
    Token("of ", "of", 4, 116),
    Token("evidence ", "evidence", 4, 117),
    Token("in ", "in", 4, 118),
    Token("favour ", "favour", 4, 119),
    Token("of ", "of", 4, 120),
    Token("this ", "this", 4, 121),
    Token("view ", "view", 4, 122),
    Token(": ", ":", 4, 123),
    Token("to ", "to", 4, 124),
    Token("assert ", "assert", 4, 125),
    Token("that ", "that", 4, 126),
    Token("we ", "we", 4, 127),
    Token("could ", "could", 4, 128),
    Token("not ", "not", 4, 129),
    Token("breed ", "breed", 4, 130),
    Token("our ", "our", 4, 131),
    Token("cart ", "cart", 4, 132),
    Token("and ", "and", 4, 133),
    Token("race ", "race", 4, 134),
    Token("- ", "-", 4, 135),
    Token("horses ", "horses", 4, 136),
    Token(", ", ",", 4, 137),
    Token("long ", "long", 4, 138),
    Token("and ", "and", 4, 139),
    Token("short ", "short", 4, 140),
    Token("- ", "-", 4, 141),
    Token("horned ", "horned", 4, 142),
    Token("cattle ", "cattle", 4, 143),
    Token(", ", ",", 4, 144),
    Token("and ", "and", 4, 145),
    Token("poultry ", "poultry", 4, 146),
    Token("of ", "of", 4, 147),
    Token("various ", "various", 4, 148),
    Token("breeds ", "breeds", 4, 149),
    Token(", ", ",", 4, 150),
    Token("and ", "and", 4, 151),
    Token("esculent ", "esculent", 4, 152),
    Token("vegetables ", "vegetables", 4, 153),
    Token(", ", ",", 4, 154),
    Token("for ", "for", 4, 155),
    Token("an ", "an", 4, 156),
    Token("unlimited ", "unlimited", 4, 157),
    Token("number ", "number", 4, 158),
    Token("of ", "of", 4, 159),
    Token("generations ", "generations", 4, 160),
    Token(", ", ",", 4, 161),
    Token("would ", "would", 4, 162),
    Token("be ", "be", 4, 163),
    Token("opposed ", "opposed", 4, 164),
    Token("to ", "to", 4, 165),
    Token("all ", "all", 4, 166),
    Token("experience ", "experience", 4, 167),
    Token(". ", ".", 4, 168),
    Token("character ", "character", 4, 169),
    Token("of ", "of", 4, 170),
    Token("domestic ", "domestic", 4, 171),
    Token("varieties ", "varieties", 4, 172),
    Token("; ", ";", 4, 173),
    Token("difficulty ", "difficulty", 4, 174),
    Token("of ", "of", 4, 175),
    Token("distinguishing ", "distinguishing", 4, 176),
    Token("between ", "between", 4, 177),
    Token("varieties ", "varieties", 4, 178),
    Token("and ", "and", 4, 179),
    Token("species ", "species", 4, 180),
    Token("; ", ";", 4, 181),
    Token("origin ", "origin", 4, 182),
    Token("of ", "of", 4, 183),
    Token("domestic ", "domestic", 4, 184),
    Token("varieties ", "varieties", 4, 185),
    Token("from ", "from", 4, 186),
    Token("one ", "one", 4, 187),
    Token("or ", "or", 4, 188),
    Token("more ", "more", 4, 189),
    Token("species ", "species", 4, 190),
    Token("Sep191", "Sep191", 4, 191),
    Token("there ", "there", 5, 192),
    Token("is ", "is", 5, 193),
    Token("not ", "not", 5, 194),
    Token("a ", "a", 5, 195),
    Token("shadow ", "shadow", 5, 196),
    Token("of ", "of", 5, 197),
    Token("evidence ", "evidence", 5, 198),
    Token("in ", "in", 5, 199),
    Token("favour ", "favour", 5, 200),
    Token("of ", "of", 5, 201),
    Token("this ", "this", 5, 202),
    Token("view ", "view", 5, 203),
    Token(": ", ":", 5, 204),
    Token("to ", "to", 5, 205),
    Token("assert ", "assert", 5, 206),
    Token("that ", "that", 5, 207),
    Token("we ", "we", 5, 208),
    Token("could ", "could", 5, 209),
    Token("not ", "not", 5, 210),
    Token("breed ", "breed", 5, 211),
    Token("our ", "our", 5, 212),
    Token("cart ", "cart", 5, 213),
    Token("and ", "and", 5, 214),
    Token("race ", "race", 5, 215),
    Token("- ", "-", 5, 216),
    Token("horses ", "horses", 5, 217),
    Token(", ", ",", 5, 218),
    Token("long ", "long", 5, 219),
    Token("and ", "and", 5, 220),
    Token("short ", "short", 5, 221),
    Token("- ", "-", 5, 222),
    Token("horned ", "horned", 5, 223),
    Token("cattle ", "cattle", 5, 224),
    Token(", ", ",", 5, 225),
    Token("and ", "and", 5, 226),
    Token("poultry ", "poultry", 5, 227),
    Token("of ", "of", 5, 228),
    Token("various ", "various", 5, 229),
    Token("breeds ", "breeds", 5, 230),
    Token(", ", ",", 5, 231),
    Token("and ", "and", 5, 232),
    Token("esculent ", "esculent", 5, 233),
    Token("vegetables ", "vegetables", 5, 234),
    Token(", ", ",", 5, 235),
    Token("for ", "for", 5, 236),
    Token("an ", "an", 5, 237),
    Token("unlimited ", "unlimited", 5, 238),
    Token("number ", "number", 5, 239),
    Token("of ", "of", 5, 240),
    Token("generations ", "generations", 5, 241),
    Token(", ", ",", 5, 242),
    Token("would ", "would", 5, 243),
    Token("be ", "be", 5, 244),
    Token("opposed ", "opposed", 5, 245),
    Token("to ", "to", 5, 246),
    Token("all ", "all", 5, 247),
    Token("experience ", "experience", 5, 248),
    Token(". ", ".", 5, 249),
    Token("character ", "character", 5, 250),
    Token("of ", "of", 5, 251),
    Token("domestic ", "domestic", 5, 252),
    Token("varieties ", "varieties", 5, 253),
    Token(": ", ":", 5, 254),
    Token("difficulty ", "difficulty", 5, 255),
    Token("of ", "of", 5, 256),
    Token("distinguishing ", "distinguishing", 5, 257),
    Token("between ", "between", 5, 258),
    Token("varieties ", "varieties", 5, 259),
    Token("and ", "and", 5, 260),
    Token("species ", "species", 5, 261),
    Token("; ", ";", 5, 262),
    Token("origin ", "origin", 5, 263),
    Token("of ", "of", 5, 264),
    Token("domestic ", "domestic", 5, 265),
    Token("varieties ", "varieties", 5, 266),
    Token("from ", "from", 5, 267),
    Token("one ", "one", 5, 268),
    Token("or ", "or", 5, 269),
    Token("more ", "more", 5, 270),
    Token("species ", "species", 5, 271)
  )
val hg1: Hypergraph[String, TokenRange] = Hypergraph(
  Map(
    "173" -> Set(TokenRange(173, 174)),
    "254" -> Set(TokenRange(254, 255)),
    "255" -> Set(TokenRange(255, 272), TokenRange(174, 191)),
    "192" -> Set(TokenRange(192, 254), TokenRange(111, 173))
  ),
  Map(
    TokenRange(254, 255) -> Set("254"),
    TokenRange(174, 191) -> Set("255"),
    TokenRange(173, 174) -> Set("173"),
    TokenRange(192, 254) -> Set("192"),
    TokenRange(111, 173) -> Set("192"),
    TokenRange(255, 272) -> Set("255")
  )
)
val hg2: Hypergraph[String, TokenRange] = Hypergraph(
  Map(
    "22" -> Set(TokenRange(22, 43), TokenRange(0, 21), TokenRange(44, 65), TokenRange(66, 87)),
    "87" -> Set(TokenRange(87, 110))
  ),
  Map(
    TokenRange(44, 65) -> Set("22"),
    TokenRange(87, 110) -> Set("87"),
    TokenRange(66, 87) -> Set("22"),
    TokenRange(0, 21) -> Set("22"),
    TokenRange(22, 43) -> Set("22")
  )
)

// Sorted map (treemap?) from start of token range (Int) to hyperedge label (String)
def createTreeMap(hg: Hypergraph[String, TokenRange]): TreeMap[Int, String] =
  hg.am2
    .map((tr, l) => tr.start -> l.head)
    .to(TreeMap)

// Take in hypergraph with fake starts plus tree map and return dependency graph
// For each key in hg 1) find all target TokenRange starts, 2) look up start value
//   in tm keys, and 3) retrieve next tm key sequentially, and return value associated
//   with that next key. E.g., with hyperedge
//   255 -> Set(TokenRange(255,272), TokenRange(174, 191)) locate keys 255 and
//   174 in treemap, find next key sequentially, and return associated value.
def createDependencyGraph(
    hg: Hypergraph[String, TokenRange],
    tm: TreeMap[Int, String]
)(using gTa: Vector[Token]): Graph[String] =
  // println(tm)
  /*
    For each hyperedge
      a) Covert to vector and sort by hyperedge label, with "starts" first
      b) Create <tbody>, inside which:
    For each token range within the hyperedge, sorted by witness
      a) Create <tr>, inside which
      b) If first token range, create <th> with rowspan matching count of token ranges
      c) Compute source and target
      d) If hyperedge lable is "starts", retrieve witness id from next token; otherwise from same token
      e) Create <td> for witness id, token range, source, target
   */
  // outer vector is hyperedges, inner vector is token ranges within hyperedge
  val hgId = hg.hyperedges.filterNot(_ == "starts").toSeq.sortBy(_.toInt).mkString("-")
  def processTokR(tokr: TokenRange, he: String) =
    val witness = <th>{
      he match {
        case "starts" => gTa(tokr.start + 1).w
        case _        => gTa(tokr.start).w
      }
    }</th>
    val tokenRange = <td>{tokr}</td>
    val source = <td>{tokr.start}</td>
    val targetValue = tm.minAfter(tokr.start + 1)
    val target = <td>{targetValue}</td>
    val edge = <td>{s"$he → ${targetValue.get._2}"}</td>
    Seq(witness, tokenRange, source, target, edge)
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
  val tbodysAndEdges =
    val sortedHes = // move starts to beginning, sort labels as integers, rather than strings
      val allHes = hg.hyperedges.toSeq.sorted
      allHes.last +: allHes.dropRight(1).sortBy(_.toInt)
    for he <- sortedHes yield
      val tokrs = hg.members(he).toSeq.sortBy(e => e.start) // gTa is already ordered
      val heHead =
        val th = if tokrs.size > 1 then <th rowspan={tokrs.size.toString}>{he}</th> else <th>{he}</th>
        val rowData = processTokR(tokrs.head, he)
        <tr>{Seq(th, rowData)}</tr>
      val heTail =
        tokrs.tail.map(e => <tr>{processTokR(e, he)}</tr>)
      val rows = heHead +: heTail
      val columnData = for row <- rows yield
        row \\ "td"
      val edgeData = columnData.map(_.last).distinct.map(_.text)
      val edges = <ul>{edgeData.map(e => <li>{e}</li>)}</ul>
      (<tbody>{Seq(heHead, heTail)}</tbody>, edges)
  val h = <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>{hgId}</title>
      <style type="text/css">
        section {{
          background-color: seashell;
          border: 2px black solid;
          width: fit-content;
          margin-left: .5em;
          padding: 0 .2em;
        }}
        ul {{
          padding-left: 1.5em;
        }}
        table {{
          background-color: seashell;
          border-collapse: collapse;
        }}
        table,
        thead,
        tbody {{
          border: 2px black solid;
        }}
        th {{
          border-left: 2px black solid;
          border-right: 2px black solid;
          border-top: 1px darkgray solid;
          border-bottom: 1px darkgray solid;
        }}
        td {{
        border: 1px darkgray solid;
        }}
        th,
        td {{
          padding: 2px 3px;
        }}
        tr:first-child > th:nth-child(2),
        tr:not(:first-child) > th:first-child,
        tr:first-child > td:nth-child(4),
        tr:not(:first-child) > td:nth-child(3){{
         text-align: right;
        }}</style>
    </head>
    <body><table>{Seq(thead, tbodysAndEdges.map(_._1))}</table></body>
    <section>
    <h2>Edges</h2>
    {tbodysAndEdges.map(_._2)}
    </section>
  </html>
  val doctypeHtml: scala.xml.dtd.DocType = DocType("html") // used for single-column and mixed output
  val dependencyTablePath =
    os.pwd / "src" / "main" / "outputs" /
      s"dependency-graph-table-$hgId.xhtml"
  scala.xml.XML.save(dependencyTablePath.toString, h, "UTF-8", true, doctypeHtml)

  val targets = hg.hyperedges
    .map(e => hg.members(e))
    .map(_.map(f => tm.minAfter(f.start + 1).get).map(_._2))
  val edges = hg.hyperedges
    .zip(targets)
    .flatMap((source, targets) => targets.map(target => Graph.edge(source, target)))
  val dependencyGraph = edges.foldLeft(Graph.empty[String])(_ + _)
  dependencyGraph

def dependencyGraphToDot(
    depGraph: Graph[String],
    hg: Hypergraph[String, TokenRange]
)(using gTa: Vector[Token]): String =
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
    .diff(Set("starts", "ends"))
    .map(k => k -> Vector("\"", k, ": ", hg.members(k).head.tString, "\"").mkString)
    .toMap ++ Map("starts" -> "starts", "ends" -> "ends")
  val dotEdges = edges
    .map((k, v) => k + " -> " + v)
    .mkString(";\n\t")
  val dotNodes = ";\n\t" + readings
    .map((k, v) => Vector(k, "[label=", v, "]").mkString)
    .mkString(";\n\t")

  prologue + dotEdges + dotNodes + epilogue

@main def tm(): Unit =
  val sepRegex = """Sep\d+"""
  val seps = gTa.filter(_.t matches sepRegex)
  val starts = Token("Sep-1", "Sep-1", -1, -1) +: seps
  val ends = seps :+ Token("Sep" + gTa.size.toString, "", 5, gTa.size)
  val heStarts = Hypergraph.hyperedge("starts", starts.map(e => TokenRange(e.g, e.g)): _*)
  val heEnds = Hypergraph.hyperedge("ends", ends.map(e => TokenRange(e.g, e.g)): _*)
  val hgWithStarts = Vector(hg1 + heStarts, hg2 + heStarts)
  val tmWithEnds = Vector(hg1 + heEnds, hg2 + heEnds).map(createTreeMap)
  val dependencyGraphs: Vector[Graph[String]] =
    hgWithStarts
      .zip(tmWithEnds)
      .map((hg, tm) => createDependencyGraph(hg, tm))
  val dots = dependencyGraphs
    .zip(Vector(hg1, hg2))
    .map((dg, hg) => dependencyGraphToDot(dg, hg))
