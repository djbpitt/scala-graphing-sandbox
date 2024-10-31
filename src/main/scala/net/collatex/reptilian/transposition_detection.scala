package net.collatex.reptilian

import net.collatex.util.Hypergraph
import scala.collection.immutable.{SortedMap, TreeMap}

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
 *    0. Every hyperedge in the hypergraph becomes a node (use the hyperedge label as the
 *       identifier) in the dependency graph. Like a variant graph, a dependency graph needs
 *       a start and an end, which need to have the start and end offsets in the global token
 *       array of the starts and ends. This provides the lowest and highest offsets for each
 *       witness, which we need in order to know where they start and stop in the global token
 *       array. (NB: Create separators in the global token array to facilitate finding the
 *       seams between witnesses.
 *    a. Create 'fake' hyperedges for the start and end nodes and find the start and end
 *       coordinates in the global token array. Start and end nodes must contain all witnesses.
 *    b. Go over the hyperedges in each hypergraph to add the hypergraphs to a sorted
 *       Map[Int, String], where Int is the position where a token range starts in the global
 *       token array and String is the edge label where the data comes from.
 *    d. Create the outgoing edges for the dependency graph by going over the hyperedges and
 *       using the sorted to determine what the next item in the map is for the positions in
 *       each of the witnesses.
 *    This finds the hyperedges, which are the nodes in the dependency graph to connect to.
 *    De-duplicate the edges because when witnesses will point to the same ones.
 * 2. Topological sort the dependency graph
 * 3. Rank the hyperedges
 * 4. Create a traversal/decision graph for the traversal of the two sorted and ranked hyperedges
 * 5. Beam search the traversal graph
 *
 * Later optimization: We can determmine the relative order of two blocks for a hyperedge that
 * appears in both blocks.
 * */

// Data
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

@main def tm(): Unit =
  val result = Vector(hg1, hg2).map(createTreeMap)
  val next = result.head.minAfter(255 + 1)
  println(result)
  println(next)
