package net.collatex.reptilian
import net.collatex.util.{Hypergraph, hypergraphToReadings}

import net.collatex.reptilian.returnSampleData

import scala.math.Ordering

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
 * !. Calculate the matches between the two hypergraphs or get them as input into this transposition detection function
 * 1. Create a dependency graph (DAG) for each of the hyper-graphs.
 * 2. Rank the nodes in the two dependency graphs, this is done by a topological sort of each dependency graph
 * 3. Sort the matches first in the order of the first dependency graph, then sort the matches in the order in the second dependency graph
 * 4. Create a traversal/decision graph for the traversal of the two sorted lists of matches
 * 5. Beam search or a-star search the traversal graph to create the alignment (resolving transpositions)
 *
 * Later optimization: We can determine the relative order of two blocks for a hyperedge that
 * appears in both blocks.
 * */

// NOTE: This method is both incorrect and incomplete!
// It is incorrect because the two hypergraphs are merged, which means that in a case of a
//  transposition the ranking will fail because of a cycle.
// TODO: Check or am I wrong about that? Would the dependency graph just split into two separate paths from the root?
// Also this implementation assumes that a HyperedgeMatch head is always from the first hypergraph and last is always from
// the second hypergraph. However I don't think SetOf2 has any such guarantee. But if point one is not a problem, then this concern
// disappears.
// The method is incomplete because no graph traversal is done!
// TODO: We are building a traversal graph in secondAlignmentPhase traversalGraph2. That functionality should probably go here.
def detectTransposition(
    matchesAsSet: Set[HyperedgeMatch],
    matchesAsHg: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean
): Boolean =
  if matchesAsSet.size > 1 // more than one block means possible transposition
  then
    val ranking: Map[NodeType, Int] = matchesAsHg.rank(debug)
    val matchesSortedHead =
      matchesAsSet.toSeq.sortBy(e => ranking(NodeType(e.head.label)))
    val matchesSortedLast =
      matchesAsSet.toSeq.sortBy(e => ranking(NodeType(e.last.label)))
    val transpositionBool = matchesSortedHead != matchesSortedLast
    if transpositionBool then
      println("Found a transposition")
      println(matchesSortedHead.map(_.head.label))
      println(matchesSortedLast.map(_.head.label))
    transpositionBool
  else false

def realMainFunction(debug: Boolean): Unit =
  val (_, hg1, hg2) = returnSampleData() // don’t use (global) names of hgs because real data isn’t global
  val hgWithMergeResults: Hypergraph[EdgeLabel, TokenRange] = mergeHgHg(hg1, hg2, debug)
  val result = hypergraphToReadings(hgWithMergeResults)
  // println(result)

@main def runWithSampleData(): Unit = // no files saved to disk
  realMainFunction(false)

@main def runWithSampleDataDebug(): Unit = // dot and html saved to disk
  realMainFunction(true)

