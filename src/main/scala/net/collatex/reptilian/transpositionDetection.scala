package net.collatex.reptilian
import net.collatex.util.Hypergraph
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
 * 0. Calculate the matches between the two hypergraphs or get them as input into this transposition detection function
 * 1. Create a dependency graph (DAG) for each of the hyper-graphs.
 * 2. Rank the nodes in the two dependency graphs, this is done by a topological sort of each dependency graph
 * 3. Sort the matches first in the order of the first dependency graph, then in the order of the second
 * 4. Create a traversal/decision graph for the traversal of the two sorted lists of matches
 * 5. Beam search the traversal graph to create the alignment (resolving transpositions)
 * */

// SetOf2 preserves insertion order, so head is necessarily Hg1 and last is necessarily Hg2
// TODO: 2025-11-20 Transpositions are now (okay, will soon be) automatically avoided during traversal, so a separate
//  detectTransposition() function is no longer needed
def detectTransposition(
    matchesAsSet: Set[HyperedgeMatch],
    matchesAsHg: Hypergraph[EdgeLabel, TokenRange]
): (Boolean, Seq[HyperedgeMatch], Seq[HyperedgeMatch]) =
  if matchesAsSet.size > 1 // more than one block means possible transposition
  then
    val ranking: Map[NodeType, Int] = matchesAsHg.rank()
    val matchesSortedHead =
      matchesAsSet.toSeq.sortBy(e => ranking(NodeType(e.head.label)))
    val matchesSortedLast =
      matchesAsSet.toSeq.sortBy(e => ranking(NodeType(e.last.label)))
    val transpositionBool =
      matchesSortedHead != matchesSortedLast // Expensive, especially with no transposition (= most of the time)
    (transpositionBool, matchesSortedHead, matchesSortedLast)
  else (false, matchesAsSet.toList, matchesAsSet.toList) // Single item lists
