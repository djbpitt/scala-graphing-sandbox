package reptilian

import os.Path

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex // Create tokenization regex here but tokenize in tokenization.scala

/** Read data files from supplied path to directory (one file per witness)
 *
 * @param path_to_data os.Path object that points to data directory
 * @return Indexed sequence of lists of strings (token lists)
 */
def read_data(path_to_data: Path): List[String] =
  os.walk(path = path_to_data, skip = _.last.startsWith(".")) // exclude hidden files
    .sorted
    .toList
    .map(os.read)

@main def main(): Unit =
  // Prepare tokenizer (partially applied function)
  // NB: Sequences of non-word characters (except spaces) are entire tokens
  // Unlike in CollateX Python, punctuation characters are their own tokens
  val token_pattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = make_tokenizer(token_pattern) // Tokenizer function with user-supplied regex
  // Prepare data (List[String])
  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  //  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin_small" // no skip edge; direct transposition
  // val path_to_darwin = os.pwd / "src" / "main" / "data" / "cats"
  // Small skip edge test examples
  //  val path_to_darwin = os.pwd / "src" / "main" / "data" / "no_skip_cats" // no skip edge; direct transposition
  // val path_to_darwin = os.pwd / "src" / "main" / "data" / "one_skip_cats" // one skip edge
  // val path_to_darwin = os.pwd / "src" / "main" / "data" / "two_skip_cats" // two (parallel) skip edges
  // End of skip edge test examples
  val witness_strings = read_data(path_to_darwin) // One string per witness
  implicit val token_array: Vector[Token] = tokenize(tokenizer)(witness_strings)
  // Find blocks (vectorize, create suffix array and lcp array, create blocks, find depth)
  val (all_blocks, tmp_suffix_array, longest_full_depth_nonrepeating_blocks) = create_aligned_blocks(token_array, witness_strings.size)
  implicit val suffix_array: Array[Int] = tmp_suffix_array
  val block_texts: Map[Int, String] = block_text_by_id(longest_full_depth_nonrepeating_blocks, token_array)

  val blockRangeSeq = createRangedSeq(all_blocks) // Finger tree

  // create navigation graph and filter out transposed nodes
  val graph = create_traversal_graph(longest_full_depth_nonrepeating_blocks.toVector)

  //  val set_of_non_transposed_node_ids = find_optimal_alignment(graph).toSet
  val set_of_non_transposed_node_ids = Set[Int]()

  val alignment = find_optimal_alignment(graph) // Int identifiers of full-depth blocks

  val alignment_as_set = alignment.toSet
  val alignment_blocks = longest_full_depth_nonrepeating_blocks
    .filter(e => alignment_as_set.contains(e.instances.head))

  val reading_nodes = blocks_to_nodes(alignment_blocks)
  var root = tree(witness_count = witness_strings.size)
  val sorted_reading_nodes = reading_nodes // Sort reading nodes in token order
    .toVector
    .sortBy(_.witness_readings("w0")._1)
  val sigla = sorted_reading_nodes.head.witness_readings.keys.toList // Humiliating temporary step
  /* For each sliding pair of reading nodes create an unexpanded node with witness readings
  *   that point from each siglum to a slice from the end of the first reading node to the
  *   start of the second. */
  val unaligned_intermediates = sorted_reading_nodes
    .sliding(2)
    .map(pair =>
      val map_entries = sigla
        .map(siglum => siglum -> (pair.head.witness_readings(siglum)(1), pair(1).witness_readings(siglum)(0)))
        .toMap
      UnexpandedNode(map_entries.filterNot(e => e._2._1 == e._2._2))
    )
  // Used to check for unaligned leading or trailing tokens
  // Possibly unnecessary traversal of token array
  // Can we find the first and last tokens of each witness without a separate traversal?
  val boundary_tokens =
    token_array
      .map(_.t)
      .zipWithIndex
      .filter(e => e._1.contains(" #"))
      .map(_._2)
  val first_tokens = Vector(0) ++ boundary_tokens.map(_ + 1)
  val last_tokens = boundary_tokens.map(_ - 1) ++ Vector(token_array.size - 1)
  val leading_tokens = sigla
    .sorted
    .zipWithIndex
    .map(e => e._1 -> (first_tokens(e._2), sorted_reading_nodes.head.witness_readings(e._1)(0)))
    .toMap
  val leading_deltas: Boolean = leading_tokens
    .values
    .map(e => e._2 - e._1)
    .sum != 0
  val leading_unexpanded: Option[UnexpandedNode] =
    if leading_deltas then
      Some(UnexpandedNode(leading_tokens))
    else
      None
  val trailing_tokens = sigla
    .sorted
    .zipWithIndex
    .map(e => e._1 -> (sorted_reading_nodes.last.witness_readings(e._1)(1), last_tokens(e._2)))
    .toMap
  val trailing_deltas: Boolean = trailing_tokens
    .values
    .map(e => e._2 + 1 - e._1) // Range points *after* last token, so add 1
    .sum != 0
  val trailing_unexpanded: Option[UnexpandedNode] =
    if trailing_deltas then
      Some(UnexpandedNode(trailing_tokens))
    else
      None
  val reading_and_intermediate_nodes = sorted_reading_nodes
    .zip(unaligned_intermediates)
    .flatMap(_.toList) ++ List(sorted_reading_nodes.last)

  val all_block_ranges = all_blocks
    .map(
      (b: Block) =>
        (suffix_array.slice(b.start, b.end).toList, b.length)
    )
    .map(e => e._1.map(f => (f, f + e._2)))
//  all_block_ranges.foreach(println)

  val new_children: ListBuffer[AlignmentTreeNode] =
      ListBuffer(leading_unexpanded).flatten
    new_children.appendAll(reading_and_intermediate_nodes)
    new_children.appendAll(List(trailing_unexpanded).flatten)
    root = RootNode(new_children)

  val newer_children =
    new_children.map {
      case e: UnexpandedNode =>
        val node_ranges = e.witness_readings.values
        all_block_ranges.filter(_.size == node_ranges.size)
      case e: ReadingNode => "R"
      case _ => "Oops" // Shouldn't happen
    }

  /** Create views of tree
   *
   * Graphviz dot file
   * HTML alignment table
  * */
  val alignment_tree_as_dot = dot(root, token_array)
  val alignmentGraphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
  os.write.over(alignmentGraphOutputPath, alignment_tree_as_dot)

  val output = create_alignment_table(root, token_array, sigla)
  val outputPath = os.pwd / "src" / "main" / "output" / "traversal-alignment.xhtml"
  os.write.over(outputPath, output)

  // Diagnostic: visualize traversal graph
  val traversal_graph_as_dot = traversal_graph_to_dot(graph, block_texts, set_of_non_transposed_node_ids)
  val graphOutputPath = os.pwd / "src" / "main" / "output" / "traversal.dot"
  os.write.over(graphOutputPath, traversal_graph_as_dot) // Create HTML output and write to specified path

