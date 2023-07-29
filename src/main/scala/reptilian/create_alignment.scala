package reptilian

/** Input is Vector[Int], representing FullDepthBlock instances
 * Output is Vector[AlignmentNode], where the nodes are all of
 * type ReadingNode
 *
 * Will need to deal with non-full-depth locations in the alignment
 * */

def blocks_to_nodes(blocks: Iterable[FullDepthBlock]): Iterable[ReadingNode] =
  blocks
    .map(fullDepthBlock_to_ReadingNode)
def fullDepthBlock_to_ReadingNode(block: FullDepthBlock): ReadingNode =
  val readings = block.instances
    .zipWithIndex
    .map((start, witness_no) =>
      "w" + witness_no.toString -> Tuple2(start, start + block.length))
    .toMap
  ReadingNode(readings)

