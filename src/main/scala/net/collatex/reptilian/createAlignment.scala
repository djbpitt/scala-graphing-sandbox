package net.collatex.reptilian

/** Input is Vector[Int], representing FullDepthBlock instances
 * Output is Vector[AlignmentNode], where the nodes are all of
 * type ReadingNode
 *
 * Will need to deal with non-full-depth locations in the alignment
 * */

def blocksToNodes(blocks: Iterable[FullDepthBlock]): Iterable[ReadingNode] =
  blocks
    .map(fullDepthBlockToReadingNode)
def fullDepthBlockToReadingNode(block: FullDepthBlock): ReadingNode =
  val readings = block.instances
    .zipWithIndex
    .map((start, witnessNo) =>
      "w" + witnessNo.toString -> Tuple2(start, start + block.length))
    .toMap
  ReadingNode(readings)

