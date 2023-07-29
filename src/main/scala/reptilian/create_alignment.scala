package reptilian

/** Input is Vector[Int], representing FullDepthBlock instances
 *  Output is Vector[AlignmentNode], where the nodes are all of
 *    type ReadingNode
 *
 *  Will need to deal with non-full-depth locations in the alignment
 * */

def Blocks_to_Nodes(blocks: Iterable[FullDepthBlock]) =
  blocks
    .map(fullDepthBlock_to_ReadingNode)
def fullDepthBlock_to_ReadingNode(block: FullDepthBlock): WitnessReadings =
  block.instances
    .zipWithIndex
    .map((start, witness_no) =>
      "w" + witness_no.toString -> Tuple2(start, start + block.length))
    .toMap

@main def tmp(): Unit =
  val fdb = FullDepthBlock(Vector(0, 4, 8), 2)
  val result = fullDepthBlock_to_ReadingNode(fdb)
  println(result)
