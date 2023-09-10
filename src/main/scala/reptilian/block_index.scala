package reptilian

def createRangedSeq(all_blocks: List[Block])(implicit suffix_array: Array[Int]) =
  val blockList = all_blocks // First create list of all blocks
    .flatMap(e => e.instanceStartOffsets // start offsets of block instances in token array
      .map(f => ((f, f + e.length), e))) // start and stop offsets plus original block object

  // Treat list of blocks as varargs (type ascription), which constructor requires
  val blockRangeSeq = myRangedSeq(blockList: _*)(_._1, Ordering.Int)
  blockRangeSeq
// println(blockRangeSeq.tree)

  
