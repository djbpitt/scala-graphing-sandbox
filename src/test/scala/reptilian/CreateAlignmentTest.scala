package reptilian

import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

class CreateAlignmentTest extends AnyFunSuite:
  private val fdb = FullDepthBlock(Vector(0, 4, 8), 2)
  private val result = fullDepthBlock_to_ReadingNode(fdb)

  test("Map from FullDepthBlock to ReadingNode") {
    val result = fullDepthBlock_to_ReadingNode(fdb)
    val expected = Map("w0" -> (0,2), "w1" -> (4,6), "w2" -> (8,10))
    assert(result == expected)
  }



