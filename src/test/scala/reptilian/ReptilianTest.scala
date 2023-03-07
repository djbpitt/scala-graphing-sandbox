package reptilian

import org.scalatest.funsuite.AnyFunSuite

class ReptilianTest extends AnyFunSuite:

  // tests for lcp interval
  /** lcp array always starts with -1
   *  If all other values are 0 should find no lcp intervals
   *  Could happen if witnesses are dissimilar and there is no repetition
   */
  test("lcp array with all zero values should return no intervals") {
    val input = Array[Int](-1, 0, 0, 0, 0, 0)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result.isEmpty)
  }
end ReptilianTest

