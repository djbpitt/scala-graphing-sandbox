package reptilian

import org.scalatest.funsuite.AnyFunSuite

class ReptilianTest extends AnyFunSuite:

  /** tests for lcp interval
   *  lcp array always starts with -1
   */

  //Could happen if witnesses are dissimilar and there is no repetition
  test("lcp array with all zero values should return no intervals") {
    val input = Array[Int](-1, 0, 0, 0, 0, 0)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result.isEmpty)
  }
  //First value is offset before new interval, second is position of last member, third is length
  test("lcp array with all same values (after first) should return one interval") {
    val input = Array[Int](-1, 1, 1, 1, 1)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result == List(Block(0, 4, 1)))
  }
  //First interval, with value 1, runs to end because 1 < 2
  test("lcp array with two adjacent intervals should return … er … two adjacent intervals") {
    val input = Array[Int](-1, 1, 1, 1, 2, 2, 2)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result == List(Block(3, 6, 2), Block(0, 6, 1)))
  }
  //Second interval, with value 1, runs from start to end because 1 < 2
  test("lcp array with values that go down can also return two adjacent intervals") {
    val input = Array[Int](-1, 2, 2, 2, 1, 1, 1)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result == List(Block(0, 3, 2), Block(0, 6, 1)))
  }
  //The behavior for this is wrong in CollateX Java, and perhaps also Python
  //Fixed in reptilian Python, although not tested there
  test("lcp array with values that go up and down has two intervals, one full length") {
    val input = Array[Int](-1, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result == List(Block(3, 6, 2), Block(0, 9, 1)))
  }

  test("test that one or more zeroes at the end of an LCP array closes all open blocks and doesn't cause any zero length blocks to be created.") {
    val input = Array[Int](-1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 0, 0)
    val result = splitLCP_ArrayIntoIntervals(input)
    assert(result == List(Block(3, 6, 2), Block(0, 9, 1)))
  }


end ReptilianTest

