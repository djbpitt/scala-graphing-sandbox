package math

import org.scalatest.funsuite.AnyFunSuite

class MathUtilsTest extends AnyFunSuite:

  // test 1
  test("'double' should handle 0") {
    val result = MathUtils.double(0)
    assert(result == 0)
  }

  // test 2
  test("'double' should handle 1") {
    val result = MathUtils.double(1)
    assert(result == 2)
  }

  test("test with Int.MaxValue") (pending)

end MathUtilsTest
