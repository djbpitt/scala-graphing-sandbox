package reptilian

import org.scalatest.funsuite.AnyFunSuite
import org.hammerlab.suffixes.dc3.make as calculate_suffix_array
import org.scalatest.matchers.should.Matchers._
import org.scalatest._
import Assertions._
import Inspectors._

import scala.util.matching.Regex

class ReptilianTest extends AnyFunSuite:

  /** Set up fixtures
   *
   */
  val tokens: Vector[Token] = Vector(
    Token("B ", "b", 0),
    Token("a ", "a", 0),
    Token("n ", "n", 0),
    Token("a ", "a", 0),
    Token("n ", "n", 0),
    Token("a ", "a", 0),
    Token("b ", "b", 0),
    Token("a ", "a", 0),
    Token("n ", "n", 0),
    Token("$", "$", 0)
  )

  /** tests for creating suffix array
   *
   */
  //Verify final output of suffix array
  test("suffix array returns correct suffixes and integers") {
    val (vectorization, voc_size) = vectorize(tokens)
    val suffix_positions = calculate_suffix_array(vectorization, voc_size)
    val suffixes: Array[Vector[String]] = suffix_positions.map(tokens.map(_.n).slice(_, suffix_positions.length))
    val target_suffixes = Array[String]("$", "aban$", "an$", "anaban$", "ananaban$", "ban$", "bananaban$", "n$", "naban$", "nanaban$")
    assert(suffix_positions sameElements Array(9, 5, 7, 3, 1, 6, 0, 8, 4, 2))
    val zs = suffixes.map(_.toString) zip target_suffixes.map(_.toVector).map(_.toString)
    forAll (zs) {(x, y) => assert(x == y)}
  }

  /** test for creating lcp array
   *
   */
  test("lcp array returns correct values") {
    val target_lcp_values = Vector[Int](-1, 0, 1, 2, 3, 0, 3, 0, 1, 2)
    val (vectorization, voc_size) = vectorize(tokens)
    val suffix_positions = calculate_suffix_array(vectorization, voc_size)
    val lcp_array = calculate_lcp_array(tokens, suffix_positions)
    val zs = lcp_array zip target_lcp_values
    forAll(zs) {(x, y) => assert(x == y)}
  }

  /** tests for computing lcp intervals
   * lcp array always starts with -1
   */

  //Could happen if witnesses are dissimilar and there is no repetition
  test("lcp array with all zero values should return no intervals") {
    val input = Vector[Int](-1, 0, 0, 0, 0, 0)
    val result = create_blocks(input)
    assert(result.isEmpty)
  }
  //First value is offset before new interval, second is position of last member, third is length
  test("lcp array with all same values (after first) should return one interval") {
    val input = Vector[Int](-1, 1, 1, 1, 1)
    val result = create_blocks(input)
    assert(result == List(Block(0, 4, 1)))
  }
  //First interval, with value 1, runs to end because 1 < 2
  test("lcp array with two adjacent intervals should return … er … two adjacent intervals") {
    val input = Vector[Int](-1, 1, 1, 1, 2, 2, 2)
    val result = create_blocks(input)
    assert(result == List(Block(3, 6, 2), Block(0, 6, 1)))
  }
  //Second interval, with value 1, runs from start to end because 1 < 2
  test("lcp array with values that go down can also return two adjacent intervals") {
    val input = Vector[Int](-1, 2, 2, 2, 1, 1, 1)
    val result = create_blocks(input)
    assert(result == List(Block(0, 3, 2), Block(0, 6, 1)))
  }
  //The behavior for this is wrong in CollateX Java, and perhaps also Python
  //Fixed in reptilian Python, although not tested there
  test("lcp array with values that go up and down has two intervals, one full length") {
    val input = Vector[Int](-1, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    val result = create_blocks(input)
    assert(result == List(Block(3, 6, 2), Block(0, 9, 1)))
  }

  test("lcp array with zeroes at end closes all open blocks and doesn't create zero length blocks") {
    val input = Vector[Int](-1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 0, 0)
    val result = create_blocks(input)
    assert(result == List(Block(3, 6, 2), Block(0, 9, 1)))
  }

  /** Tests for token-witness tracking
   *
   */

  test("Test token position to witness number mapping") {
    val input = List[String]("a a2", "b b2", "c c2")
    // prepare tokenizer
    val token_pattern: Regex = raw"\w+\s*|\W+".r // From CollateX Python, syntax adjusted for Scala
    val tokenizer = make_tokenizer(token_pattern) // Tokenizer function with user-supplied regex
    // create List[List[Str]] (one inner list per witness)
    val tokenized_input = input.map(tokenizer)
    val result = create_token_witness_mapping(tokenized_input)
    assert(result == Vector(0, 0, -1, 1, 1, -1, 2, 2))
  }

end ReptilianTest

