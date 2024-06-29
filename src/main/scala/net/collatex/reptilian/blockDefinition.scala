package net.collatex.reptilian

/** Block is an lcp interval
 *
 * @param start  start position in suffix array
 * @param end    until position in suffix array
 * @param length number of tokens in prefix
 *
 *               width = until - start (number of instances)
 *               if one per witness, block is full-depth, but could be repetition within a single witness
 */
case class Block(start: Int, end: Int, length: Int)

/** Full depth block
 *
 * @param instances : start positions of all instances (at least two) in enhanced token array
 *                  (incorporates token witness membership information)
 *                  Sort in order of witnesses during construction
 * @param length    : length of pattern
 *
 *                  Start position plus length makes it possible to compute until positions, if needed
 *                  We use this remove shorter embedded blocks
 *                  This plus token array is enough for all subsequent processing; no further need for suffix array, etc.
 */
case class FullDepthBlock(instances: Vector[Int], length: Int):
  def show(using gTa: Vector[Token]): String =
    gTa
      .slice(this.instances(0), this.instances(0) + this.length)
      .map(_.n)
      .mkString(" ")

case class OpenBlock(start: Int, length: Int)


