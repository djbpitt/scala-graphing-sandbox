package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering

opaque type Siglum = String
object Siglum:
  def apply(s: String): Siglum = s
  extension (s: Siglum) {
    def value: String = s
  }
  given Ordering[Siglum] =
    (a: Siglum, b: Siglum) => a.value.compare(b.value)
given CanEqual[Siglum, Siglum] = CanEqual.derived

type WitnessReadings = Map[Siglum, TokenRange] // type alias

sealed trait AlignmentUnit // supertype AlignmentRibbon (with children) and AlignmentPoint (with groups)

final case class AlignmentPoint(witnessReadings: WitnessReadings, witnessGroups: Set[WitnessReadings])
    extends AlignmentUnit

/** Custom constructor to simplify creation of AlignmentPoint
  *
  * Input is a varargs of (Siglum, TokenRange). Will eventually create only WitnessGroups and no WitnessReadings
  */
object AlignmentPoint {
  def apply(gTa: Vector[TokenEnum], m: (Siglum, TokenRange)*): AlignmentPoint =
    val wr = m.toMap
    val wg = wr
      .groupBy((_, offsets) =>
        gTa
          .slice(offsets.start, offsets.until)
          .map(_.n)
      ) // groups readings by shared text (n property); can we improve the performance here?
      .values // we don't care about the shared text after we've used it for grouping
      .toSet
    AlignmentPoint(wr, wg)
}

/** Zone to be processed in phase two
  *
  * Same input as AlignmentPoint (varargs of (Siglum, TokenRange)) but create only WitnessReadings and no WitnessGroups
  */
final case class UnalignedZone(witnessReadings: WitnessReadings) extends AlignmentUnit

object UnalignedZone {
  def apply(m: (Siglum, TokenRange)*): UnalignedZone =
    val wr = m.toMap
    UnalignedZone(wr)
}

/** AlignmentRibbon
  *
  * Container for all alignment units
  *
  * @param children
  *   ListBuffer of AlignmentUnit (AlignmentPoint | UnalignedZone)
  */
final case class AlignmentRibbon(
    children: ListBuffer[AlignmentUnit] = ListBuffer.empty
) extends AlignmentUnit

def blocksToNodes(
    blocks: Iterable[FullDepthBlock],
    lTa: Vector[TokenEnum],
    gTa: Vector[TokenEnum],
    sigla: List[Siglum]
): Iterable[AlignmentPoint] =
  val result = blocks
    .map(e => fullDepthBlockToAlignmentPoint(e, gTa, sigla))
  result
// Convert local alignment offsets to global token-array offsets for the reading node
def fullDepthBlockToAlignmentPoint(
    block: FullDepthBlock,
    lTa: Vector[TokenEnum], // local
    sigla: List[Siglum]
): AlignmentPoint =
//  println(s"block: $block")
  val readings = block.instances
    .map(e =>
      sigla(lTa(e).w) -> TokenRange(
        start = lTa(e).g,
        until = lTa(
          e
        ).g + block.length,
        ta = lTa
      )
    )
    .toMap
  val wg = Set(readings)
  AlignmentPoint(readings, wg)
