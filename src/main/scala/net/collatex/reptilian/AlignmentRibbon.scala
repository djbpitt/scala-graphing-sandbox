package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import net.collatex.reptilian.TokenRange.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
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

/** Zone to be processed in phase two
 *
 * Same input as AlignmentPoint (varargs of (Siglum, TokenRange)) but create only WitnessReadings and no WitnessGroups
 */
final case class UnalignedZone(witnessReadings: WitnessReadings, global: Boolean) extends AlignmentUnit:
  def convertToTokenLists(): List[List[Token]] =
    val sortedKeys = this.witnessReadings.keys.toSeq.sorted
    sortedKeys.map(e => this.witnessReadings(e).tokens.map(_.asInstanceOf[Token]).toList).toList

  def splitUnalignedZone(
      alignment_point_for_split: AlignmentPoint,
      global: Boolean
    ): (UnalignedZone, UnalignedZone) =

    // We filter out all the witnesses that have an empty range after the split
    val preAndPost = this.witnessReadings
      .map((k, v) => k -> v.splitTokenRange(alignment_point_for_split.witnessReadings(k)))
    val unfilteredPre = preAndPost.map((k, v) => k -> v._1)
    val unfilteredPost = preAndPost.map((k, v) => k -> v._2)
    val pre  = removeEmptyTokenRanges(unfilteredPre)
    val post = removeEmptyTokenRanges(unfilteredPost)
    (UnalignedZone(pre, global), UnalignedZone(post, global))

  private def removeEmptyTokenRanges(before: Map[Siglum, TokenRange]): Map[Siglum, TokenRange] =
    before.filter((_, v) => v.isInstanceOf[LegalTokenRange])

  def createLocalTokenArrayForUnalignedZone: Vector[TokenEnum] =
    // Create a local token array by filtering the global one
    // Selection comes in unsorted, so sort by siglum first
    val localTokenArrayByWitness: Seq[Vector[TokenEnum]] = {
      val orderedWitnessReadings =
        for siglum <- this.witnessReadings.keys.toSeq.sorted
          yield this.witnessReadings(siglum)
      for r <- orderedWitnessReadings yield r.tokens
    }
    // Replacement that uses witnessGroups instead of witnessReadings
    val lTa = localTokenArrayByWitness.head ++
      localTokenArrayByWitness.tail.zipWithIndex
        .flatMap((e, index) =>
          Vector(
            TokenSep(t = s" #$index ", n = s" #$index ", w = index, g = index)
          ) ++ e
        )
    lTa


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

  def apply(gTa: Vector[TokenEnum], m: WitnessReadings): AlignmentPoint =
    val wg = m
      .groupBy((_, offsets) =>
        gTa
          .slice(offsets.start, offsets.until)
          .map(_.n)
      ) // groups readings by shared text (n property); can we improve the performance here?
      .values // we don't care about the shared text after we've used it for grouping
      .toSet
    AlignmentPoint(m, wg)

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

