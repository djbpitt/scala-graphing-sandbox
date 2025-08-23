package net.collatex.reptilian

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.text.Normalizer
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.Map
import scala.util.Try

object FontMetricsCalculator {

  /** Sentinel for missing glyph */
  private final val MissingWidthSentinel: Float = -1f

  /** Immutable font metrics table with memoized string widths */
  final case class FontMetrics(widths: Map[Int, Float]) {

    // Concurrent cache for memoized string widths
    private val cache = new ConcurrentHashMap[String, Float]()

    /** Get width of a single character code point, normalized to NFC. Throws an exception if the glyph is missing.
      */
    private def charWidthOrError(codePoint: Int): Float =
      widths.get(codePoint) match
        case Some(w) if w != MissingWidthSentinel => w
        case _ =>
          val name = FontMetricsCalculator.unicodeName(codePoint)
          throw new IllegalStateException(
            f"Missing font metrics for character: U+$codePoint%04X ($name)"
          )

    /** Get total width of a string, normalized to NFC. Throws an exception listing all missing glyphs if any are
      * absent.
      */
    def stringWidth(s: String): Float = {
      val normalized = Normalizer.normalize(s, Normalizer.Form.NFC)

      Option(cache.get(normalized)).getOrElse {
        val codePoints = normalized.codePoints().toArray

        // Identify missing glyphs and count occurrences
        val missingCounts: Map[Int, Int] =
          codePoints.foldLeft(Map.empty[Int, Int]) { (acc, cp) =>
            if !widths.get(cp).exists(_ != MissingWidthSentinel) then acc.updated(cp, acc.getOrElse(cp, 0) + 1)
            else acc
          }

        if missingCounts.nonEmpty then
          val details = missingCounts
            .map { case (cp, count) =>
              val name = unicodeName(cp)
              if count > 1 then f"U+$cp%04X ($name) x$count"
              else f"U+$cp%04X ($name)"
            }
            .mkString(", ")
          throw new IllegalStateException(
            s"Missing glyphs in font: $details"
          )

        // Compute total width
        val computed = codePoints.map(charWidthOrError).sum
        cache.put(normalized, computed)
        computed
      }
    }
  }

  /** Build font metrics from a given Java AWT Font */
  def buildMetrics(font: Font): FontMetrics = {
    val frc = new FontRenderContext(new AffineTransform(), true, true)
    val widths: Map[Int, Float] = (0 to Char.MaxValue).map { ch =>
      val str = String.valueOf(ch)
      val normalized = Normalizer.normalize(str, Normalizer.Form.NFC)
      val w = font.getStringBounds(normalized, frc).getWidth.toFloat
      normalized.codePointAt(0) -> (if w > 0 then w else MissingWidthSentinel)
    }.toMap
    FontMetrics(widths)
  }

  /** Get Unicode character name (Java 9+), used for error messages */
  private def unicodeName(codePoint: Int): String =
    Try(Character.getName(codePoint)).getOrElse("UNKNOWN CHARACTER")
}
