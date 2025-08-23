package net.collatex.reptilian

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.text.Normalizer
import scala.collection.immutable.Map
import scala.collection.concurrent.TrieMap

/** Helper object to verify that a font is installed and return a Font instance. */
object FontUtils {

  /** Returns a Font instance for the given font name and size. Throws IllegalArgumentException if the font is not
    * installed.
    */
  def fontOrError(name: String, size: Int): java.awt.Font = {
    val availableFonts = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
    if !availableFonts.contains(name) then throw new IllegalArgumentException(s"Font '$name' is not installed")
    new java.awt.Font(name, java.awt.Font.PLAIN, size)
  }
}

object FontMetricsCalculator {

  /** Sentinel for missing glyph */
  private val MissingWidthSentinel: Float = -1f

  /** Immutable font metrics table with memoized string widths */
  // Replace with unboxed Array[Float] if performance becomes an issue
  case class FontMetrics(widths: Map[Int, Float]) {

    // Thread-safe cache for memoized string widths (idiomatic Scala)
    protected val cache: TrieMap[String, Float] = TrieMap.empty[String, Float]

    /** Get width of a single character code point. Throws an exception if the glyph is missing.
      */
    private def charWidthOrError(codePoint: Int): Float =
      widths.get(codePoint) match
        case Some(w) if w != MissingWidthSentinel => w
        case _ =>
          val name = FontMetricsCalculator.unicodeName(codePoint)
          throw new IllegalStateException(f"Missing font metrics for character: U+$codePoint%04X ($name)")

    /** Get total width of a string, normalized to NFC. Throws an exception listing all missing glyphs (with counts) if
      * any are absent.
      */
    def stringWidth(s: String): Float = {
      val normalized = Normalizer.normalize(s, Normalizer.Form.NFC)

      cache.getOrElseUpdate(
        normalized, {
          val codePoints = normalized.codePoints().toArray

          // Identify missing glyphs and count occurrences
          val missingCounts: Map[Int, Int] =
            codePoints.foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { (acc, cp) =>
              if !widths.get(cp).exists(_ != MissingWidthSentinel) then acc.updated(cp, acc(cp) + 1)
              else acc
            }

          if missingCounts.nonEmpty then
            val details = missingCounts
              .map { case (cp, count) =>
                val name = unicodeName(cp)
                if count > 1 then f"U+$cp%04X ($name) x$count" else f"U+$cp%04X ($name)"
              }
              .mkString(", ")
            throw new IllegalStateException(s"Missing glyphs in font: $details")

          // Compute total width (all glyphs present)
          codePoints.iterator.map(charWidthOrError).sum
        }
      )
    }
  }

  /** Build font metrics from a given font name and size (verifies font exists).
   * Throws via helper if font not found */
  def buildMetrics(fontName: String, fontSize: Int): FontMetrics = {
    val font = FontUtils.fontOrError(fontName, fontSize)
    val frc = new FontRenderContext(new AffineTransform(), true, true)

    val widths: Map[Int, Float] = (0 to Char.MaxValue).map { ch =>
      val cp = ch
      val str = new String(Character.toChars(cp))
      val normalized = Normalizer.normalize(str, Normalizer.Form.NFC)

      val w =
        if font.canDisplay(cp) then font.getStringBounds(normalized, frc).getWidth.toFloat
        else MissingWidthSentinel

      cp -> w
    }.toMap

    FontMetrics(widths)
  }

  /** Get Unicode character name (Java 9+), used for error messages. Character.getName may return null for unassigned
    * code points; handle that safely.
    */
  private def unicodeName(codePoint: Int): String =
    Option(Character.getName(codePoint)).getOrElse("UNKNOWN CHARACTER")
}
