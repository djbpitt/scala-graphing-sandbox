package net.collatex.reptilian.display

import java.awt.{Font, GraphicsEnvironment}
import java.awt.font.{FontRenderContext, TextAttribute, TextLayout}
import java.awt.geom.AffineTransform
import java.text.{AttributedString, Normalizer}
import java.util
import java.util.Locale
import scala.collection.concurrent.TrieMap
import scala.util.chaining.scalaUtilChainingOps

/** Text measurement utilities grouped under a single module namespace. */
object TextWidth:

  // -----------------------------
  // 1) Font utilities (exact match)
  // -----------------------------
  /** Return a Font if the exact family is installed; else throw with a helpful list. */
  object FontUtils {
    def fontOrError(familyExact: String, sizePt: Float = 16f, style: Int = java.awt.Font.PLAIN): java.awt.Font = {
      val installed = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment
        .getAvailableFontFamilyNames(java.util.Locale.ROOT)
        .toVector
        .sorted

      val key = familyExact.trim // <— NEW: tolerate incidental whitespace

      if (!installed.contains(key)) {
        val preview = installed.mkString("\n  - ")
        val msg =
          s"""Font family not installed: "$familyExact"
             |(After trimming: "$key")
             |Installed font families include:
             |  - $preview
             |""".stripMargin
        throw new IllegalArgumentException(msg)
      }

      new java.awt.Font(key, style, sizePt.toInt).deriveFont(sizePt)
    }
  }

  // --------------------------------------------
  // 2) Measurer: pure-looking façade (String => Float)
  // --------------------------------------------
  /** Pure façade over a fast, cached width function. */
  final case class Measurer private (measure: String => Float):
    def apply(word: String): Float = measure(word)

  object Measurer:

    /** For code points the font can’t display. */
    enum MissingGlyphStrategy:
      /** Throw an error listing the first missing code point. */
      case Strict

      /** Use the font’s own missing-glyph advance. */
      case UseFontMissingGlyph

      /** Replace missing code points with a per-font average advance. */
      case AverageAdvance

    /** Build a measurer for a specific font. Internals use TextLayout (kerning/ligatures) and a bounded LRU cache.
      */
    def forFont(
        font: Font,
        antialiased: Boolean = true,
        fractionalMetrics: Boolean = true,
        maxCacheSize: Int = 80_000,
        missingGlyph: MissingGlyphStrategy = MissingGlyphStrategy.UseFontMissingGlyph,
        units: TextWidth.Units = TextWidth.Units.Points
    ): Measurer =
      val frc = TextWidth.frcFor(antialiased, fractionalMetrics, units)

      final class Lru[K, V](max: Int) extends util.LinkedHashMap[K, V](math.max(16, max * 4 / 3), 0.75f, true):
        override def removeEldestEntry(e: util.Map.Entry[K, V]): Boolean = size() > max
      val cache = new Lru[String, java.lang.Float](maxCacheSize)

      // one-time, per-font “average glyph” calibration used only if AverageAdvance is selected
      lazy val averageAdvance: Float =
        val sample = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        val advances =
          sample.iterator
            .map(_.toString)
            .filter(s => font.canDisplayUpTo(s) < 0)
            .map { s =>
              val as = new AttributedString(s)
              as.addAttribute(TextAttribute.FONT, font)
              new TextLayout(as.getIterator, frc).getAdvance
            }
            .toArray
        if advances.nonEmpty then advances.sum / advances.length
        else // pathological font: fallback to “M”
          new TextLayout(
            new AttributedString("M").tap(_.addAttribute(TextAttribute.FONT, font)).getIterator,
            frc
          ).getAdvance

      def shapeWidth(text: String): Float =
        val as = new AttributedString(text)
        as.addAttribute(TextAttribute.FONT, font)
        as.addAttribute(TextAttribute.KERNING, TextAttribute.KERNING_ON)
        as.addAttribute(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
        new TextLayout(as.getIterator, frc).getAdvance

      def measure(word: String): Float =
        val nfc = Normalizer.normalize(word, Normalizer.Form.NFC)

        Option(cache.get(nfc)) match
          case Some(boxed) => boxed.floatValue()
          case None =>
            val idx = font.canDisplayUpTo(nfc)
            val width =
              if idx < 0 then
                // all glyphs displayable → shape full word (kerning/ligatures)
                shapeWidth(nfc)
              else
                missingGlyph match
                  case MissingGlyphStrategy.Strict =>
                    val cp = nfc.codePointAt(nfc.offsetByCodePoints(0, idx))
                    val name = Option(Character.getName(cp)).getOrElse("UNKNOWN")
                    throw new IllegalStateException(f"Missing glyph U+$cp%04X ($name) in ${font.getFamily}")
                  case MissingGlyphStrategy.UseFontMissingGlyph =>
                    // Let TextLayout use the font’s missing-glyph advance for the whole word
                    shapeWidth(nfc)
                  case MissingGlyphStrategy.AverageAdvance =>
                    // Approximate: sum advances of displayable code points; use average for missing
                    val it = nfc.codePoints().iterator()
                    var sum = 0f
                    while it.hasNext do
                      val cp = it.next()
                      val s = new String(Character.toChars(cp))
                      if font.canDisplay(cp) then
                        val as = new AttributedString(s)
                        as.addAttribute(TextAttribute.FONT, font)
                        sum += new TextLayout(as.getIterator, frc).getAdvance
                      else sum += averageAdvance
                    sum

            cache.put(nfc, java.lang.Float.valueOf(width))
            width

      Measurer(measure)

    // test-only factory with feature toggles
    def forFontWithFeatures(
        font: java.awt.Font,
        antialiased: Boolean = true,
        fractionalMetrics: Boolean = true,
        maxCacheSize: Int = 80_000,
        enableKerning: Boolean = true,
        enableLigatures: Boolean = true,
        units: TextWidth.Units = TextWidth.Units.Points
    ): Measurer = {
      val frc = TextWidth.frcFor(antialiased, fractionalMetrics, units)

      // Small, fast LRU (access-order) for word → width
      final class Lru[K, V](max: Int)
          extends java.util.LinkedHashMap[K, V](Math.max(16, max * 4 / 3), 0.75f, /*accessOrder*/ true) {
        override def removeEldestEntry(e: java.util.Map.Entry[K, V]): Boolean = size() > max
      }
      val cache = new Lru[String, java.lang.Float](maxCacheSize)

      // Measure whole string with the chosen feature toggles
      def shapeWidth(text: String): Float = {
        val as = new java.text.AttributedString(text)
        as.addAttribute(java.awt.font.TextAttribute.FONT, font)
        if (enableKerning) as.addAttribute(java.awt.font.TextAttribute.KERNING, java.awt.font.TextAttribute.KERNING_ON)
        if (enableLigatures)
          as.addAttribute(java.awt.font.TextAttribute.LIGATURES, java.awt.font.TextAttribute.LIGATURES_ON)
        new java.awt.font.TextLayout(as.getIterator, frc).getAdvance
      }

      def measure(word: String): Float = {
        val nfc = java.text.Normalizer.normalize(word, java.text.Normalizer.Form.NFC)

        Option(cache.get(nfc)) match {
          case Some(boxed) =>
            boxed.floatValue() // cache hit

          case None =>
            val w = shapeWidth(nfc) // miss → compute
            cache.put(nfc, java.lang.Float.valueOf(w))
            w
        }
      }

      new Measurer(measure)
    }

  // --------------------------------------------
  // 3) Pool: one measurer per exact family (size/style fixed here)
  // --------------------------------------------
  object Pool:
    private val pool = scala.collection.concurrent.TrieMap.empty[String, Measurer]

    private def normalizeFamily(f: String): String = f.trim

    def measurerForExactFamily(familyExact: String): Measurer =
      pool.getOrElseUpdate(
        normalizeFamily(familyExact), {
          val font = FontUtils.fontOrError(normalizeFamily(familyExact), sizePt = 16f)
          Measurer.forFont(font, units = Units.CssPx)
        }
      )

  // --------------------------------------------
  // 4) Optional syntax: scoped, terse usage inside a render block
  // --------------------------------------------
  object syntax:
    extension (s: String) def width(using m: Measurer): Float = m(s)

  // --------------------------------------------
  // 5) Support for font stack (for default font)
  // Application specifies wide serif stack: Georgia, Palatino, Palatino Linotype,serif
  // --------------------------------------------

  object FontStack {

    // Cache installed family names (non-localized) once
    private val installed: Set[String] =
      GraphicsEnvironment.getLocalGraphicsEnvironment
        .getAvailableFontFamilyNames(Locale.ROOT)
        .toSet

    // CSS logical → AWT logical
    private val logical: Map[String, String] = Map(
      "serif" -> "Serif",
      "sans-serif" -> "SansSerif",
      "monospace" -> "Monospaced"
    )

    private def stripQuotes(s: String): String =
      s.trim match {
        case t if t.startsWith("'") && t.endsWith("'") && t.length >= 2   => t.substring(1, t.length - 1)
        case t if t.startsWith("\"") && t.endsWith("\"") && t.length >= 2 => t.substring(1, t.length - 1)
        case t                                                            => t
      }

    /** Return the first installed family from a comma-separated CSS-style stack. Accepts exact family names and the CSS
      * logicals (serif/sans-serif/monospace). Throws with a helpful message if none are available.
      */
    def firstInstalledFamily(cssStack: String): String = {
      val tokens = cssStack
        .split(",")
        .iterator
        .map(stripQuotes)
        .map(_.trim)
        .filter(_.nonEmpty)

      val tried = scala.collection.mutable.ArrayBuffer.empty[String]

      tokens
        .map { t =>
          val mapped = logical.getOrElse(t.toLowerCase(Locale.ROOT), t)
          tried += t
          mapped
        }
        .find(fam => installed.contains(fam) || logical.values.exists(_ == fam)) // logicals are always present
        .getOrElse {
          val preview = installed.toVector.sorted.mkString(", ")
          throw new IllegalArgumentException(s"""No installed font found in stack: $cssStack
               |Tried (in order): ${tried.mkString(", ")}
               |Installed families include: $preview
               |""".stripMargin)
        }
    }

    /** Convenience: build a Font (points) from a stack. */
    def fontFromStack(cssStack: String, sizePt: Float = 16f, style: Int = Font.PLAIN): Font =
      TextWidth.FontUtils.fontOrError(firstInstalledFamily(cssStack), sizePt, style)

    /** Convenience: get a Measurer from a stack via the pool (uses your standard size). */
    def measurerFromStack(cssStack: String): TextWidth.Measurer =
      TextWidth.Pool.measurerForExactFamily(firstInstalledFamily(cssStack))
  }

  // --------------------------------------------
  // 6) Specify font in pt and get width returned in SVG px
  // --------------------------------------------

  enum Units:
    case Points, CssPx

  private[display] def frcFor(
      antialiased: Boolean,
      fractional: Boolean,
      units: Units
  ): java.awt.font.FontRenderContext =
    val tx = new java.awt.geom.AffineTransform()
    // Scale from points (72 dpi) to CSS px (96 dpi) when requested
    if units == Units.CssPx then tx.scale(96.0 / 72.0, 96.0 / 72.0)
    new java.awt.font.FontRenderContext(tx, antialiased, fractional)

  /* Default font is wide serif 16pt */
  val defaultFontStack = "Georgia, Palatino, 'Palatino Linotype', serif"
  val fontSize = 16f
  val defaultFamily: String =
    TextWidth.FontStack.firstInstalledFamily(defaultFontStack)
  // 2) As a Measurer (recommended for your rendering path)
  val defaultMeasurer: TextWidth.Measurer =
    TextWidth.FontStack.measurerFromStack(defaultFontStack)
  // 3) Or as an AWT Font (if you need metrics like ascent/descent)
  val defaultFont: java.awt.Font =
    TextWidth.FontStack.fontFromStack(defaultFontStack, sizePt = fontSize)
