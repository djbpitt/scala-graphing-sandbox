package net.collatex.reptilian.display

import java.awt.{Font, GraphicsEnvironment}
import java.awt.font.{FontRenderContext, TextAttribute, TextLayout}
import java.awt.geom.AffineTransform
import java.text.{AttributedString, Normalizer}
import java.util
import scala.collection.concurrent.TrieMap
import scala.util.chaining.scalaUtilChainingOps

/** Text measurement utilities grouped under a single module namespace. */
object TextWidth:

  // -----------------------------
  // 1) Font utilities (exact match)
  // -----------------------------
  object FontUtils:
    /** Return a Font if the exact family is installed; else throw with a helpful list. */
    def fontOrError(familyExact: String, sizePt: Float = 16f, style: Int = Font.PLAIN): Font =
      val installed = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toVector.sorted
      if !installed.contains(familyExact) then
        val preview = installed.mkString("\n  - ")
        val msg =
          s"""Font family not installed: "$familyExact"
             |Installed font families include:
             |  - $preview
             |""".stripMargin
        throw new IllegalArgumentException(msg)
      // AWT Font(size) takes points; deriveFont keeps the float size.
      new Font(familyExact, style, sizePt.toInt).deriveFont(sizePt)

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
        missingGlyph: MissingGlyphStrategy = MissingGlyphStrategy.UseFontMissingGlyph
    ): Measurer =
      val frc = new FontRenderContext(new AffineTransform(), antialiased, fractionalMetrics)

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

  // --------------------------------------------
  // 3) Pool: one measurer per exact family (size/style fixed here)
  // --------------------------------------------
  object Pool:
    private val DefaultPt: Float = 16f
    private val DefaultStyle: Int = Font.PLAIN
    private val pool: TrieMap[String, Measurer] = TrieMap.empty

    // Avoid accidental divergence if different given names resolve to the same font
    private def normalizeFamily(f: String): String = f.trim

    def measurerForExactFamily(familyExact: String): Measurer =
      val key = normalizeFamily(familyExact)
      pool.getOrElseUpdate(
        key, {
          val font = FontUtils.fontOrError(key, sizePt = DefaultPt, style = DefaultStyle)
          Measurer.forFont(font)
        }
      )

  // --------------------------------------------
  // 4) Optional syntax: scoped, terse usage inside a render block
  // --------------------------------------------
  object syntax:
    extension (s: String) def width(using m: Measurer): Float = m(s)
