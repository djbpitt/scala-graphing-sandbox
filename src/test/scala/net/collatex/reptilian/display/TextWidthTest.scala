package net.collatex.reptilian.display

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion
import TextWidth.*
import TextWidth.Measurer.MissingGlyphStrategy

class TextWidthTest extends AnyFunSuite {

  // Ensure AWT runs headless in CI
  System.setProperty("java.awt.headless", "true")

  private def firstInstalledFamily(): String = {
    val names = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
    assert(names.nonEmpty, "No installed fonts found on this system")
    names.head
  }

  private def secondInstalledFamilyDifferentFrom(f: String): Option[String] = {
    val names = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toVector
    names.find(_ != f)
  }

  test("FontUtils.fontOrError succeeds for an installed font") {
    val fam = firstInstalledFamily()
    val f = TextWidth.FontUtils.fontOrError(fam)
    assert(f.getFamily == fam)
    assert(f.getSize2D == 16f) // default sizePt is 16f
  }

  test("FontUtils.fontOrError throws with helpful list for missing font") {
    val missing = "Definitely_Not_A_Real_Font_12345"
    val ex = intercept[IllegalArgumentException] {
      TextWidth.FontUtils.fontOrError(missing) // default sizePt is 16f
    }
    assert(ex.getMessage.contains("Font family not installed"))
    assert(ex.getMessage.contains(missing))
    assert(ex.getMessage.contains("Installed font families include"))
  }

  test("Measurer returns positive widths for words") {
    val fam = firstInstalledFamily()
    val m = TextWidth.Measurer.forFont(TextWidth.FontUtils.fontOrError(fam))
    val w1 = m("AV")
    val w2 = m("fi")
    assert(w1 > 0f && w2 > 0f)
  }

  test("Pool keeps caches per font family (different families → different measurers)") {
    val fam1 = firstInstalledFamily()
    val fam2 = secondInstalledFamilyDifferentFrom(fam1).getOrElse(fam1)

    val m1 = TextWidth.Pool.measurerForExactFamily(fam1)
    val m2 = TextWidth.Pool.measurerForExactFamily(fam2)

    if (fam1 == fam2) cancel("Only one font family installed on this system; skipping separation check.")
    else {
      assert(!(m1 eq m2), s"Expected different measurer instances for $fam1 vs $fam2")
    }
  }

  test("Pool reuses existing cache for the same family") {
    val fam = firstInstalledFamily()
    val m1 = TextWidth.Pool.measurerForExactFamily(fam)
    val m2 = TextWidth.Pool.measurerForExactFamily(fam)
    assert(m1 eq m2, "Expected the same measurer instance to be reused for identical family")
  }

  // Shared helper for missing glyph behavior with different options (return average width vs throw)
  private def withMissingGlyphCase[A](font: java.awt.Font)(use: (String /*textWithMissing*/, Int /*cp*/ ) => A): A =
    def firstMissingCodePoint(f: java.awt.Font): Option[Int] = {
      def sampleRange(s: Int, e: Int, step: Int) =
        Iterator.iterate(s)(_ + step).takeWhile(_ <= e)

      val candidates = // ranges that are likely to have gaps
        sampleRange(0xe000, 0xf8ff, 113) ++ // PUA
          sampleRange(0x1f300, 0x1faff, 97) ++ // Misc symbols, pictorgraphs, emoji
          sampleRange(0xf0000, 0xffffd, 257) ++ // Supplemental PUA
          sampleRange(0x100000, 0x10fffd, 257) // Supplemental PUA
      candidates.find(cp => !f.canDisplay(cp))
    }

    firstMissingCodePoint(font) match {
      case None =>
        cancel(s"No missing glyph found for font '${font.getFamily}'; skipping.")
      case Some(cp) =>
        val s = s"hello ${new String(Character.toChars(cp))} world"
        use(s, cp)
    }

  test("AverageAdvance returns a positive width when glyphs are missing") {
    val fam = firstInstalledFamily()
    val font = TextWidth.FontUtils.fontOrError(fam)
    withMissingGlyphCase(font) { (text, _) =>
      val mAvg = TextWidth.Measurer.forFont(
        font,
        missingGlyph = TextWidth.Measurer.MissingGlyphStrategy.AverageAdvance
      )
      assert(mAvg(text) > 0f)
    }
  }

  test("Strict strategy throws when glyphs are missing") {
    val fam = firstInstalledFamily()
    val font = TextWidth.FontUtils.fontOrError(fam)
    withMissingGlyphCase(font) { (text, _) =>
      val mStrict = TextWidth.Measurer.forFont(
        font,
        missingGlyph = TextWidth.Measurer.MissingGlyphStrategy.Strict
      )
      intercept[IllegalStateException] {
        mStrict(text)
      }
    }
  }

  test("NFC normalization: composed and decomposed forms measure the same") {
    val fam = firstInstalledFamily()
    val m = TextWidth.Measurer.forFont(TextWidth.FontUtils.fontOrError(fam))
    val composed = "\u00e9" // é
    val decomposed = "e\u0301" // e + ◌́
    assert(m(composed) == m(decomposed))
  }

  test("Same word measured with different families typically differs") {
    val fam1 = firstInstalledFamily()
    val fam2 = secondInstalledFamilyDifferentFrom(fam1).getOrElse(cancel("Only one family installed; skipping"))
    val m1 = TextWidth.Measurer.forFont(TextWidth.FontUtils.fontOrError(fam1))
    val m2 = TextWidth.Measurer.forFont(TextWidth.FontUtils.fontOrError(fam2))
    // Not a hard guarantee across all fonts, but likely
    assert(m1("reptilian") != m2("reptilian"))
  }

}
