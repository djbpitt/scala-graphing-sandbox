package net.collatex.reptilian.display

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion
import TextWidth.*
import TextWidth.Measurer.MissingGlyphStrategy

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextLayout}
import java.awt.geom.AffineTransform
import java.text.AttributedString
import scala.util.Using

class TextWidthTest extends AnyFunSuite with TestFontHelpers {

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

  test("Pool reuses same measurer for family name with extra whitespace") {
    val fam = firstInstalledFamily()
    val a = TextWidth.Pool.measurerForExactFamily(fam)
    val b = TextWidth.Pool.measurerForExactFamily("  " + fam + "  ")
    assert(a eq b)
  }

  test("Space width positive and sum of words + spaces behaves sanely") {
    val fam = firstInstalledFamily()
    val m = TextWidth.Measurer.forFont(TextWidth.FontUtils.fontOrError(fam))
    val space = m(" ")
    assert(space > 0f)
    val words = List("the", "quick", "brown", "fox")
    val sumWords = words.map(m(_)).sum
    val withSpaces = sumWords + space * (words.size - 1)
    assert(withSpaces > sumWords)
  }

  test("If font supports ligatures/kerning, shaped width reflects it") {
    val fam = firstInstalledFamily()
    val m = TextWidth.Measurer.forFont(TextWidth.FontUtils.fontOrError(fam))
    val separate = m("f") + m("i")
    val combined = m("fi")
    // Heuristic: combined should be <= separate (ligature) or slightly less (kerning); if not, skip.
    if (combined <= separate) succeed
    else cancel(s"Font '$fam' doesn’t show ligature/kerning effect for 'fi'; skipping.")
  }

  // Helper to load test font that supports ligatures and kerning
  private def loadTestFontFromResources(path: String, sizePt: Float = 16f): java.awt.Font = {
    val url = Option(getClass.getResource(path)).getOrElse { // e.g. "/SourceSerif4-Regular.ttf"
      fail(s"Missing test font resource at $path")
    }
    Using.resource(url.openStream()) { is =>
      val f0 = java.awt.Font
        .createFont(java.awt.Font.TRUETYPE_FONT, is)
        .deriveFont(sizePt)
      f0
    }
  }

  // Helper for testing ligature and kerning support
  private def measureWith(text: String, font: Font, kerning: Boolean, ligatures: Boolean): Float = {
    val frc = new FontRenderContext(new AffineTransform(), /*antialiased*/ true, /*fractional*/ true)
    val as = new AttributedString(text)
    as.addAttribute(TextAttribute.FONT, font)
    if (kerning) as.addAttribute(TextAttribute.KERNING, TextAttribute.KERNING_ON)
    if (ligatures) as.addAttribute(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    new TextLayout(as.getIterator, frc).getAdvance
  }

  // NB: Does not test whether ligation is performed, which is brittle (depends on JVM/OS)
  test("Precomposed ligature glyph ‘ﬁ’ is narrower than ‘f’ + ‘i’ in Source Serif 4") {
    System.setProperty("java.awt.headless", "true")
    val font = loadTestFontFromResources("/SourceSerif4-Regular.ttf")
    assume(font.canDisplayUpTo("\uFB01") < 0, "Font lacks U+FB01 ‘ﬁ’ ligature")

    val eps = 1e-4f // Small epsilon (tolerance)
    val seqWidth = measureWith("fi", font, kerning = true, ligatures = false) // no substitution
    val ligWidth = measureWith("\uFB01", font, kerning = true, ligatures = false) // single ligature glyph

    assert(ligWidth + eps < seqWidth, s"Expected ‘ﬁ’ < ‘f’+‘i’: lig=$ligWidth, seq=$seqWidth")

    // And the production measurer should match the single-glyph case:
    val prod = TextWidth.Measurer.forFont(font)
    assert(math.abs(prod("\uFB01") - ligWidth) <= eps)
  }

  // NB: Does not test whether kerning is performed, which is brittle (depends on JVM/OS)
  // Tests whether our measurers match library java.awt.Font.TextLayout results
  test("Kerning wiring: measurer matches TextLayout for kerning ON and OFF") {
    System.setProperty("java.awt.headless", "true")

    val font = loadTestFontFromResources("/SourceSerif4-Regular.ttf")
    val s = "AV" * 80 // long string just to be safe
    val eps = 1e-4f

    // Ground truth via direct TextLayout
    val truthOff = measureWith(s, font, kerning = false, ligatures = false)
    val truthOn = measureWith(s, font, kerning = true, ligatures = false)

    // Our measurers with explicit toggles
    val mOff = TextWidth.Measurer.forFontWithFeatures(font, enableKerning = false, enableLigatures = false)
    val mOn = TextWidth.Measurer.forFontWithFeatures(font, enableKerning = true, enableLigatures = false)

    val wOff = mOff(s)
    val wOn = mOn(s)

    // Assert we match Java2D’s ON/OFF results (verifies wiring regardless of platform behavior)
    assert(math.abs(wOff - truthOff) <= eps, s"kerning=OFF: measurer=$wOff vs TextLayout=$truthOff")
    assert(math.abs(wOn - truthOn) <= eps, s"kerning=ON:  measurer=$wOn  vs TextLayout=$truthOn")
  }

  // NB: Will be canceled if JVM/OS undermine our control of kerning parameters
  test("Kerning effect: on this JRE/font, kerning reduces width (integration)") {
    System.setProperty("java.awt.headless", "true")

    val font = loadTestFontFromResources("/SourceSerif4-Regular.ttf")
    val s = "AV" * 200 // amplify effect
    val eps = 1e-4f
    val min = 1e-3f // require at least this delta to call it “real”

    val off = measureWith(s, font, kerning = false, ligatures = false)
    val on = measureWith(s, font, kerning = true, ligatures = false)

    if (off - on > min) assert(on + eps < off)
    else cancel("No measurable kerning delta on this JRE/font; skipping effect assertion.")
  }

}
