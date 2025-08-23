package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite

/** Test suite for FontMetricsCalculator.
  *
  * Behaviors covered:
  *   - Existence of requested font
  *   - Construction of font metrics tables
  *   - Computation of character and string widths
  *   - Handling of missing glyphs and informative exception messages
  *   - NFC normalization of input strings
  *   - Memoization of repeated string width lookups
  */
class FontMetricsCalculatorTest extends AnyFunSuite {

  test("fontOrError returns a Font instance for an installed font") {
    val fontName = "Monospaced" // commonly installed font
    val size = 12
    val font = FontUtils.fontOrError(fontName, size)
    assert(font.getFamily == fontName || font.getFontName.contains(fontName))
  }

  test("fontOrError throws IllegalArgumentException for a non-existent font") {
    val missingFontName = "ThisFontDoesNotExist123"
    val size = 12

    val thrown = intercept[IllegalArgumentException] {
      FontUtils.fontOrError(missingFontName, size)
    }

    assert(
      thrown.getMessage.contains(missingFontName),
      s"Expected exception message to include font name '$missingFontName'"
    )
  }

  // Use a monospace font to make width predictions simple
  private val metrics = FontMetricsCalculator.buildMetrics("Monospaced", 12)

  test("single character width exists for valid character") {
    val width = metrics.stringWidth("A")
    assert(width > 0, "Width should be positive for existing glyph")
  }

  test("string width sums correctly for multiple characters") {
    val s = "Hello"
    val expected = s.toCharArray.map(ch => metrics.stringWidth(ch.toString)).sum
    val computed = metrics.stringWidth(s)
    assert(computed == expected, "String width should sum individual character widths")
  }

  test("string width throws for missing glyph") {
    val missingChar = "\uFFFF" // likely missing in most fonts
    val thrown = intercept[IllegalStateException] {
      metrics.stringWidth(missingChar)
    }
    assert(thrown.getMessage.contains("U+FFFF"))
  }

  test("exception message lists all missing glyphs with counts") {
    val s = "\uFFFF\uFFFF\u0000\uFFFF" // repeated missing glyphs and a valid one
    val thrown = intercept[IllegalStateException] {
      metrics.stringWidth(s)
    }
    val msg = thrown.getMessage
    assert(msg.contains("U+FFFF"))
    assert(msg.contains("x3")) // count for repeated missing glyph
  }

  test("NFC normalization yields same width for composed/decomposed forms") {
    val composed = "\u00E9" // é
    val decomposed = "e\u0301" // e + combining accent
    val w1 = metrics.stringWidth(composed)
    val w2 = metrics.stringWidth(decomposed)
    assert(w1 == w2, "Widths should be equal after NFC normalization")
  }

  test("empty string has zero width") {
    val width = metrics.stringWidth("")
    assert(width == 0f, "Empty string should have width 0")
  }

  test("memoization prevents repeated computation") {
    // Test-only subclass to count actual width computations.
    // It detects a cache miss by checking the cache before delegating.
    class TestFontMetrics(widths: Map[Int, Float]) extends FontMetricsCalculator.FontMetrics(widths) {
      var computeCount: Int = 0

      override def stringWidth(s: String): Float = {
        val normalized = java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFC)
        val wasCached = cache.contains(normalized)
        val result = super.stringWidth(s)
        if (!wasCached) computeCount += 1
        result
      }
    }

    val testMetrics = new TestFontMetrics(metrics.widths)
    val s = "MemoTest"

    // First call: miss → compute and cache
    val w1 = testMetrics.stringWidth(s)
    assert(testMetrics.computeCount == 1, "First call should increment computeCount on cache miss")

    // Second call: hit → should not compute again
    val w2 = testMetrics.stringWidth(s)
    assert(w2 == w1, "Widths should match for repeated calls")
    assert(testMetrics.computeCount == 1, "Second call should not increment computeCount (cache hit)")
  }
}
