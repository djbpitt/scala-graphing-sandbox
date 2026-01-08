package net.collatex.reptilian.display

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterAll

import scala.util.Using
import java.awt.{Font, GraphicsEnvironment}
import java.awt.font.{FontRenderContext, TextAttribute, TextLayout}
import java.awt.geom.AffineTransform
import java.text.AttributedString
import scala.compiletime.uninitialized

class FontInfraTest extends AnyFunSuite with BeforeAndAfterAll {

  private var sourceSerif4: Font = uninitialized // Bring into scope for all tests

  override def beforeAll(): Unit = {
    System.setProperty("java.awt.headless", "true")
    sourceSerif4 = {
      val url = Option(getClass.getResource("/SourceSerif4-Regular.ttf"))
        .getOrElse(fail("Missing /SourceSerif4-Regular.ttf in test resources"))
      Using.resource(url.openStream()) { is =>
        Font.createFont(Font.TRUETYPE_FONT, is).deriveFont(16f)
      }
    }
    GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(sourceSerif4)
    super.beforeAll()
  }

  test("FontStack picks the first installed from a CSS-style stack (trim + quotes)") {
    val fam = sourceSerif4.getFamily(java.util.Locale.ROOT) // e.g., "Source Serif 4"
    val stack = s"  'No Such Font',  '$fam' , serif  "
    val resolved = TextWidth.FontStack.firstInstalledFamily(stack)
    assert(resolved == fam)
  }

  test("FontStack falls back to logical family when none in stack are installed") {
    val resolved = TextWidth.FontStack.firstInstalledFamily("NopeOne, NopeTwo, serif")
    // AWT logical name should be selected
    assert(Set("Serif", "SansSerif", "Monospaced").contains(resolved))
  }

  test("buildWitnessMeasurers: default + specified families map to pool measurers in order") {
    // Suppose your createHorizontalRibbons() calls this helper:
    def buildWitnessMeasurers(fonts: List[Option[String]]): Vector[TextWidth.Measurer] = {
      val families =
        fonts.iterator
          .map(_.fold(TextWidth.defaultFamily)(TextWidth.FontStack.firstInstalledFamily))
          .toVector
      families.distinct.foreach(TextWidth.Pool.measurerForExactFamily)
      families.map(TextWidth.Pool.measurerForExactFamily)
    }

    val famSS4 = sourceSerif4.getFamily(java.util.Locale.ROOT)
    val measurers = buildWitnessMeasurers(
      List(
        None, // should use default
        Some(s"$famSS4, serif"), // should resolve to Source Serif 4
        Some("Serif") // logical family, always present
      )
    )

    // Same measurer reused if default family == resolved family at position 0 (pool reuse check, not identity here)
    assert(measurers.length == 3)

    // Sanity: each measurer produces a positive width
    val sample = "reptilian"
    assert(measurers.forall(m => m(sample) > 0f))
  }

  test("Pool returns the same measurer instance for the same family (reuse)") {
    val fam = sourceSerif4.getFamily(java.util.Locale.ROOT)
    val a = TextWidth.Pool.measurerForExactFamily(fam)
    val b = TextWidth.Pool.measurerForExactFamily(fam)
    assert(a eq b)
  }

  // If it passes we're working with px, which is what we want
  // If it fails, we're working with pts, which is wrong
  test("Pool measurer matches kerning+ligatures ON with CSS-px-scaled FRC") {
    System.setProperty("java.awt.headless", "true")
    TextWidth.Pool.clear() // Start from a clean reset

    val fam = TextWidth.defaultFamily
    val font = TextWidth.FontUtils.fontOrError(fam, sizePt = 16f)
    val s = "HamburgefontsIV 123 fi AV" // Includes kerning and ligation

    // Build a CSS-px FRC (pt → px via 96/72 scale)
    val tx = new AffineTransform()
    tx.scale(96.0 / 72.0, 96.0 / 72.0)
    val frcPx = new FontRenderContext(tx, /*AA*/ true, /*fractional*/ true)

    // Ground truth: kerning + ligatures ON
    val as = new AttributedString(s)
    as.addAttribute(TextAttribute.FONT, font)
    as.addAttribute(TextAttribute.KERNING, TextAttribute.KERNING_ON)
    as.addAttribute(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    val truthPx = new TextLayout(as.getIterator, frcPx).getAdvance

    val mPool = TextWidth.Pool.measurerForExactFamily(fam) // Pool calls forFont(..., units = CssPx)
    val wPool = mPool(s)

    val eps = 1e-4f
    val diff = math.abs(wPool - truthPx)
    // Print info message only on failure
    withClue(f"pool vs truth(px): wPool=$wPool%.4f  truthPx=$truthPx%.4f  diff=$diff%.5f  ") {
      assert(diff <= eps, "Pool should match kerning+ligatures ON at CSS px scale")
    }
  }

}
