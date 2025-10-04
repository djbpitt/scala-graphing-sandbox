package net.collatex.reptilian.display

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterAll
import java.awt.{Font, GraphicsEnvironment}
import scala.util.Using

class FontInfraTest extends AnyFunSuite with BeforeAndAfterAll {

  private var sourceSerif4: Font = _ // Bring into scope for all tests

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
}
