package net.collatex.reptilian.display

import org.scalatest.Assertions
import java.awt.GraphicsEnvironment
import java.util.Locale

trait TestFontHelpers extends Assertions {

  /** All installed family names (non-localized, trimmed). */
  def installedFamilies: Vector[String] =
    GraphicsEnvironment.getLocalGraphicsEnvironment
      .getAvailableFontFamilyNames(Locale.ROOT)
      .iterator
      .map(_.trim)
      .toVector

  /** First installed family, with assertions baked in. By default skips dot-prefixed system families (e.g.
    * ".AppleSystemUIFont").
    */
  def firstInstalledFamily(skipDotPrefixed: Boolean = true): String = {
    val all = installedFamilies
    assert(all.nonEmpty, "No installed fonts found on this system")
    val fam = all.find(n => !skipDotPrefixed || !n.startsWith(".")).getOrElse(all.head)
    // Guard against accidental padding (and keep behavior consistent):
    assert(fam == fam.trim, s"Installed family appears padded: '$fam'")
    fam // already trimmed
  }

  /** A different installed family than `avoid`, respecting `skipDotPrefixed`. */
  def secondInstalledFamilyDifferentFrom(avoid: String, skipDotPrefixed: Boolean = true): Option[String] =
    installedFamilies.find(n => n != avoid && (!skipDotPrefixed || !n.startsWith(".")))
}
