package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite

import java.awt.{Font, Graphics2D}
import java.awt.image.BufferedImage
import java.awt.font.TextLayout
import java.awt.geom.Rectangle2D
import java.awt.GraphicsEnvironment
import java.util
import scala.collection.mutable.ArrayBuffer
import scala.runtime.stdLibPatches.Predef.assert

class fontMetricsTest extends AnyFunSuite:
  test("tmp") {
    val BukyVede12Widths = getCharacterSizes("BukyVede", 12)
    ('\u0000' to '\uFFFF').foreach(e =>
      System.err.println(s"${e.toInt.toHexString}, ${BukyVede12Widths(e)}")
    )
    assert(1 == 1)
  }

object HelperFunctions {
  /** Map from char to (advanceWidth: Double, height: Double)
    *
    * NB: Height is constant across the font
    *
    * @param fontName
    *   String
    * @param fontSize
    *   Int
    * @return
    *   Map from char to (advanceWidth: Double, height: Double)
    */
  def getCharacterSizes(fontName: String, fontSize: Int): Map[Char, (Int, Double, Double)] =
    // Create a dummy BufferedImage to get a Graphics2D context
    val image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val g2d = image.createGraphics()

    // Create the font
    val font = new Font(fontName, Font.PLAIN, fontSize)
    g2d.setFont(font)

    // Get FontMetrics for general height
    val fontMetrics = g2d.getFontMetrics(font)
    val fontHeight = fontMetrics.getHeight.toDouble // Height is uniform for most fonts

    // Map to store character sizes (width, height)
    val bmpHex = 0x0000 to 0xFFFF
    val sizes = bmpHex
      .filter(e => font.canDisplay(e))
      .map { hex =>
        // Use TextLayout for precise character bounds
        val str = hex.toString
        val textLayout = new TextLayout(str, font, g2d.getFontRenderContext)
        val bounds = textLayout.getBounds
        val width = bounds.getWidth
        (Character.toChars(hex).head, (hex, width, fontHeight))
      }
      .toMap

    // Clean up
    g2d.dispose()

    sizes

}

export HelperFunctions._
