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
    // val BukyVedeWidths = getCharacterSizes("BukyVede", 12)
    assert(1 == 1)
  }

object HelperFunctions {
  def x(fontName: String, fontSize: Int): Map[Int, Double] = 
    // Load a font (system font example)
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val font = new Font(fontName, Font.PLAIN, fontSize)
    val supportedCodePoints: ArrayBuffer[Int] = ArrayBuffer()
    // Check BMP range (U+0000 to U+FFFF)
    for (cp <- Character.MIN_CODE_POINT to Character.MAX_CODE_POINT) {
      if (font.canDisplay(cp)) supportedCodePoints.append(cp)
    }
    Map()
  

  def getCharacterSizes(fontName: String, fontSize: Int, characters: String): Map[Char, (Double, Double)] = 
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
    val sizes = characters.map { char =>
      // Use TextLayout for precise character bounds
      val textLayout = new TextLayout(char.toString, font, g2d.getFontRenderContext)
      val bounds = textLayout.getBounds
      val width = bounds.getWidth
      (char, (width, fontHeight))
    }.toMap

    // Clean up
    g2d.dispose()

    sizes
  
}

