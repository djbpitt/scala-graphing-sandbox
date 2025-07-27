package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

import scala.xml.Elem
import net.collatex.reptilian.{AlignmentPoint, AlignmentRibbon}
import os.{rel, temp}

import scala.collection.mutable.ListBuffer

/* Helper functions */
def cleanTempFile(path: os.Path): Unit =
  if os.exists(path) then os.remove(path)

def readXmlFile(path: os.Path): Elem =
  scala.xml.XML.loadFile(path.toIO)

def readString(path: os.Path): String =
  os.read(path)

def assertMatchesGolden(output: String, goldenFile: os.Path): Unit =
  val expected = os.read(goldenFile).trim
  assert(output.trim == expected, s"Output did not match golden file: $goldenFile")

class displayfunctionsTest extends AnyFunSuite:

  test("emitTableHorizontal writes correct table output to file") {
    val gTa = Vector(
      Token(t = "abc", n = "abc", w = 0, g = 0),
      Token(t = "def", n = "def", w = 0, g = 1),
      TokenSep(t = "sep1", n = "sep1", w = 1, g = 2),
      Token(t = "abc", n = "abc", w = 1, g = 3),
      Token(t = "ghi", n = "ghi", w = 1, g = 4)
    )

    val witness0range0 = TokenRange(0, 1, gTa) // "a"
    val witness1range0 = TokenRange(3, 4, gTa) // "a"

    val witness0range1 = TokenRange(1, 2, gTa) // "b"
    val witness1range1 = TokenRange(4, 5, gTa) // "c"

    val alignmentPoint0 = AlignmentPoint(
      gTa,
      0 -> witness0range0,
      1 -> witness1range0
    )
    val alignmentPoint1 = AlignmentPoint(
      gTa,
      0 -> witness0range1,
      1 -> witness1range1
    )

    val ribbon = AlignmentRibbon(ListBuffer(alignmentPoint0, alignmentPoint1))

    val sigla = List(Siglum("A"), Siglum("B"))

    val tmpBase = os.temp(prefix = "table-h-", deleteOnExit = false).toString
    emitTableHorizontal(ribbon, sigla, gTa, Set(tmpBase))

    val actualPath = os.Path(tmpBase + "-h.txt", os.pwd)
    assert(os.exists(actualPath), s"Output file was not created: $actualPath")

    val actual = os.read.lines(actualPath).mkString("\n").trim
    val expectedPath = os.pwd / "src" / "test" / "resources" / "gold" / "table-h.txt"
    val expected = os.read.lines(expectedPath).mkString("\n").trim

    assert(actual == expected, s"Output mismatch:\nEXPECTED:\n$expected\n\nACTUAL:\n$actual")
  }
