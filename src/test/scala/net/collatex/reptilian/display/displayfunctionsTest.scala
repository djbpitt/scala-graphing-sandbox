package net.collatex.reptilian.display

import DisplayFunctions.*
import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

import scala.xml.Elem
import net.collatex.reptilian.{AlignmentPoint, AlignmentRibbon, Siglum, TokenEnum, TokenRange}

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ListBuffer

/* Alignment fixture for use in tests */
object DisplayTestFixtures {

  val gTa: Vector[TokenEnum] = Vector(
    Token(t = "abc", n = "abc", w = 0, g = 0),
    Token(t = "def", n = "def", w = 0, g = 1),
    TokenSep(t = "sep1", n = "sep1", w = 1, g = 2),
    Token(t = "abc", n = "abc", w = 1, g = 3),
    Token(t = "ghi", n = "ghi", w = 1, g = 4)
  )

  val witness0range0: TokenRange = TokenRange(0, 1, gTa)
  val witness1range0: TokenRange = TokenRange(3, 4, gTa)
  val witness0range1: TokenRange = TokenRange(1, 2, gTa)
  val witness1range1: TokenRange = TokenRange(4, 5, gTa)

  val alignmentPoint0: AlignmentPoint = AlignmentPoint(gTa, 0 -> witness0range0, 1 -> witness1range0)
  val alignmentPoint1: AlignmentPoint = AlignmentPoint(gTa, 0 -> witness0range1, 1 -> witness1range1)
  val alignment: AlignmentRibbon = AlignmentRibbon(ListBuffer(alignmentPoint0, alignmentPoint1))

  val sigla: List[Siglum] = List(Siglum("A"), Siglum("B"))
}

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

  import DisplayTestFixtures.*

  test("emitTableHorizontal writes correct table output to file") {

    val tmpBase = os.temp(prefix = "table-h-", deleteOnExit = true).toString
    emitTableHorizontal(alignment, sigla, gTa, Set(tmpBase))

    val actualPath = os.Path(tmpBase + "-h.txt", os.pwd)
    assert(os.exists(actualPath), s"Output file was not created: $actualPath")

    val actual = os.read.lines(actualPath).mkString("\n").trim
    val expectedPath = os.pwd / "src" / "test" / "resources" / "gold" / "table-h.txt"
    val expected = os.read.lines(expectedPath).mkString("\n").trim

    assert(actual == expected, s"Output mismatch:\nEXPECTED:\n$expected\n\nACTUAL:\n$actual")
  }

  test("emitTableHorizontal writes to stdOut when no filename is provided") {

    // Capture stdout
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(outputStream) {
      emitTableHorizontal(alignment, sigla, gTa, Set.empty)
    }

    val output = outputStream.toString.trim
    val expectedPath = os.pwd / "src" / "test" / "resources" / "gold" / "table-h.txt"
    val expected = os.read.lines(expectedPath).mkString("\n").trim

    assert(output == expected, s"stdOut output mismatch:\nEXPECTED:\n$expected\n\nACTUAL:\n$output")
  }

  test("emitTableVertical writes correct table output to file") {

    val tmpBase = os.temp(prefix = "table-v-", deleteOnExit = true).toString
    emitTableVertical(alignment, sigla, gTa, Set(tmpBase))

    val actualPath = os.Path(tmpBase + "-v.txt", os.pwd)
    assert(os.exists(actualPath), s"Output file was not created: $actualPath")

    val actual = os.read.lines(actualPath).mkString("\n").trim
    val expectedPath = os.pwd / "src" / "test" / "resources" / "gold" / "table-v.txt"
    val expected = os.read.lines(expectedPath).mkString("\n").trim

    assert(actual == expected, s"Output mismatch:\nEXPECTED:\n$expected\n\nACTUAL:\n$actual")
  }

  test("emitTableVertical writes to stdOut when no filename is provided") {

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(outputStream) {
      emitTableVertical(alignment, sigla, gTa, Set.empty)
    }

    val output = outputStream.toString.trim
    val expectedPath = os.pwd / "src" / "test" / "resources" / "gold" / "table-v.txt"
    val expected = os.read.lines(expectedPath).mkString("\n").trim

    assert(output == expected, s"stdOut output mismatch:\nEXPECTED:\n$expected\n\nACTUAL:\n$output")
  }

  test("emitTableHorizontalHTML writes correct table output to file") {
    val tempDir = os.temp.dir(prefix = "table-h-dir-", deleteOnExit = true)
    val outputBase = "test-table"
    val outputBaseFilename = Set((tempDir / outputBase).toString)
    val htmlExtension = Set("xhtml")

    emitTableHorizontalHTML(
      alignment = alignment,
      displaySigla = sigla,
      gTa = gTa,
      outputBaseFilename = outputBaseFilename,
      htmlExtension = htmlExtension
    )

    val outputPath = tempDir / s"$outputBase-h.${htmlExtension.head}"
    assert(os.exists(outputPath), s"Expected output file $outputPath does not exist")

    val actual = os.read(outputPath)
    val expected = os.read(os.pwd / "src" / "test" / "resources" / "gold" / "table-h.xhtml")
    assert(actual.trim == expected.trim)
  }

  test("emitTableHorizontalHTML writes to stdOut when no filename is provided") {
    val output = new ByteArrayOutputStream()
    Console.withOut(output) {
      emitTableHorizontalHTML(
        alignment = alignment,
        displaySigla = sigla,
        gTa = gTa,
        outputBaseFilename = Set.empty,
        htmlExtension = Set("xhtml")
      )
    }

    val actual = output.toString.trim
    val expected = os
      .read(
        os.pwd / "src" / "test" / "resources" / "gold" / "table-h.xhtml"
      )
      .trim

    assert(actual == expected)
  }

  test("emitTableVerticalHTML writes correct table output to file") {
    val tempDir = os.temp.dir(prefix = "table-v-dir-", deleteOnExit = true)
    val outputBase = "test-table"
    val outputBaseFilename = Set((tempDir / outputBase).toString)
    val htmlExtension = Set("xhtml")

    emitTableVerticalHTML(
      alignment = alignment,
      displaySigla = sigla,
      gTa = gTa,
      outputBaseFilename = outputBaseFilename,
      htmlExtension = htmlExtension
    )

    val outputPath = tempDir / s"$outputBase-v.${htmlExtension.head}"
    assert(os.exists(outputPath), s"Expected output file $outputPath does not exist")

    val actual = os.read(outputPath)
    val expected = os.read(os.pwd / "src" / "test" / "resources" / "gold" / "table-v.xhtml")
    assert(actual.trim == expected.trim)
  }

  test("emitTableVerticalHTML writes to stdOut when no filename is provided") {
    val output = new ByteArrayOutputStream()
    Console.withOut(output) {
      emitTableVerticalHTML(
        alignment = alignment,
        displaySigla = sigla,
        gTa = gTa,
        outputBaseFilename = Set.empty,
        htmlExtension = Set("xhtml")
      )
    }

    val actual = output.toString.trim
    val expected = os
      .read(
        os.pwd / "src" / "test" / "resources" / "gold" / "table-v.xhtml"
      )
      .trim
    assert(actual == expected)
  }

  ignore("emitAlignmentRibbon writes correct HTML to file") {
    val tempDir = os.temp.dir(prefix = "alignment-ribbon-dir-", deleteOnExit = true)
    val outputBase = "alignment-ribbon"
    val outputBaseFilename = Set((tempDir / outputBase).toString)
    val htmlExtension = Set("xhtml")

    emitAlignmentRibbon(
      alignment = alignment,
      displaySigla = sigla,
      displayColors = List("pink", "aqua"),
      fonts = List(), // FIXME: Temporary
      outputBaseFilename = outputBaseFilename,
      htmlExtension = htmlExtension
    )

    val outputPath = tempDir / s"$outputBase.${htmlExtension.head}"
    assert(os.exists(outputPath), s"Expected output file $outputPath does not exist")
    val actual = os.read(outputPath)
    val expected = os.read(os.pwd / "src" / "test" / "resources" / "gold" / "alignment-ribbon.xhtml")
    assert(actual.trim == expected.trim)
  }

  ignore("emitAlignmentRibbon writes to stdOut when no filename is provided") {
    val output = new ByteArrayOutputStream()
    Console.withOut(output) {
      emitAlignmentRibbon(
        alignment = alignment,
        displaySigla = sigla,
        displayColors = List("pink", "aqua"),
        fonts = List(), // FIXME: Temporary
        outputBaseFilename = Set.empty,
        htmlExtension = Set("html")
      )
    }

    val actual = output.toString.trim
    val expected = os
      .read(os.pwd / "src" / "test" / "resources" / "gold" / "alignment-ribbon.xhtml")
      .trim
    assert(actual == expected)

  }
