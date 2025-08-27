package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}

class parseArgsTest extends AnyFunSuite:

  test("Valid args with all switches present") {
    val tmpDir = Files.createTempDirectory("reptilian-test")
    val outPath = tmpDir.resolve("result_file").toString

    val args = Seq(
      "manifest.xml",
      "--format", "table", "svg",
      "--html", "html",
      "--output", outPath
    )
    val res = parseArgs(args)
    assert(res.isRight)

    tmpDir.toFile.deleteOnExit()
  }

  test("Omitting optional switches, only manifest provided") {
    val args = Seq("manifest.json")
    val res = parseArgs(args)
    assert(res.isRight)
  }

  test("Error on missing manifest filename") {
    val args = Seq()
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("Missing required filename"))
  }

  test("Error on manifest with invalid extension") {
    val args = Seq("manifest.txt")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("must end with '.xml' or '.json'"))
  }

  test("Unknown switch triggers error") {
    val args = Seq("manifest.xml", "--unknown", "value")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("Unknown switch"))
  }

  test("Duplicate switches (different spellings) trigger error") {
    val args = Seq("manifest.xml", "--format", "svg", "-f", "table")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("Duplicate switch"))
  }

  test("Switch present without required value triggers error") {
    val args = Seq("manifest.xml", "--html")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("requires exactly one value"))
  }

  test("Invalid --html value triggers error") {
    val args = Seq("manifest.xml", "--html", "invalid")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("must be one of"))
  }

  test("Invalid --format value triggers error") {
    val tmpDir = Files.createTempDirectory("reptilian-test")
    val outPath = tmpDir.resolve("base").toString

    val args = Seq("manifest.xml", "--format", "svg", "badformat", "--output", outPath)
    val res = parseArgs(args)

    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("Invalid '--format' values"))

    tmpDir.toFile.deleteOnExit()
  }

  test("Valid --output path with existing directory is accepted") {
    val tmpDir = Files.createTempDirectory("reptilian-test")
    val outPath = tmpDir.resolve("testfile").toString

    val args = Seq("manifest.xml", "--output", outPath)
    val res = parseArgs(args)

    assert(res.isRight)

    tmpDir.toFile.deleteOnExit()
  }

  test("Invalid --output when parent directory does not exist triggers error") {
    val tmpDir = Files.createTempDirectory("reptilian-test")
    val nonExistentDir = tmpDir.resolve("missing-subdir")
    val outPath = nonExistentDir.resolve("somefile").toString

    val args = Seq("manifest.xml", "--output", outPath)
    val res = parseArgs(args)

    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("parent directory does not exist"))

    tmpDir.toFile.deleteOnExit()
  }

  test("Invalid --output if only directory provided (e.g., '.') triggers error") {
    val args = Seq("manifest.xml", "--output", ".")
    val res = parseArgs(args)

    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("must specify a valid, non-empty file name component"))
  }

  test("Providing --format with no values triggers error") {
    val args = Seq("manifest.xml", "--format")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("requires at least one value"))
  }

  test("Error if multiple --format values provided without --output") {
    val args = Seq("manifest.xml", "--format", "table", "ribbon")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("must also specify an '--output' value"))
  }

  test("Accept multiple --format values when --output is provided") {
    val tmpDir = Files.createTempDirectory("reptilian-test")
    val outPath = tmpDir.resolve("results").toString

    val args = Seq("manifest.xml", "--format", "table", "ribbon", "--output", outPath)
    val res = parseArgs(args)

    assert(res.isRight)

    tmpDir.toFile.deleteOnExit()
  }
