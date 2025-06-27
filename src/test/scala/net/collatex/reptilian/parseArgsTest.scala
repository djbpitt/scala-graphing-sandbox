package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite

class parseArgsTest extends AnyFunSuite:

  test("Valid args with all switches present") {
    val args = Seq(
      "manifest.xml",
      "--format", "table", "svg",
      "--html", "html",
      "--output", "result_file"
    )
    val res = parseArgs(args)
    assert(res.isRight)
    val (manifest, parsedMap) = res.toOption.get
    assert(manifest == "manifest.xml")
    assert(parsedMap("--format") == Set("table", "svg"))
    assert(parsedMap("--html") == Set("html"))
    assert(parsedMap("--output") == Set("result_file"))
  }

  test("Omitting optional switches, only manifest provided") {
    val args = Seq("manifest.json")
    val res = parseArgs(args)
    assert(res.isRight)
    val (manifest, parsedMap) = res.toOption.get
    assert(manifest == "manifest.json")
    assert(parsedMap.isEmpty)
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
    assert(res.swap.toOption.get.contains("'--html' requires exactly one value"))
  }

  test("Invalid --html value triggers error") {
    val args = Seq("manifest.xml", "--html", "invalid")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("'--html' value must be one of"))
  }

  test("Invalid --format value triggers error") {
    val args = Seq("manifest.xml", "--format", "svg", "badformat")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("Invalid '--format' values"))
  }

  test("Invalid --output characters trigger error") {
    val args = Seq("manifest.xml", "--output", "bad name!")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("contains invalid characters"))
  }

  test("Providing --format with no values triggers error") {
    val args = Seq("manifest.xml", "--format")
    val res = parseArgs(args)
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("'--format' requires at least one value"))
  }
