package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import org.virtuslab.yaml.*

class ConfigTest extends AnyFunSuite {

  private def readResource(name: String): String =
    io.Source.fromResource(name).mkString

  private def decode(yaml: String): Config =
    yaml.as[Config].getOrElse(throw new RuntimeException("Missing or invalid config.yaml"))

  // ---- 1) Integration: load + decode ----
  test("config.yaml loads from classpath and decodes to Config") {
    val raw = readResource("config.yaml")
    assert(raw.nonEmpty)

    val cfg = decode(raw)
    assert(cfg.tokenPattern.nonEmpty)
    assert(cfg.defaultColors.nonEmpty) // non-empty by contract
  }

  // ---- 2) Defaulting rule via ResolvedConfig.from (using real file) ----
  test("ResolvedConfig.from defaults tokensPerWitnessLimit to Int.MaxValue when missing") {
    val cfg = decode(readResource("config.yaml"))
    assert(cfg.tokensPerWitnessLimit.isEmpty) // omitted by design

    val rc = ResolvedConfig.from(cfg)
    assert(rc.tokensPerWitnessLimit == Int.MaxValue)
  }

  // ---- 3) Explicit tokensPerWitnessLimit respected (inline YAML) ----
  test("ResolvedConfig.from honors explicit integer tokensPerWitnessLimit") {
    val yaml =
      """tokenPattern: '(\w+|[^\w\s])\s*'
        |tokensPerWitnessLimit: 100
        |defaultColors: ['#ff0000', '#00ff00']
        |""".stripMargin

    val cfg = decode(yaml)
    val rc = ResolvedConfig.from(cfg)
    assert(cfg.tokensPerWitnessLimit.contains(100)) // Test parse into Some(100)
    assert(rc.tokensPerWitnessLimit == 100) // Test resolver
  }

  // ---- 4) Regex compiles and behaves as intended (single-quoted YAML) ----
  test("tokenPattern compiles and tokenizes a small sample") {
    val cfg = decode(readResource("config.yaml"))
    val re = ResolvedConfig.from(cfg).tokenPattern

    val input = "Hello, world!"
    val tokens = re.findAllMatchIn(input).map(_.matched).toList
    // Expect: word, comma + space, word, exclamation
    assert(tokens == List("Hello", ", ", "world", "!"))
  }

  // ---- 5) defaultColors: non-empty, order preserved, duplicates allowed ----
  test("defaultColors is non-empty") {
    val cfg = decode(readResource("config.yaml"))
    assert(cfg.defaultColors.nonEmpty)
  }

  test("ResolvedConfig preserves sequence order and duplicates for defaultColors") {
    val yaml =
      """tokenPattern: '(?:\w+|[^\w\s])\s*'
        |defaultColors: ['A','B','B','C']
        |""".stripMargin

    val cfg = decode(yaml)
    val rc = ResolvedConfig.from(cfg)
    assert(rc.defaultColors == List("A", "B", "B", "C")) // exact order + dup preserved
  }

  // Unordered YAML sets shouldn’t decode into List (prevents accidental order matching)
  test("decoding !!set into defaultColors fails (avoids unordered semantics)") {
    val yaml =
      """tokenPattern: 'x'
        |defaultColors: !!set { A: null, B: null }
        |""".stripMargin

    val err = yaml.as[Config]
    assert(err.isLeft)
  }
}
