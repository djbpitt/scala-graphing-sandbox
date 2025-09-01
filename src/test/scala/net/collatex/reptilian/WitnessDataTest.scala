package net.collatex.reptilian

import net.collatex.reptilian.GtaUnifiedBuilder.{normalizeToken, tokenizeContent}
import net.collatex.reptilian.ManifestFormat.Xml
import net.collatex.reptilian.TokenEnum.Token
import org.scalatest.funsuite.AnyFunSuite
import os.Path

import scala.io.Source
import scala.util.Using
import scala.xml.Elem

class WitnessDataTest extends AnyFunSuite:
  test("xmlToWitnessData basics") {
    val manifestPath = Path("src/test/resources/manifests/xmlNoRootFont.xml", os.pwd)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, Xml)
    val cfg = GtaUnifiedBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)
    val manifest = retrieveManifestXml(manifestSource).getOrElse(fail("Oops!"))
    System.err.println(manifest)
    val expected = Right(
      Vector(
        WitnessData(
          Siglum("A"),
          Some("TmpColor"),
          Some("TmpFont"),
          Vector(
            Token("This ", "this", 0, 0),
            Token("is ", "is", 0, 1),
            Token("witness ", "witness", 0, 2),
            Token("A" + "\u000a", "a", 0, 3)
          )
        ),
        WitnessData(
          Siglum("B"),
          Some("TmpColor"),
          Some("TmpFont"),
          Vector(
            Token("This ", "this", 1, 0),
            Token("is ", "is", 1, 1),
            Token("witness ", "witness", 1, 2),
            Token("B" + "\u000a", "b", 1, 3)
          )
        ),
        WitnessData(
          Siglum("C"),
          Some("TmpColor"),
          Some("TmpFont"),
          Vector(
            Token("This ", "this", 2, 0),
            Token("is ", "is", 2, 1),
            Token("witness ", "witness", 2, 2),
            Token("C" + "\u000a", "c", 2, 3)
          )
        )
      )
    )
    val result = xmlToWitnessData(manifest, manifestData, cfg)
    System.err.println(result)
    assert(result == expected)
  }

  /* Verify that normalizeToken() correctly removes trailing newline */
  test("Normalize token test") {
    val cfg = GtaUnifiedBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)
    val ts: Vector[String] =
      Using(Source.fromURL(getClass.getResource("/manifests/localA.txt"))) { source =>
        tokenizeContent(source.mkString, cfg)
      }.get.toVector
    val ns: Vector[String] = ts.map(e => normalizeToken(e))
    val expected = Vector("This , this", "is , is", "witness , witness", "A" + "\u000a, a")
    val result = ts.zip(ns).map((t, n) => s"$t, $n")
    assert(result == expected)
  }
