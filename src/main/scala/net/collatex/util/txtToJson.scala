package net.collatex.util

/** Creates pretokenized JSON input from Mikulka Slavonic prayer example
  *
  * Used for testing CollateX processing of pretokenized JSON
  */

import os._
import scala.util.matching.Regex
import scala.xml.*
import ujson.*

/* Constants */
val tokenPattern: Regex = raw"(\w+|[^\w\s]+)\s*".r

/* Create soundexifier (Soundex function) */
val superscriptMap: Elem =
  XML.loadString(os.read(os.pwd / "src" / "main" / "data" / "manifest" / "mikulka" / "superscript-map.xml"))
val replacementInputs: String = (superscriptMap \\ "sup").map(_.text).mkString
val replacementOutputs: String = (superscriptMap \\ "base").map(_.text).mkString
val unSuper: String => String = translate(replacementInputs, replacementOutputs)

/** Mimics XPath translate()
  *
  * Partially applied to create unSuper function
  *
  * @param input
  *   String to modify
  * @param from
  *   Sequence of characters to replace, as string
  * @param to
  *   Sequence of one-to-one replacements
  * @return
  *   Modified string
  */
def translate(from: String, to: String)(input: String): String = {
  val translation = from.zip(to).toMap
  input.map(c => translation.getOrElse(c, c))
}

def replaceNotOneToOne(inText: String, mapping: Map[String, String]): String =
  mapping.foldLeft(inText) { case (str, (pattern, replacement)) =>
    str.replace(pattern, replacement)
  }

def replaceManyToOne(inText: String): String = {
  val replacements = Map("оу" -> "у", "шт" -> "щ")
  replaceNotOneToOne(inText, replacements)
}

def replaceOneToMany(inText: String): String = {
  val replacements = Map("ѿ" -> "ѡт", "ѯ" -> "кс", "ѱ" -> "пс")
  replaceNotOneToOne(inText, replacements)
}

// One-to-one replacements, grouped by replacement character
val replacementsByGroup: Map[String, Char] = Map(
  "ѧѩꙙꙝꙗя" -> 'ѧ',
  "еєѥѣꙓ" -> 'е', // jat' merged with e experimentally
  "ыꙑиіїꙇй" -> 'и',
  "оꙩꙫꙭꙮѡꙍѽѻꚙꚛ" -> 'о',
  "уꙋюꙕѵѷӱѹ" -> 'у',
  "ѫѭꙛ" -> 'ѫ',
  // "ѣꙓ" -> 'ѣ',
  "ьъ" -> 'ь',
  "зꙁꙃѕꙅ" -> 'з'
)

// One to one replacements (from replacementsByGroup) as map from input char to output char
val charMap: Map[Char, Char] = replacementsByGroup.flatMap { case (group, replacementChar) =>
  group.map(c => c -> replacementChar)
}

// Partially apply to create function that uses specified mapping
def replaceOneToOne(mapping: Map[Char, Char])(input: String): String =
  input.map(c => mapping.getOrElse(c, c)) // leaves characters unchanged if not in map
val myReplaceOneToOne: String => String = replaceOneToOne(charMap)

def tokenize(inText: String) =
  val result = tokenPattern.findAllIn(inText).toList
  result

// Keep punctuation-only strings, but otherwise remove punctuation
def stripPunct(s: String): String =
  val tmp = s.replaceAll("""\p{Punct}""", "")
  if tmp.isEmpty then s
  else tmp

val vowelRegex = "[аѧеиоуѫь]".r
def stripNoninitialVowels(s: String): String =
  val first = s.head
  val rest = vowelRegex.replaceAllIn(s.tail, "")
  s"$first$rest"

def degeminateConsonants(input: String): String =
  input.replaceAll("(?i)([^аѧеиоуѫь\\W_])\\1+", "$1")

val normalizationPipeline: String => String = {
  Function.chain(
    Seq(
      _.trim,
      _.toLowerCase,
      stripPunct,
      unSuper,
      replaceManyToOne,
      replaceOneToMany,
      myReplaceOneToOne,
      degeminateConsonants, // must precede stripping vowels
      stripNoninitialVowels,
      _.take(4)
    )
  )
}

@main def textToJson(): Unit =
  val pathToData = os.pwd / "src" / "main" / "data" / "manifest" / "mikulka"
  val sigla = os.read.lines(pathToData / "sigla.txt")
  val txt = os.read.lines(pathToData / "readings.txt")
  val data = sigla.zip(txt)
  val jsonItems = data map { (s, t) =>
    val ts = tokenize(t)
    val ns = ts.map(normalizationPipeline)
    val tokens = ts.zip(ns).map((t, n) => Obj("t" -> t, "n" -> n))
    Obj(
      "id" -> s,
      "tokens" -> tokens
    )
  }
  val result = Obj("witnesses" -> jsonItems)
  val jsonString: String = ujson.write(result, indent = 2)
  val outputPath: os.Path = pathToData / "mikulka.json" // Save to current working directory
  os.write.over(outputPath, jsonString)
