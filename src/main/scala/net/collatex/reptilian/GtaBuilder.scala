package net.collatex.reptilian

import net.collatex.reptilian.ManifestValidator.{validateJson, validateXml}

import java.net.URI
import scala.io.{BufferedSource, Source}
import scala.util.{Try, Using}
import scala.util.matching.Regex
import scala.xml.Elem
import upickle.default.*
import ujson.Value

object GtaBuilder:

  // --- helpers used by the JSON builder ---

  /** Default color palette (same order/logic as legacy). */
  val defaultColors: List[String] =
    List("peru", "orange", "yellow", "limegreen", "dodgerblue", "violet")

  // Shared tokenizer for any content string (JSON or XML)
  def tokenizeContent(text: String, cfg: BuildConfig): Iterator[String] =
    cfg.tokenPattern.findAllMatchIn(text).map(_.matched).take(cfg.tokensPerWitnessLimit)

  // Normalize ‘n’ consistently
  def normalizeToken(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFC).normalizeSpace.toLowerCase

  // Emit tokens from raw strings (assign w/g, build Token)
  def emitFromStrings(raws: Iterator[String], witIndex: Int, startG: Int): (Vector[TokenEnum], Int) = {
    var g = startG
    val out = Vector.newBuilder[TokenEnum]
    raws.foreach { t =>
      out += TokenEnum.Token(t = t, n = normalizeToken(t), w = witIndex, g = g, other = Map.empty)
      g += 1
    }
    (out.result(), g)
  }

  // Emit tokens from provided TokenEnum.Token (preserve ‘other’, including `n`, which we create, if needed, before calling)
  // TODO: See https://github.com/djbpitt/scala-graphing-sandbox/issues/83
  def emitFromProvided(tokens: Seq[TokenEnum.Token], witIndex: Int, startG: Int): (Vector[TokenEnum], Int) = {
    var g = startG
    val out = Vector.newBuilder[TokenEnum]
    tokens.foreach { inTok =>
      out += inTok.copy(w = witIndex, g = g)
      g += 1
    }
    (out.result(), g)
  }

  /** Tunable knobs for building GTa. */
  final case class BuildConfig(tokensPerWitnessLimit: Int, tokenPattern: Regex)

  object BuildConfig:
    val Default: BuildConfig =
      BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)

  /** Main entrypoint with defaults (keeps existing call sites working). */
  def build(md: ManifestData): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
    build(md, BuildConfig.Default)

  /** Entrypoint that allows callers/tests to supply config. */
  def build(
      md: ManifestData,
      cfg: BuildConfig
  ): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
    md match
      case ManifestData(source, ManifestFormat.Json) =>
        for
          _ <- validateJson(source)
          json <- retrieveManifestJson(source)
          witnesses <- jsonToWitnessData(json, cfg)
          out <- buildFromWitnessData(witnesses, cfg, defaultColors)
        yield out

      case ManifestData(source, ManifestFormat.Xml) =>
        for
          _ <- validateXml(source)
          xml <- retrieveManifestXml(source)
          witnesses <- retrieveWitnessDataXml(xml, md) // Seq[CollateXWitnessData], change to Seq[WitnessData]
          out <- buildFromXmlWitnesses(
            witnesses,
            cfg
          ) // Creates gTa and other artifacts; change to accept Seq[WitnessData]
        yield out

  // ---------- JSON unified implementation (no legacy delegation) ----------

  private def buildFromJsonWitnesses(
      witData: Seq[WitnessJsonData],
      cfg: BuildConfig
  ): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] = {

    final case class Acc(
        gta: Vector[TokenEnum],
        sigla: List[Siglum],
        colors: List[String],
        g: Int
    )

    val init = Acc(Vector.empty, Nil, Nil, g = 0)

    val out = witData.zipWithIndex.foldLeft(init) { case (acc0, (w, witIndex)) =>
      // 1) Insert TokenSep BETWEEN witnesses (not before first)
      val acc1 =
        if witIndex == 0 then acc0
        else {
          val sepId = s"sep$witIndex"
          val sep = TokenEnum.TokenSep(sepId, sepId, witIndex, acc0.g)
          acc0.copy(gta = acc0.gta :+ sep, g = acc0.g + 1)
        }

      // 2) Siglum + color (mirror legacy rules)
      val (siglum: Siglum, color: String) = w match
        case WitnessJsonData.FromTokens(id, _, _) =>
          val c = defaultColors(witIndex % defaultColors.length)
          (id, c)
        case WitnessJsonData.FromContent(cd) =>
          val c = cd.color.getOrElse(defaultColors(witIndex % defaultColors.length))
          (cd.siglum, c)

      val acc2 = acc1.copy(sigla = acc1.sigla :+ siglum, colors = acc1.colors :+ color)

      // 3) Emit tokens for this witness using shared helpers
      w match
        case WitnessJsonData.FromContent(cd) =>
          val raws = tokenizeContent(cd.content, cfg)
          val (emitted, nextG) = emitFromStrings(raws, witIndex, acc2.g)
          acc2.copy(gta = acc2.gta ++ emitted, g = nextG)

        case WitnessJsonData.FromTokens(_, toks, _) =>
          val (emitted, nextG) = emitFromProvided(toks, witIndex, acc2.g)
          acc2.copy(gta = acc2.gta ++ emitted, g = nextG)
    }

    Right((out.gta, out.sigla, out.colors))
  }

  private def buildFromXmlWitnesses(
      wits: Seq[CollateXWitnessData],
      cfg: BuildConfig
  ): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =

    final case class Acc(
        gta: Vector[TokenEnum],
        sigla: List[Siglum],
        colors: List[String],
        g: Int
    )

    val init = Acc(Vector.empty, Nil, Nil, g = 0)

    val out = wits.zipWithIndex.foldLeft(init) { case (acc0, (w, witIndex)) =>
      // 1) Insert TokenSep BETWEEN witnesses (not before first)
      val acc1 =
        if witIndex == 0 then acc0
        else {
          // Match legacy XML: label uses current global g; w = previous witness index
          val sepId = s"sep${acc0.g}"
          val sep = TokenEnum.TokenSep(sepId, sepId, /* w = */ witIndex - 1, /* g = */ acc0.g)
          acc0.copy(gta = acc0.gta :+ sep, g = acc0.g + 1)
        }

      // 2) Siglum + color (match legacy: witness.color or palette default)
      val siglum: Siglum = w.siglum
      val color: String = w.color.getOrElse(defaultColors(witIndex % defaultColors.length))
      val acc2 = acc1.copy(sigla = acc1.sigla :+ siglum, colors = acc1.colors :+ color)

      // 3) Tokenize XML content and emit tokens with w/g
      val raws = tokenizeContent(w.content, cfg)
      val (emitted, nextG) = emitFromStrings(raws, witIndex, acc2.g)
      acc2.copy(gta = acc2.gta ++ emitted, g = nextG)
    }

    Right((out.gta, out.sigla, out.colors))

  private def buildFromWitnessData(
      wits: Seq[WitnessData],
      cfg: BuildConfig,
      defaultColors: List[String]
  ): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
    final case class Acc(
        gta: Vector[TokenEnum],
        sigla: List[Siglum],
        colors: List[String],
        g: Int
    )
    val init = Acc(Vector.empty, Nil, Nil, g = 0)
    val out: Acc = wits.zipWithIndex.foldLeft(init) { case (acc0, (witData, witIndex)) =>
      // 1) Insert TokenSep BETWEEN witnesses (not before first)
      val acc1 =
        if witIndex == 0 then acc0
        else {
          val sepId = s"sep${acc0.g}"
          val sep = TokenEnum.TokenSep(sepId, sepId, /* w = */ witIndex - 1, /* g = */ acc0.g)
          acc0.copy(gta = acc0.gta :+ sep, g = acc0.g + 1)
        }
      // 2) Siglum + color (match legacy: witness.color or palette default)
      val siglum: Siglum = witData.siglum
      val color: String = witData.color.getOrElse(defaultColors(witIndex % defaultColors.length))
      val acc2 = acc1.copy(sigla = acc1.sigla :+ siglum, colors = acc1.colors :+ color)
      // 3) Emit tokens with w/g
      val (emitted, nextG) = emitFromProvided(witData.tokens, witIndex, acc2.g)
      acc2.copy(gta = acc2.gta ++ emitted, g = nextG)
    }
    Right(out.gta, out.sigla, out.colors)

  /** Convert incoming XML manifest to Seq[WitnessData]
    *
    * Tokenize each witness, creating `n`, `w`, and `g` properties
    *
    * @param manifest
    *   XML manifest as XML Elem
    * @param manifestSource
    *   Contains pointer to manifest location and type (XML or JSON)
    * @param cfg
    *   Configuration case class with tokensPerWitnessLimit and tokenPattern
    * @return
    *   Seq[WitnessData]
    */
  def xmlToWitnessData(
      manifest: Elem,
      manifestSource: ManifestData,
      cfg: BuildConfig
      // TODO: Cannot fail (?), so cannot return Left(); Either to allow use in for-comprehension
  ): Either[String, Seq[WitnessData]] =
    val rootFontOpt: Option[String] = (manifest \ "@font").headOption.map(_.text)
    val results: Seq[WitnessData] =
      val witnessUrlAttr: Seq[String] = (manifest \ "_").map(e => (e \ "@url").head.text)
      val witTokenStrings: Seq[BufferedSource] = witnessUrlAttr map {
        case remote if remote.startsWith("http://") || remote.startsWith("https://") =>
          Source.fromURL(remote)
        case pathLike =>
          manifestSource.source match
            case ManifestSource.Remote(baseUrl) =>
              val resolvedUrl = URI.create(baseUrl.toString).resolve(pathLike).toURL
              Source.fromURL(resolvedUrl)
            case ManifestSource.Local(basePath) =>
              val manifestParent = basePath / os.up
              val resolvedPath = os.Path(pathLike, manifestParent)
              Source.fromFile(resolvedPath.toString)
      }
      val allSigla = (manifest \ "_").map(_ \ "@siglum")
      val gCounter = 0
      val out = witTokenStrings.zipWithIndex
        .foldLeft((Vector.empty[WitnessData], gCounter)) { (acc, current) =>
          val (currentBs, currentWitOffset): (BufferedSource, Int) = current // current witness data as buffered string
          val currentFont: Option[String] =
            ((manifest \ "_")(currentWitOffset) \ "@font").headOption.map(_.text).orElse(rootFontOpt)
          val currentSiglum: Siglum = Siglum(allSigla(currentWitOffset).head.text) // current siglum
          val currentColor: Option[String] =
            ((manifest \ "_")(currentWitOffset) \ "@color").headOption.map(_.text)
          val ts: Iterator[String] = // raw token strings (t values)
            tokenizeContent(currentBs.mkString, cfg)
          val (currentTokens, nextG) =
            emitFromStrings(
              ts,
              currentWitOffset,
              gCounter + acc._1.map(_.tokens.size).sum + currentWitOffset
            ) // currentWitOffset is, coincidentally and helpfully, also a count of TokenSep instances
          val currentWitnessData = WitnessData(
            currentSiglum,
            currentColor,
            currentFont,
            currentTokens.map(_.asInstanceOf[TokenEnum.Token])
          )
          (acc._1 :+ currentWitnessData, gCounter) // gCounter is ignored in final result
        }
      out._1 // we no longer need gCounter
    Right(results)

  /** Intermediary WitObj case class for transforming JSON manifest to Seq[WitnessData]
    *
    * @param id
    *   Siglum as String
    * @param font
    *   Optional String
    * @param color
    *   Optional String
    * @param content
    *   Optional string
    * @param tokens
    *   Optional Seq[ujson.Value]
    *
    * NB: Every witness must have either `content` or `tokens`, but not both; this is validated earlier, but we also
    * throw an error here if this condition is not met
    *
    * Tokens are represented as a sequence of JSON values to accommodate the optional `other` property, the keys for
    * which cannot be predicted, and therefore cannot be represented directly as case class property names
    */
  case class WitObj(
      id: String,
      font: Option[String] = None,
      color: Option[String] = None,
      content: Option[String] = None,
      tokens: Option[Seq[ujson.Value]] = None // ← Value, not Obj
  ) derives ReadWriter
  case class JsonDataForAlignment(font: Option[String] = None, witnesses: Seq[WitObj]) derives ReadWriter

  def jsonToWitnessData(
      manifest: String,
      cfg: BuildConfig
  ): Either[String, Seq[WitnessData]] = {
    val jd: JsonDataForAlignment = read[JsonDataForAlignment](manifest)
    val rootFontOpt: Option[String] = jd.font
    val gCounter = 0
    val out: (Vector[WitnessData], Int) = jd.witnesses.zipWithIndex
      .foldLeft(Vector.empty[WitnessData], gCounter) { (acc, current) =>
        val (currentWitnessObj, currentWitOffset) = current
        val currentSiglum = Siglum(currentWitnessObj.id)
        val currentColor: Option[String] = currentWitnessObj.color
        val currentFont: Option[String] = currentWitnessObj.font.orElse(rootFontOpt)
        val witnessTokens: Seq[TokenEnum.Token] = currentWitnessObj match {
          case WitObj(_, _, _, Some(content), None) =>
            val ts: Iterator[String] = // raw token strings (t values)
              tokenizeContent(content, cfg)
            val (currentTokens: Vector[TokenEnum], nextG: Int) = {
              emitFromStrings(
                ts,
                currentWitOffset,
                gCounter + acc._1.map(_.tokens.size).sum + currentWitOffset
              ) // currentWitOffset is, coincidentally and helpfully, also a count of TokenSep instances
            }
            currentTokens.map(_.asInstanceOf[TokenEnum.Token])
          case WitObj(_, _, _, None, Some(tokens)) => // Only t and option n properties are real
            // TODO: See https://github.com/djbpitt/scala-graphing-sandbox/issues/83
            val suppliedTokens: Seq[TokenEnum.Token] = tokens.arr
              .map(t =>
                val otherFields: Map[String, ujson.Value] =
                  t.obj.view
                    // "Other" properties; filters out t, n, w, and g, which we handle explicitly
                    .filter { case (k, _) => !Set("t", "n", "w", "g")(k) }.toMap
                TokenEnum
                  .Token( // Create `n` if missing; `w` and `g` are fakes because Token case class requires them
                    t.obj("t").str,
                    t.obj.get("n").strOpt.getOrElse(normalizeToken(t.obj("t").str)), // Short for flatMap(_.strOpt)
                    0,
                    0,
                    otherFields
                  )
                  .asInstanceOf[TokenEnum.Token]
              )
              .toSeq
            val (emittedTokens, nextG: Int) = emitFromProvided(
              suppliedTokens,
              currentWitOffset,
              gCounter + acc._1.map(_.tokens.size).sum + currentWitOffset
            )
            emittedTokens.map(_.asInstanceOf[TokenEnum.Token])
          case _ => throw new RuntimeException(s"WitObj should have either content or tokens: $currentWitnessObj")
        }
        val currentWitnessData = WitnessData(
          currentSiglum,
          currentColor,
          currentFont,
          witnessTokens
        )
        (acc._1 :+ currentWitnessData, gCounter) // gCounter is ignored in final result
      }
    Right(out._1) // we no longer need gCounter
  }
