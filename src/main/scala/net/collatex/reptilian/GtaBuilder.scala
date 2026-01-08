package net.collatex.reptilian

import net.collatex.reptilian.ManifestValidator.{validateJson, validateXml}

import java.net.URI
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.xml.Elem
import upickle.default.*
import ujson.Value

object GtaBuilder:

  // Shared tokenizer for any content string (JSON or XML)
  def tokenizeContent(text: String, cfg: BuildConfig): Iterator[String] =
    cfg.tokenPattern.findAllMatchIn(text).map(_.matched).take(cfg.tokensPerWitnessLimit)

  // Normalize ‘n’ consistently
  def normalizeToken(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFC).normalizeSpace.toLowerCase

  // Emit tokens from raw strings (assign w/g, build Token)
  private def emitFromStrings(raws: Iterator[String], witIndex: Int, startG: Int): Vector[TokenEnum] =
    val result = raws.zipWithIndex.map { (rawT, localTokenOffset) =>
      TokenEnum.Token(
        rawT,
        normalizeToken(rawT),
        witIndex,
        startG + localTokenOffset,
        Map.empty
      )
    }.toVector
    result

  // Emit tokens from provided JSON object (ujson.Value of type JSON array)
  // Create `n` if needed, preserve `other`
  private def emitFromProvided(tokens: Seq[Value], witIndex: Int, startG: Int): Vector[TokenEnum] =
    val result = tokens.arr.zipWithIndex.map { (jsonToken, localTokenOffset) =>
      val t: String = jsonToken.obj("t").str
      val n: String =
        jsonToken.obj.get("n").strOpt.getOrElse(normalizeToken(jsonToken.obj("t").str)) // Short for flatMap(_.strOpt)
      val w: Int = witIndex
      val g = startG + localTokenOffset
      val other = jsonToken.obj.view.filter { case (k, _) => !Set("t", "n", "w", "g")(k) }.toMap
      TokenEnum.Token(t, n, w, g, other)
    }.toVector
    result

  /** Tunable knobs for building GTa. */
  final case class BuildConfig(tokensPerWitnessLimit: Int, tokenPattern: Regex)

  object BuildConfig:
    val Default: BuildConfig =
      BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)

  /** Main entrypoint with defaults (keeps existing call sites working). */
//  def build(md: ManifestData): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
//    build(md, BuildConfig.Default)

  /** Entrypoint that allows callers/tests to supply config. */
  def build(
      md: ManifestData,
      cfg: BuildConfig,
      defaultColors: List[String]
  ): Either[String, (Vector[TokenEnum], List[Siglum], /* colors */ List[String], /* fonts*/ List[Option[String]])] =
    md match
      case ManifestData(source, ManifestFormat.Json) =>
        for
          _ <- validateJson(source)
          json <- retrieveManifestJson(source)
          witnesses <- jsonToWitnessData(json, cfg)
          out <- buildFromWitnessData(witnesses, defaultColors)
        yield out

      case ManifestData(source, ManifestFormat.Xml) =>
        for
          _ <- validateXml(source)
          xml <- retrieveManifestXml(source)
          witnesses <- xmlToWitnessData(xml, md, cfg)
          out <- buildFromWitnessData(witnesses, defaultColors)
        yield out

  private[reptilian] def buildFromWitnessData(
      wits: Seq[WitnessData],
      defaultColors: List[String]
  ): Either[String, (Vector[TokenEnum], List[Siglum], List[String], List[Option[String]])] =
    final case class Acc(
        gta: Vector[TokenEnum],
        sigla: List[Siglum],
        colors: List[String],
        fonts: List[Option[String]]
    )
    val init = Acc(Vector.empty, Nil, Nil, Nil)
    val out: Acc = wits.zipWithIndex.foldLeft(init) { case (acc0, (witData, witIndex)) =>
      // 1) Insert TokenSep BETWEEN witnesses (not before first)
      val acc1 =
        if witIndex == 0 then acc0
        else {
          val sepG = acc0.gta.last.g + 1
          val sepId = s"sep$sepG"
          val sep = TokenEnum.TokenSep(
            sepId,
            sepId,
            /* w = */ acc0.gta.last.w,
            /* g = */ sepG
          )
          acc0.copy(gta = acc0.gta :+ sep)
        }
      // 2) Siglum + color + font (color is witness.color or palette default)
      val siglum: Siglum = witData.siglum
      val color: String = witData.color.getOrElse(defaultColors(witIndex % defaultColors.length))
      val font: Option[String] = witData.font
      val acc2 = acc1.copy(sigla = acc1.sigla :+ siglum, colors = acc1.colors :+ color, fonts = acc1.fonts :+ font)
      // 3) Emit tokens with w/g
      acc2.copy(gta = acc2.gta ++ witData.tokens)
    }
    Right(out.gta, out.sigla, out.colors, out.fonts)

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
        .foldLeft(Vector.empty[WitnessData]) { (acc, current) =>
          val (currentBs, currentWitOffset): (BufferedSource, Int) = current // current witness data as buffered string
          val currentFont: Option[String] =
            ((manifest \ "_")(currentWitOffset) \ "@font").headOption.map(_.text).orElse(rootFontOpt)
          val currentSiglum: Siglum = Siglum(allSigla(currentWitOffset).head.text) // current siglum
          val currentColor: Option[String] =
            ((manifest \ "_")(currentWitOffset) \ "@color").headOption.map(_.text)
          val ts: Iterator[String] = // raw token strings (t values)
            tokenizeContent(currentBs.mkString, cfg)
          val currentTokens =
            emitFromStrings(
              ts,
              currentWitOffset,
              gCounter + acc.map(_.tokens.size).sum + currentWitOffset
            ) // currentWitOffset is, coincidentally and helpfully, also a count of TokenSep instances
          val currentWitnessData = WitnessData(
            currentSiglum,
            currentColor,
            currentFont,
            currentTokens.map(_.asInstanceOf[TokenEnum.Token])
          )
          acc :+ currentWitnessData
        }
      out
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
  private case class WitObj(
      id: String,
      font: Option[String] = None,
      color: Option[String] = None,
      content: Option[String] = None,
      tokens: Option[Seq[ujson.Value]] = None // Value, not Obj
  ) derives ReadWriter
  private case class JsonDataForAlignment(font: Option[String] = None, witnesses: Seq[WitObj]) derives ReadWriter

  def jsonToWitnessData(
      manifest: String,
      cfg: BuildConfig
  ): Either[String, Seq[WitnessData]] = {
    val jd: JsonDataForAlignment = read[JsonDataForAlignment](manifest)
    val rootFontOpt: Option[String] = jd.font
    val out: Vector[WitnessData] = jd.witnesses.zipWithIndex
      .foldLeft(Vector.empty[WitnessData]) { (acc, current) =>
        val (currentWitnessObj, currentWitOffset) = current
        val currentSiglum = Siglum(currentWitnessObj.id)
        val currentColor: Option[String] = currentWitnessObj.color
        val currentFont: Option[String] = currentWitnessObj.font.orElse(rootFontOpt)
        val witnessTokens: Seq[TokenEnum.Token] = currentWitnessObj match {
          case WitObj(_, _, _, Some(content), None) =>
            val ts: Iterator[String] = // raw token strings (t values)
              tokenizeContent(content, cfg)
            val currentTokens: Vector[TokenEnum] =
              emitFromStrings(
                ts,
                currentWitOffset,
                acc.map(_.tokens.size).sum + currentWitOffset // startG
              ) // currentWitOffset is, coincidentally and helpfully, also a count of TokenSep instances
            currentTokens.map(_.asInstanceOf[TokenEnum.Token])
          case WitObj(_, _, _, None, Some(tokens)) => // Only t and option n properties are real
            val emittedTokens = emitFromProvided(
              tokens.take(cfg.tokensPerWitnessLimit),
              currentWitOffset,
              acc.map(_.tokens.size).sum + currentWitOffset // startG
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
        acc :+ currentWitnessData
      }
    Right(out)
  }
