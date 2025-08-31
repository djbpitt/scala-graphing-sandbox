package net.collatex.reptilian

import net.collatex.reptilian.GtaUnifiedBuilder.tokenizeContent

import java.net.URI
import scala.io.Source
import scala.util.{Try, Using}
import scala.util.matching.Regex
import scala.xml.Elem

object GtaUnifiedBuilder:

  // --- helpers used by the JSON builder ---

  /** Default color palette (same order/logic as legacy). */
  private val defaultColors: List[String] =
    List("peru", "orange", "yellow", "limegreen", "dodgerblue", "violet")

  // Shared tokenizer for any content string (JSON or XML)
  def tokenizeContent(text: String, cfg: BuildConfig): Iterator[String] =
    cfg.tokenPattern.findAllMatchIn(text).map(_.matched).take(cfg.tokensPerWitnessLimit)

  // Normalize ‘n’ consistently
  def normalizeToken(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFC).trim.toLowerCase

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

  // Emit tokens from provided TokenEnum.Token (preserve ‘other’, backfill/normalize n)
  private def emitFromProvided(tokens: Seq[TokenEnum.Token], witIndex: Int, startG: Int): (Vector[TokenEnum], Int) = {
    var g = startG
    val out = Vector.newBuilder[TokenEnum]
    tokens.foreach { inTok =>
      val nFixed = Option(inTok.n).filter(_.nonEmpty).getOrElse(normalizeToken(inTok.t))
      out += inTok.copy(w = witIndex, g = g, n = nFixed)
      g += 1
    }
    (out.result(), g)
  }

  /** Tunable knobs for building GTa. */
  final case class BuildConfig(tokensPerWitnessLimit: Int, tokenPattern: Regex)

  object BuildConfig:
    // Keep defaults identical to legacy behavior
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
          json <- retrieveManifestJson(source)
          witnesses <- retrieveWitnessDataJson(json, md)
          out <- buildFromJsonWitnesses(witnesses, cfg)
        yield out

      case ManifestData(source, ManifestFormat.Xml) =>
        for
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

// --Tmp --
/** Convert incoming XML manifest to Seq[WitnessData]
  *
  * Tokenize each witness, creating `n`, `w`, and `g` properties
  *
  * @param manifest
  * @param manifestSource
  * @return
  */
def xmlToWitnessData(
    manifest: Elem,
    manifestSource: ManifestData,
    cfg: GtaUnifiedBuilder.BuildConfig
): Either[String, Seq[WitnessData]] =
  val rootFontOpt = (manifest \ "@font").headOption.map(_.text)
  val results: Seq[Either[String, WitnessData]] =

    // RESUME HERE 2025-08-31
    // We create the wrong output types
    // We incorrectly build gTa and lists of sigla and colors instead of sequence of WitnessData instances
    // Instead of Acc, just track witness number and global token number, including skip for TokenSep
    final case class Acc(
      gta: Vector[TokenEnum],
      sigla: List[Siglum],
      colors: List[String],
      g: Int
    )

    val init = Acc(Vector.empty, Nil, Nil, g = 0)
    val witnessUrlAttr = (manifest \ "_").map(e => (e \ "@url").head.text)
    val wits = witnessUrlAttr map {
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
    val out = wits.zipWithIndex.foldLeft(init) { case (currentAcc, (bs, witIndex)) =>
      // 1) Insert TokenSep BETWEEN witnesses (not before first)
      val acc1 =
        if witIndex == 0 then currentAcc
        else {
          // Match legacy XML: label uses current global g; w = previous witness index
          val sepId = s"sep${currentAcc.g}"
          val sep = TokenEnum.TokenSep(sepId, sepId, witIndex - 1, currentAcc.g)
          currentAcc.copy(gta = currentAcc.gta :+ sep, g = currentAcc.g + 1)
        }

      // 2) Siglum + color (match legacy: witness.color or palette default)
      val siglum: Siglum = Siglum((manifest \ "_")(witIndex).text)
      val color: String = "Tmp" // w.color.getOrElse(defaultColors(witIndex % defaultColors.length))
      val acc2 = acc1.copy(sigla = acc1.sigla :+ siglum, colors = acc1.colors :+ color)

      // 3) Tokenize XML content and emit tokens with w/g
      val raws = GtaUnifiedBuilder.tokenizeContent(bs.mkString, cfg)
      val (emitted, nextG) = GtaUnifiedBuilder.emitFromStrings(raws, witIndex, acc2.g)
      acc2.copy(gta = acc2.gta ++ emitted, g = nextG)
    }

    Right((out.gta, out.sigla, out.colors))


    (manifest \ "_").zipWithIndex).foldLeft()(
      
    )
      
    { (e, witNo) =>
      val maybeWitness: Either[String, WitnessData] = Try {
        val siglum = (e \ "@siglum").headOption.map(_.text).getOrElse {
          throw new RuntimeException(s"Missing required @siglum attribute in: ${e.toString}")
        }
        val witnessFont = (e \ "@font").headOption.map(_.text)
        val finalFont: Option[String] = witnessFont.orElse(rootFontOpt)
        // Defined as Option[String], will be None if missing or empty string
        val color = (e \ "@color").headOption.map(_.text).filter(_.nonEmpty)
        val witnessUrlAttr = (e \ "@url").head.text // pointer to witness data location (file or remote)
        val inputSource = witnessUrlAttr match
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
        Using(inputSource) { source =>
          val tValues = GtaUnifiedBuilder.tokenizeContent(
            inputSource.mkString,
            cfg
          ) // tokenize, maintain global counter, add `w` and `g`
          val witTokens = GtaUnifiedBuilder.emitFromStrings(tValues, witNo, 0)
          val tokens = witTokens._1.map(_.asInstanceOf[TokenEnum.Token])
          System.err.println(s"Next witness will start counting at ${witTokens._2}")

          WitnessData(Siglum(siglum), color, finalFont, tokens)
        }.get
      }.toEither.left.map(ex => s"Error reading witness: ${ex.getMessage}")
      System.err.println(s"Current maybeWitness: $maybeWitness")
      maybeWitness
    }
  }
  val (errors, witnesses) = results.partitionMap(identity)

  if errors.nonEmpty then Left(errors.mkString("\n"))
  else Right(witnesses)
