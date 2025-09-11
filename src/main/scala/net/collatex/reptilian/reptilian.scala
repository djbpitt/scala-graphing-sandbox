package net.collatex.reptilian

import ManifestValidator._

import os.Path
import ujson.Value
import org.virtuslab.yaml.*

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{CommandLineParser, Try, Using}
import scala.xml.*

// To process JSON input
import net.collatex.reptilian.WitnessJsonData.*

// Scala 3 prohibits local returns and uses boundary.break instead
import java.net.{URI, URL}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

/** Mimic XPath normalize-space()
  *
  *   1. Convert all newlines to space characters
  *   1. Collapse all sequences of space characters to single space
  *   1. Remove leading and trailing space characters
  */
extension (s: String) def normalizeSpace: String = raw"\s+".r.replaceAllIn(s, " ").trim

/** Read data files from supplied path to directory (one file per witness)
  *
  * @param pathToData
  *   os.Path object that points to data directory
  * @return
  *   List of tuples of 1) abbreviated filename and 2) string (token lists)
  */
def readData(pathToData: Path): List[(String, String)] =
  os.walk(
    path = pathToData,
    skip = _.last.startsWith(".")
  ) // exclude hidden files
    .sorted
    .toList
    .map(e => (e.last, os.read(e)))

def retrieveManifestJson(source: ManifestSource): Either[String, String] = {
  source match
    case ManifestSource.Local(path) =>
      try
        val content = os.read(path) // using OS-lib to read local file
        Right(content)
      catch case e: Exception => Left(s"Failed to read local JSON manifest: ${e.getMessage}")
    case ManifestSource.Remote(url) =>
      try {
        val stream = new URI(url.toString).toURL.openStream()
        val content =
          try scala.io.Source.fromInputStream(stream).mkString
          finally stream.close()
        Right(content)
      } catch {
        case e: Exception => Left(s"Failed to fetch remote JSON manifest: ${e.getMessage}")
      }
}

// For config.yaml
case class Config(tokensPerWitnessLimit: Option[Int] = None, tokenPattern: String, defaultColors: List[String])
    derives YamlCodec

/** Obtain input via manifest and process
  *
  * @param args
  *   args: location of manifest and optional toggle for debug output
  * @return
  */
@main def manifest(args: String*): Unit =
  // ---- constants used later ----
  val configYaml: String = scala.io.Source.fromResource("config.yaml").mkString
  val config = configYaml.as[Config].getOrElse(throw new RuntimeException("Missing or invalid config.yaml"))
  val tokensPerWitnessLimit = config.tokensPerWitnessLimit.getOrElse(Int.MaxValue)
  val tokenPattern = Regex(config.tokenPattern)
  val defaultColors = config.defaultColors

  // Parse args, resolve manifest
  val parsedValidated: Either[String, (ManifestData, Map[String, Set[String]])] =
    for {
      // Parse args (two-step unpacking because Scala choked on one-step version)
      result <- parseArgs(args)
      (manifestPathString, argMap) = result
      manifestData <- resolveManifestString(manifestPathString)

      // gTaTuple <- GtaBuilder.build(manifestData, cfg)
      // (gTa, gTaSigla, colors) = gTaTuple
      // root <- createAlignmentRibbon(gTaSigla, gTa)
      // Yield here; manage output outside for-comprehension
    } yield (manifestData, argMap)

  parsedValidated match
    case Left(e) =>
      System.err.println(e)

    case Right((manifestData, argMap)) =>
      val cfg = GtaBuilder.BuildConfig(tokensPerWitnessLimit, tokenPattern)
      GtaBuilder.build(manifestData, cfg) match
        case Left(err) =>
          System.err.println(s"Unified builder failed: $err")
        case Right((gTa, siglaList, colorList)) =>
          val gTaSigla: List[WitId] = siglaList.indices.toList
          val root: AlignmentRibbon = createAlignmentRibbon(gTaSigla, gTa)
          displayDispatch(root, gTa, siglaList, colorList, argMap)

/** Parse command line arguments
  *
  * @param args
  *   First argument is treated as path or url for manifest. Second, if present, turns on debug reporting if equal to
  *   "debug"; otherwise ignored.
  * @return
  *   Tuple of manifest path as string and debug as Boolean
  */
def parseArgs(args: Seq[String]): Either[String, (String, Map[String, Set[String]])] =
  val usage =
    """
      |Usage: java -jar collatex-reptilian-<version>.jar <manifest-filename> [options]
      |
      |<manifest-filename> must end with '.xml' or '.json'. See TBA for structural description.
      |
      |Options:
      |
      |  -f, --format <formats...>
      |      Space-separated list of output formats (one or more values required if '--format' switch is present).
      |      Allowed formats:
      |        ribbon        Alignment ribbon (HTML)
      |        table         Plain-text table, horizontal (one row per witness) (default)
      |        table-h       Same as "table"
      |        table-v       Plain-text table, vertical (one column per witness)
      |        table-html-h  HTML table, horizontal (one row per witness)
      |        table-html-v  HTML table, vertical (one column per witness)
      |        json          JSON output (TBA)
      |        svg           Rhine delta (variant graph) as SVG (one reading per token)
      |        svg-rich      Rhine delta (variant graph) as SVG (one reading per witness per token)
      |        graphml       GraphML XML output
      |        tei           TEI XML (parallel segmentation)
      |        xml           XML
      |      See TBA for output format documentation.
      |
      |  -h, --html <html-option>
      |      Value required if '--html' switch is present. Output file extension for HTML formats:
      |      Allowed values: html (default), xhtml
      |
      |  -o, --output <base-path>
      |      Value required if '--output' switch is present. Base path for output files (may include directories).
      |      The parent directory must exist and be writable. The last path component is treated as the base filename;
      |      output extensions like '.svg' or '.txt' will be appended automatically.
      |
      |Notes:
      |  - All switches are optional. If present, each must be followed by the required number of values.
      |  - Defaults apply only when switches are omitted entirely, not when provided without values, which
      |    is an error.
      |""".stripMargin

  val aliasMap: Map[String, String] = Map(
    "-f" -> "--format",
    "--format" -> "--format",
    "-h" -> "--html",
    "--html" -> "--html",
    "-o" -> "--output",
    "--output" -> "--output"
  )

  args.toList match
    case Nil =>
      Left("Error: Missing required filename argument.\n" + usage)
    case manifestFilename :: rest =>
      @tailrec
      def nextArg(
          argQueue: Seq[String],
          acc: Map[String, Set[String]],
          currentSwitch: Option[String]
      ): Either[String, Map[String, Set[String]]] =
        argQueue match
          case Nil => Right(acc)
          case head +: tail if head.startsWith("-") =>
            aliasMap.get(head) match
              case None => Left(s"Error: Unknown switch: '$head'.\n" + usage)
              case Some(canonicalSwitch) =>
                if acc.contains(canonicalSwitch) then
                  Left(s"Error: Duplicate switch detected: '$head' (alias for $canonicalSwitch).\n" + usage)
                else nextArg(tail, acc.updated(canonicalSwitch, Set.empty), Some(canonicalSwitch))

          case head +: tail =>
            currentSwitch match
              case Some(switch) =>
                val updatedValues = acc(switch) + head
                nextArg(tail, acc.updated(switch, updatedValues), currentSwitch)
              case None =>
                Left(s"Error: Value '$head' without preceding switch.\n" + usage)

      nextArg(rest, Map.empty, None).flatMap { parsedMap =>
        val formatVals = parsedMap.getOrElse("--format", Set.empty)
        val htmlVals = parsedMap.getOrElse("--html", Set.empty)
        val outputVals = parsedMap.getOrElse("--output", Set.empty)
        val allowedFormatsList = Vector(
          "ribbon",
          "table",
          "table-h",
          "table-v",
          "table-html-h",
          "table-html-v",
          "json",
          "svg",
          "svg-rich",
          "graphml",
          "tei",
          "xml"
        )
        val allowedFormatsSet = allowedFormatsList.toSet
        val allowedHtml = Set("html", "xhtml")

        if parsedMap.contains("--format") && formatVals.isEmpty then
          Left("Error: '--format' requires at least one value if provided.\n" + usage)
        else if parsedMap.contains("--format") && formatVals.size > 1 && !parsedMap.contains("--output") then
          Left(
            "Error: If you specify more than one '--format' value you must also specify an '--output' value.\n" + usage
          )
        else if parsedMap.contains("--html") && htmlVals.size != 1 then
          Left("Error: '--html' requires exactly one value if provided.\n" + usage)
        else if parsedMap.contains("--html") && !allowedHtml.contains(htmlVals.head) then
          Left(s"Error: '--html' value must be one of: ${allowedHtml.mkString(", ")}.\n" + usage)
        else if parsedMap.contains("--format") && !formatVals.subsetOf(allowedFormatsSet) then
          val invalid = formatVals.diff(allowedFormatsSet)
          Left(
            s"Error: Invalid '--format' values: ${invalid.mkString(", ")}. Allowed values: ${allowedFormatsList
                .mkString(", ")}.\n" + usage
          )
        else if parsedMap.contains("--output") && outputVals.size != 1 then
          Left("Error: '--output' requires exactly one value if provided.\n" + usage)
        else if parsedMap.contains("--output") then
          val outputPath = Paths.get(outputVals.head).toAbsolutePath
          val parent = Option(outputPath.getParent).getOrElse(Paths.get(".").toAbsolutePath)
          if !Files.exists(parent) || !Files.isDirectory(parent) then
            Left(s"Error: '--output' parent directory does not exist: $parent.\n" + usage)
          else if !Files.isWritable(parent) then
            Left(s"Error: '--output' parent directory is not writable: $parent.\n" + usage)
          else if outputPath.getFileName.toString.isEmpty || outputPath.getFileName.toString.matches("\\.*") then
            Left("Error: '--output' must specify a valid, non-empty file name component.\n" + usage)
          else Right((manifestFilename, parsedMap))
        else if !(manifestFilename.endsWith(".xml") || manifestFilename.endsWith(".json")) then
          Left("Error: Manifest filename must end with '.xml' or '.json'.\n" + usage)
        else Right((manifestFilename, parsedMap))
      }

/** Resolves manifest location (input as string) as absolute path (if local) or URL (if remote)
  *
  * @param manifestPathString
  *   Location of manifest from command line (absolute file system path, relative file system path, or remote url)
  * @return
  *   Absolute file system path or url if success; string with error report if not.
  */
def resolveManifestString(manifestPathString: String): Either[String, ManifestData] =
  val format =
    if manifestPathString.endsWith(".xml") then ManifestFormat.Xml
    else if manifestPathString.endsWith(".json") then ManifestFormat.Json
    else return Left("Manifest filename must end with .xml or .json")

  if manifestPathString.startsWith("http://") || manifestPathString.startsWith("https://") then
    try Right(ManifestData(ManifestSource.Remote(URI.create(manifestPathString).toURL), format))
    catch
      case ex: Exception =>
        Left(s"Invalid URL: ${ex.getMessage}")
  else
    val absoluteManifestPath = os.Path(manifestPathString, os.pwd)
    if os.exists(absoluteManifestPath) then Right(ManifestData(ManifestSource.Local(absoluteManifestPath), format))
    else Left(s"Manifest file cannot be found: $absoluteManifestPath")

/** Retrieves manifest from either file system path or remote url
  *
  * @param source
  *   Location of manifest as file system path (absolute or relative to manifest file) or remote url
  * @return
  *   Manifest as xml if successful; error report as string if not.
  */
def retrieveManifestXml(source: ManifestSource): Either[String, Elem] =
  val manifestXml: Elem =
    source match
      case ManifestSource.Local(path) =>
        try XML.loadFile(path.toString)
        catch
          case _: Exception =>
            return Left(s"Manifest was found at $path, but could not be loaded as an XML document")

      case ManifestSource.Remote(url) =>
        try XML.load(url)
        catch
          case _: Exception =>
            return Left(s"URL $url was found but could not be loaded as an XML document")

  Right(manifestXml)

/** Retrieves witness data from witness sources in manifest
  *
  * @param manifest
  *   manifest as xml document
  * @param manifestSource
  *   location of manifest: file system path or remote (http:// or https://) url.
  * @return
  *   Sequence of CollateXWitnessData instances if successful; otherwise error messages as string
  */
def retrieveWitnessDataXml(
    manifest: Elem,
    manifestSource: ManifestData
): Either[String, Seq[CollateXWitnessData]] =
  val rootFontOpt = (manifest \ "@font").headOption.map(_.text)
  val results: Seq[Either[String, CollateXWitnessData]] =
    (manifest \ "_").map { e =>
      val maybeWitness: Either[String, CollateXWitnessData] = Try {
        val siglum = (e \ "@siglum").headOption.map(_.text).getOrElse {
          throw new RuntimeException(s"Missing required @siglum attribute in: ${e.toString}")
        }
        val witnessFont = (e \ "@font").headOption.map(_.text)
        val finalFont: Option[String] = witnessFont.orElse(rootFontOpt)
        // Defined as Option[String], will be None if missing or empty string
        val color = (e \ "@color").headOption.map(_.text).filter(_.nonEmpty)
        val witnessUrlAttr = (e \ "@url").head.text
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
          CollateXWitnessData(Siglum(siglum), color, finalFont, source.getLines().mkString(" "))
        }.get
      }.toEither.left.map(ex => s"Error reading witness: ${ex.getMessage}")
      maybeWitness
    }

  val (errors, witnesses) = results.partitionMap(identity)

  if errors.nonEmpty then Left(errors.mkString("\n"))
  else Right(witnesses)

def retrieveWitnessDataJson(
    jsonString: String,
    manifestSource: ManifestData
): Either[String, Seq[WitnessJsonData]] = boundary {
  val json: ujson.Value = ujson.read(jsonString)
  val rootFontOpt = json.obj.value.get("font").map(_.str)
  val witnesses = json("witnesses").arr.toSeq
  var gCounter = 0
  val result = scala.collection.mutable.ListBuffer.empty[WitnessJsonData]

  for ((w, witnessIndex) <- witnesses.zipWithIndex) {
    val siglum = w("id").str
    val witnessFont = w.obj.value.get("font").map(_.str)
    val finalFont = witnessFont.orElse(rootFontOpt)
    if (w.obj.contains("content")) {
      val content = w("content").str
      val color = w.obj.get("color").map(_.str)
      result += WitnessJsonData.FromContent(CollateXWitnessData(Siglum(siglum), color, finalFont, content))
    } else if (w.obj.contains("tokens")) {
      val tokensJson = w("tokens").arr.toSeq
      val tokens: Seq[TokenEnum.Token] = tokensJson.map { tokenObj =>
        val tField = tokenObj.obj.get("t").map(_.str).getOrElse {
          break(Left(s"Missing required 't' property in a token of witness '$siglum'"))
        }

        val nField = tokenObj.obj.get("n").map(_.str).getOrElse(normalize(tField))
        val wField = witnessIndex
        val gField = gCounter

        gCounter += 1

        val knownKeys = Set("t", "n", "w", "g")
        val otherFields = tokenObj.obj.view.filterKeys(k => !knownKeys.contains(k)).toMap

        TokenEnum.Token(t = tField, n = nField, w = wField, g = gField, other = otherFields)
      }

      gCounter += 1 // skip one between witnesses
      result += WitnessJsonData.FromTokens(Siglum(siglum), tokens, finalFont)
    } else { // Scala 3 idiom for non-local return from inside map
      break(Left(s"Witness '$siglum' must contain either 'content' or 'tokens'"))
    }
  }

  Right(result.toSeq)
}

/** Data retrieved from link in manifest
  *
  * @param siglum
  *   User-supplied string for rendering
  * @param color
  *   User-supplied string to color ribbon
  * @param content
  *   Plain-text string, to be tokenized
  */
case class CollateXWitnessData(
    siglum: Siglum,
    color: Option[String] = None,
    font: Option[String] = None,
    content: String
)

/** Unified witnessData case class for both XML and JSON input
  *
  * @param siglum
  *   Siglum; user-supplied name, used for output
  * @param color
  *   Option[String]; used in alignment ribbon output
  * @param font
  *   Option[String]; used in HTML and SVG output formats
  * @param tokens
  *   Seq[TokenEnum.Token]; `t`, `n`, `w`, and `g` properties
  */
case class WitnessData(
    siglum: Siglum,
    color: Option[String], // May be missing from manifest, but then positional default is added to case class
    font: Option[String] = None, // May be missing from manifest and case class (uses global default)
    tokens: Seq[TokenEnum.Token]
)

/** Data retrieved from JSON manifest
  *
  * `content` property is a string, which can be tokenized in the same way as strings retrieved from an XML manifest
  *
  * `tokens` is pretokenized JSON, which needs to be cleaned up:
  *
  *   - `n` properties created if they aren’t present
  *   - `w` and`g` properties created
  *   - `other` property managed as `Map`
  */
enum WitnessJsonData:
  case FromContent(id: CollateXWitnessData)
  case FromTokens(
      id: Siglum,
      tokens: Seq[TokenEnum.Token],
      font: Option[String] = None
  )
  // Common accessor for siglum (temporary, during migration)
  def siglum: Siglum = this match
    case FromContent(data)    => data.siglum
    case FromTokens(id, _, _) => id

  // Common accessor for font (temporary, during migration)
  def fontOpt: Option[String] = this match
    case FromContent(data)      => data.font
    case FromTokens(_, _, font) => font

/** ManifestData has two properties:
  *
  * Source: Remote (URL) or local (Path), where local can be absolute or relative
  *
  * Format: XML or JSON
  */
enum ManifestSource:
  case Remote(url: URL)
  case Local(path: os.Path)
enum ManifestFormat:
  case Xml
  case Json
case class ManifestData(source: ManifestSource, format: ManifestFormat)
