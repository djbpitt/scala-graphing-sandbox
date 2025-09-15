package net.collatex.reptilian

import os.Path

import scala.util.CommandLineParser
import scala.xml.*

import java.net.{URI, URL}

import ParseArgs._

/** Mimic XPath normalize-space()
  *
  *   1. Convert all newlines to space characters
  *   1. Collapse all sequences of space characters to single space
  *   1. Remove leading and trailing space characters
  */
extension (s: String) def normalizeSpace: String = raw"\s+".r.replaceAllIn(s, " ").trim

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

/** Obtain input via manifest and process
  *
  * @param args
  *   args: location of manifest and optional toggle for debug output
  * @return
  */
@main def manifest(args: String*): Unit =

  val parsedValidated
      : Either[String, (AlignmentRibbon, Vector[TokenEnum], List[Siglum], List[String], Map[String, Set[String]])] =
    for {
      ResolvedConfig(tokensPerWitnessLimit, tokenPattern, defaultColors) <- loadResolvedConfig()
      // Parse args (two-step unpacking because Scala chokes on one-step version)
      result <- parseArgs(args)
      (manifestPathString, argMap) = result
      manifestData <- resolveManifestString(manifestPathString)
      cfg = GtaBuilder.BuildConfig(tokensPerWitnessLimit, tokenPattern)
      gTaBundle <- GtaBuilder.build(manifestData, cfg, defaultColors)
      (gTa, displaySigla, colors) = gTaBundle
      root = createAlignmentRibbon(gTa)
    } yield (root, gTa, displaySigla, colors, argMap)

  parsedValidated match
    case Left(e) =>
      System.err.println(e)

    case Right((root, gTa, displaySigla, colors, argMap)) =>
      displayDispatch(root, gTa, displaySigla, colors, argMap)

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
