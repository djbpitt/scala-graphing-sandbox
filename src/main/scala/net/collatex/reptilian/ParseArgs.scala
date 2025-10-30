package net.collatex.reptilian

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

object ParseArgs {

  /** Parse command line arguments
    *
    * @param args
    *   First argument is treated as path or url for manifest. Second, if present, turns on debug reporting if equal to
    *   "debug"; otherwise ignored.
    * @return
    *   Tuple of manifest path as string and arguments: values as Map[String, Set[String] ]
    *
    * Values are Set[String] because `--format` can take multiple values
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
        |  -d, --debug
        |      Output traversal graph visualization in traversalGraphs subdirectory of output directory. Ignored if no
        |      --output value is specified.
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
      "--output" -> "--output",
      "-d" -> "--debug",
      "--debug" -> "--debug"
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

}
