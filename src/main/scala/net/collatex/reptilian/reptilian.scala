package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import net.collatex.util.Hypergraph
import os.Path

//import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.xml.dtd.DocType // Create tokenization regex here but tokenize in tokenization.scala

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

@main def main(): Unit =
  /** Select data */
  val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin"

  /** Prepare tokenizer
    *
    * Sequences of non-word characters (except spaces) are entire tokens Unlike in CollateX Python, punctuation
    * characters are their own tokens
    */
  val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = makeTokenizer(
    tokenPattern
  ) // Tokenizer function with user-supplied regex

  /** Read data into token array */
  val witnessInputInfo: List[(String, String)] = readData(
    pathToDarwin
  ) // One string per witness
  val witnessStrings: List[String] = witnessInputInfo.map(_._2)
  val sigla: List[Siglum] = witnessInputInfo.map(_._1).map(Siglum(_))
  given gTa: Vector[TokenEnum] = tokenize(tokenizer)(witnessStrings) // global token array

  /** Create alignment ribbon
    */
  val root: AlignmentRibbon = createAlignment(sigla)
  // ===
  // Temporary code to isolate phase-two candidates
  // ===
  val phaseTwoCandidates: Vector[WitnessReadings] =
    flattenNodeSeq(root)
      .filter(_._1.witnessGroups.size > 1)
      .map(_._1.combineWitnessGroups)
  // phaseTwoCandidates.foreach(println)
  val phaseTwoCandidateGroups: Map[Boolean, Vector[WitnessReadings]] = phaseTwoCandidates
    .groupBy(e => e.values.forall(f => f.getClass.getSimpleName == "LegalTokenRange"))
  // phaseTwoCandidateGroups(false).foreach(println) // Cannot be rendered with current code
  val validPhaseTwoCandidates = phaseTwoCandidateGroups(true) // 318 items
  val phaseTwoCandidateTokens: Vector[List[List[Token]]] =
    validPhaseTwoCandidates // NB: Valid groups only
      .map(e =>
        e.values.map(f => gTa.slice(f.start, f.until).asInstanceOf[Vector[Token]].toList).toList.sortBy(_.head.w)
      )
  def processUnalignedZone(tokens: List[List[Token]]): Hypergraph[EdgeLabel, TokenRange] =
    val nodesToCluster: List[ClusterInfo] = clusterWitnesses(tokens)
    val hg: Hypergraph[EdgeLabel, TokenRange] =
      mergeClustersIntoHG(nodesToCluster, tokens)
    hg
  val phaseTwoAlignmentHypergraphs =
    phaseTwoCandidateTokens.map(e =>
      println(e)
      processUnalignedZone(e)
    )
  phaseTwoAlignmentHypergraphs.foreach(println)

  // ===
  // End of temporary code
  // ===
  val doctypeHtml: scala.xml.dtd.DocType = DocType("html")
  val horizontalRibbons = createHorizontalRibbons(root, sigla.toSet)
  val horizontalRibbonsPath =
    os.pwd / "src" / "main" / "outputs" / "horizontal-ribbons-full.xhtml" // "horizontal-ribbons.xhtml"
  scala.xml.XML.save(horizontalRibbonsPath.toString, horizontalRibbons, "UTF-8", true, doctypeHtml)
