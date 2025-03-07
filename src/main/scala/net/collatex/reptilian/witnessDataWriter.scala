package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import upickle.default.write

// ===
// For temporary use
// Writes phase two candidates to disk as JSON
// ===

def writePhaseTwoJSONData(root: AlignmentRibbon): Unit =
  val phaseTwoCandidates: Vector[WitnessReadings] =
    flattenNodeSeq(root)
      .filter(_._1.witnessGroups.size > 1)
      .map(_._1.witnessReadings)
  // phaseTwoCandidates.foreach(println)
  val phaseTwoCandidateGroups: Map[Boolean, Vector[WitnessReadings]] = phaseTwoCandidates
    .groupBy(e => e.values.forall(f => f.getClass.getSimpleName == "LegalTokenRange"))
  // phaseTwoCandidateGroups(false).foreach(println) // Cannot be rendered with current code
  val validPhaseTwoCandidates = phaseTwoCandidateGroups(true) // 318 items
  val phaseTwoCandidateTokens: Vector[List[List[Token]]] =
    validPhaseTwoCandidates
      .map(e =>
        e.values
          .map(f =>
            f.tokens
              .asInstanceOf[Vector[Token]]
              .toList
          )
          .toList
          .sortBy(_.head.w)
      )
//  def processUnalignedZone(tokens: List[List[Token]]): Hypergraph[EdgeLabel, TokenRange] =
//    val nodesToCluster: List[ClusterInfo] = clusterWitnesses(tokens)
//    val hg: Hypergraph[EdgeLabel, TokenRange] =
//      mergeClustersIntoHG(nodesToCluster, tokens, gTa)
//    hg
  
  phaseTwoCandidateTokens // write unaligned zones to disk
  .foreach(e =>
    val jsonData = e.map(_.map(f => TokenJSON(f.t, f.n, f.w, f.g)))
    val filenameIntegerId = e.head.head.g.toString
    val unalignedZonePath =
      os.pwd / "src" / "main" / "outputs" / "unalignedZones" / s"$filenameIntegerId.json"
    os.write.over(unalignedZonePath, write(jsonData))
    // e => processUnalignedZone(e)
  )
