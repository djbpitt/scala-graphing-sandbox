package net.collatex.reptilian

import net.collatex.util.Hypergraph

import scala.collection.mutable.ListBuffer
import scala.xml.dtd.DocType

def createSecondAlignmentPhaseVisualization(hg: Hypergraph[EdgeLabel, TokenRange]): Unit =
  val gTa = hg.vertices.head.ta
  // println(s"hypergraph: $hg")
  val ranking: Map[NodeType, Int] = rankHg(hg, true)
  val hyperedgesByRank = hg.hyperedges.groupBy(e => ranking(NodeType(e.label))) // unsorted
  val sortedRanks = hyperedgesByRank.keySet.toSeq.sorted
  val aps: ListBuffer[AlignmentUnit] = sortedRanks
    .map(e =>
      val stuff = hyperedgesByRank(e) // set of hyperedges, one per witness group on alignment point
        .map(f =>
          f.vertices
            .map(tr =>
              val witness: Siglum = Siglum(gTa(tr.start).w.toString)
              witness -> tr
            )
            .toMap
        )
      AlignmentPoint(stuff)
    )
    .to(ListBuffer)
  val result = AlignmentRibbon(aps)
  // aps.foreach(e => println(e))
  val localSigla = (0 until 6).map(e => Siglum(e.toString)).toSet
  val horizontalRibbons = createHorizontalRibbons(result, localSigla, gTa)
  val doctypeHtml: scala.xml.dtd.DocType = DocType("html")
  val horizontalRibbonsPath =
    os.pwd / "src" / "main" / "outputs" / "secondAlignmentPhase.xhtml" // "horizontal-ribbons.xhtml"
  scala.xml.XML.save(horizontalRibbonsPath.toString, horizontalRibbons, "UTF-8", true, doctypeHtml)
