package net.collatex.reptilian

import net.collatex.util.Hypergraph

import scala.collection.mutable.ListBuffer
import scala.xml.dtd.DocType

def createSecondAlignmentPhaseVisualization(hg: Hypergraph[EdgeLabel, TokenRange]): Unit =
  val gTa = hg.verticesIterator.next.ta
  val id: String = hg.hyperedges.head.label.toString // unique id for hg
  // println(s"hypergraph: $hg")
  val ranking: Map[NodeType, Int] = hg.rank()
  val hyperedgesByRank = hg.hyperedges.groupBy(e => ranking(NodeType(e.label))) // unsorted
  val sortedRanks = hyperedgesByRank.keySet.toSeq.sorted
  val aps: ListBuffer[AlignmentUnit] = sortedRanks
    .map(e =>
      val wg = hyperedgesByRank(e) // set of hyperedges, one per witness group on alignment point
        .map(f =>
          f.verticesIterator
            .map(tr =>
              val witness: Siglum = {
                val inSiglum: String = gTa(tr.start).w.toString
                if inSiglum.length == 1 then Siglum(intToSiglum(inSiglum.toInt))
                else Siglum(inSiglum)
              }
              witness -> tr
            )
            .toMap
        )
      val wr = wg.reduce(_ ++ _)
      AlignmentPoint(wr, wg)
    )
    .to(ListBuffer)
  val result = AlignmentRibbon(aps)
  // aps.foreach(e => println(e))
  val localSigla = (0 until 6).map(e => Siglum(intToSiglum(e))).toSet
  val horizontalRibbons = createHorizontalRibbons(result, localSigla, gTa)
  val doctypeHtml: scala.xml.dtd.DocType = DocType("html")
  val horizontalRibbonsPath =
    os.pwd / "src" / "main" / "outputs" / s"secondAlignmentPhase-$id.xhtml" // unique names
  scala.xml.XML.save(horizontalRibbonsPath.toString, horizontalRibbons, "UTF-8", true, doctypeHtml)
