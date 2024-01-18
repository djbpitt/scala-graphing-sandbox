package net.collatex.util

import smile.clustering.{HierarchicalClustering, hclust}
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler
import smile.nlp.{pimpString, vectorize}
import smile.plot.Render.*
import smile.plot.show
import smile.plot.swing.*
import upickle.default.*

import scala.annotation.unused

/*
 * Domain classes, companion objects, enums
 * */
case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

@unused // object is used, but somehow IDE doesn't detect its usage.
object UnalignedFragment:  // add method to read from JSON into case class
  implicit val rw: ReadWriter[UnalignedFragment] = macroRW

case class ClusterInfo(item1: Int, item2: Int, height: Double, nodeType: NodeTypes)

object ClusterInfo:
  // "of" is conventional name for constructor
  def of(item1: Int, item2: Int, height: Double, witnessCount: Int):ClusterInfo =
    val nodeType =
      (item1 < witnessCount, item2 < witnessCount) match
        case (true, true) => NodeTypes.SingletonSingleton
        case (true, false) => NodeTypes.SingletonTree // Assume singleton is first
        case (false, false) => NodeTypes.TreeTree
        case _ => throw Exception("(false, true) should not occur")
    ClusterInfo(item1, item2, height, nodeType)

enum NodeTypes:
  case SingletonSingleton, SingletonTree, TreeTree

/*
 * Functions to manipulate unaligned nodes
 * */
def readJsonData: List[UnalignedFragment] =
  val datafilePath = os.pwd / "src" / "main" / "data" / "unaligned_data_node_296.json"
  val fileContents = os.read(datafilePath)
  val darwin = read[List[UnalignedFragment]](fileContents)
  darwin

def vectorizeReadings(node: UnalignedFragment): Array[Array[Double]] =
  /*
  * WinsorScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
  *   1) vectorize
  *   2) convert to DataFrame
  *   3) scale (still a DataFrame)
  *   4) convert to Array[Array[Double]] and return
  * */
  val listBagsOfReadings = node
    .readings
    .map(reading => pimpString(reading.mkString(" "))) // normal string doesn't have .bag() method
    // NB: filter has to be set to empty string, and not to None, for no obvious reason
    .map(_.bag(filter = "", stemmer = None)) // By default filter removes stopwords and stemmer stems, so turn those off
  // calculate combined keys of bags; vectorization requires all words, even those not present in a reading
  val terms = listBagsOfReadings
    .map(_.keySet)
    .reduce(_ union _)
    .toArray
    .sorted
  println(terms.mkString(" "))
  val arrayOfVectors = listBagsOfReadings
    .map(smile.nlp.vectorize(terms, _))
    .toArray
  val df = DataFrame.of(arrayOfVectors)
  val scaler = WinsorScaler.fit(df, 0.05, 0.95)
  val transformed = scaler(df)
  val dfArray = transformed.toArray()
  dfArray

def clusterReadings(data: Array[Array[Double]]): List[ClusterInfo] =
  val clustering = hclust(data, "ward")
  // data.length = number of vectors = number of witnesses, needed to construct ClusterInfo object
  (clustering.tree zip clustering.height)
    .map(e => ClusterInfo.of(e(0)(0), e(0)(1), e(1), data.length))
    .toList

@main def unalignedDev(): Unit =
  val darwin: List[UnalignedFragment] = readJsonData
  // we know there's only one, so we could have told it to find the first
  //   and then skipped the foreach()
  darwin.filter(_.nodeno == 3)
    .map(node => node -> (vectorizeReadings andThen clusterReadings)(node)) // list of tuples
    .toMap // map object (key -> value pairs)
    .foreach {
      (node, clusters) => println(s"${node.nodeno}:$clusters")
    }




