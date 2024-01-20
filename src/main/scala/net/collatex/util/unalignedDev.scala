package net.collatex.util

import smile.clustering.hclust
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler
import smile.nlp.vectorize
import upickle.default.*

import scala.annotation.unused

/*
 * Domain classes, companion objects, enums
 * */
case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

@unused // object is used, but somehow IDE doesn't detect its usage.
object UnalignedFragment: // add method to read from JSON into case class
  implicit val rw: ReadWriter[UnalignedFragment] = macroRW

case class ClusterInfo(
    item1: Int,
    item2: Int,
    height: Double,
    nodeType: NodeTypes
)

object ClusterInfo:
  // "of" is conventional name for constructor
  def of(
      item1: Int,
      item2: Int,
      height: Double,
      witnessCount: Int
  ): ClusterInfo =
    val nodeType =
      (item1 < witnessCount, item2 < witnessCount) match
        case (true, true) => NodeTypes.SingletonSingleton
        case (true, false) =>
          NodeTypes.SingletonTree // Assume singleton is first
        case (false, false) => NodeTypes.TreeTree
        case _              => throw Exception("(false, true) should not occur")
    ClusterInfo(item1, item2, height, nodeType)

enum NodeTypes:
  case SingletonSingleton, SingletonTree, TreeTree

/*
 * Functions to manipulate unaligned nodes
 * */
def readJsonData: List[UnalignedFragment] =
  val datafilePath =
    os.pwd / "src" / "main" / "data" / "unaligned_data_node_296.json"
  val fileContents = os.read(datafilePath)
  val darwin = read[List[UnalignedFragment]](fileContents)
  darwin


/** Create bag of readings for each witness
 *
 * Each bag includes all types found in any witness, so some may have a zero count
 *
 * @param readings List of one list of strings (token instances) for each witness
 * @return List of maps (one per witness) of token -> frequency (possibly zero)
 */
private def bag(readings: List[List[String]]): List[Map[String, Int]] =
  val allTypes = readings.flatten.toSet // shared list of all tokens
  def oneBag(reading: List[String]): Map[String, Int] =
    val typesInReading = reading.groupBy(identity).map((k, v) => (k, v.length))
    val allTypesForReading = allTypes
      .map(e => e -> typesInReading.getOrElse(e, 0))
    allTypesForReading.toMap
  val result = readings.map(oneBag)
  result

def vectorizeReadings(node: UnalignedFragment): Array[Array[Double]] =
  /*
   * WinsorScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
   *   1) vectorize
   *   2) convert to DataFrame
   *   3) scale (still a DataFrame)
   *   4) convert to Array[Array[Double]] and return
   * */
  val listBagsOfReadings = bag(node.readings)
  val terms = listBagsOfReadings
    .map(_.keySet)
    .reduce(_ union _)
    .toArray
    .sorted
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
  darwin
    .map(node =>
      node -> (vectorizeReadings andThen clusterReadings)(node)
    ) // list of tuples
    .toMap // map object (key -> value pairs)
    .foreach { (node, clusters) =>
      println(s"${node.nodeno}:$clusters")
    }
