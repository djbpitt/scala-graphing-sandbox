package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import smile.clustering.hclust
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler


enum ClusterInfo:
  def item1: Int
  def item2: Int
  def height: Double
  case SingletonSingleton(item1: Int, item2: Int, height: Double)
  case SingletonHG(item1: Int, item2: Int, height: Double)
  case HGHG(item1: Int, item2: Int, height: Double)
export ClusterInfo._

object ClusterInfo:
  // "of" is conventional name for constructor; HG = hypergraph
  def of(
          item1: Int,
          item2: Int,
          height: Double,
          witnessCount: Int
        ): ClusterInfo =
    (item1 < witnessCount, item2 < witnessCount) match
      case (true, true) => SingletonSingleton(item1, item2, height)
      case (true, false) =>
        SingletonHG(item1, item2, height) // Assume singleton is first
      case (false, false) => HGHG(item1, item2, height)
      case _              => throw Exception("(false, true) should not occur")

def clusterWitnesses = 
  vectorizeReadings andThen clusterReadings

/** Create bag of readings for each witness
 *
 * Each bag includes all types found in any witness, so some may have a zero count
 *
 * @param readings
 *   List of one list of strings (token instances) for each witness
 * @return
 *   List of maps (one per witness) of token -> frequency (possibly zero)
 */
private def bag(readings: List[List[Token]]): List[Map[String, Int]] =
  val nValues = readings.map(_.map(_.n))
  val allTypes: Set[String] = nValues.flatten.toSet // shared list of all tokens
  def oneBag(reading: List[String]): Map[String, Int] =
    val typesInReading: Map[String, Int] = reading.groupBy(identity).map((k, v) => (k, v.length))
    val allTypesForReading = allTypes
      .map(e => e -> typesInReading.getOrElse(e, 0))
    allTypesForReading.toMap
  val result = nValues.map(oneBag)
  result

def vectorizeReadings(node: List[List[Token]]): Array[Array[Double]] =
  /* WinsorScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
   *   1) vectorize
   *   2) convert to DataFrame
   *   3) scale (still a DataFrame)
   *   4) convert to Array[Array[Double]] and return
   * */
  val listBagsOfReadings = bag(node)
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
  val result = (clustering.tree zip clustering.height)
    .map(e => ClusterInfo.of(e(0)(0), e(0)(1), e(1), data.length))
    .toList
  result


