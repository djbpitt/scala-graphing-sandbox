import upickle.default.*
import scala.annotation.unused
import smile.nlp.{pimpString, vectorize}
import smile.clustering.{HierarchicalClustering, hclust}
import smile.plot.show
import smile.plot.swing.*
import smile.plot.Render.*
import smile.feature.transform.WinsorScaler
import smile.data.DataFrame

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
def read_data: List[UnalignedFragment] =
  val datafile_path = os.pwd / "src" / "main" / "data" / "unaligned_data.json"
  val fileContents = os.read(datafile_path)
  val darwin = read[List[UnalignedFragment]](fileContents)
  darwin

def vectorize_readings(node: UnalignedFragment): Array[Array[Double]] =
  // have to join tokens into a string again for this library to work
  // By default filter removes stopwords and stemmer stems, so turn those off
  // NB: filter has to be set to empty string, and not to None, for no obvious reason
  /*
  * WinsorScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
  *   1) vectorize
  *   2) convert to DataFrame
  *   3) scale (still a DataFrame)
  *   4) convert to Array[Array[Double]] and return
  * */
  val list_bags_of_readings = node
    .readings
    .map(reading => pimpString(reading.mkString(" ")).bag(filter = "", stemmer = None))
  // calculate combined keys of bags; vectorization requires all words, even those not present in a reading
  val terms = list_bags_of_readings
    .map(_.keySet)
    .reduce(_ union _)
    .toArray
    .sorted
  val array_of_vectors = list_bags_of_readings
    .map(smile.nlp.vectorize(terms, _))
    .toArray
  val df = DataFrame.of(array_of_vectors)
  val scaler = WinsorScaler.fit(df, 0.05, 0.95)
  val transformed = scaler(df)
  val df_array = transformed.toArray()
  df_array

def cluster_readings(data: Array[Array[Double]]): List[ClusterInfo] =
  val clustering = hclust(data, "ward")
  // data.length = number of vectors = number of witnesses, needed to construct ClusterInfo object
  (clustering.tree zip clustering.height)
    .map(e => ClusterInfo.of(e(0)(0), e(0)(1), e(1), data.length))
    .toList

@main def unaligned_dev(): Unit =
  val darwin: List[UnalignedFragment] = read_data
  // we know there's only one, so we could have told it to find the first
  //   and then skipped the foreach()
  darwin.filter(_.nodeno == 1146)
    .map(node => node -> vectorize_readings.andThen(cluster_readings)(node))
    .toMap
    .foreach {
      case(node, clusters) => println(s"${node.nodeno}:$clusters")
    }




