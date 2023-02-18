import upickle.default._

import scala.annotation.unused
import smile.nlp.{pimpString, vectorize}
import smile.clustering.{HierarchicalClustering, hclust}
import smile.plot.show
import smile.plot.swing.*
import smile.plot.Render.*
import smile.feature.transform.WinsorScaler
import smile.data.DataFrame
case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

@unused // object is used, but somehow IDE doesn't detect its usage.
object UnalignedFragment:  // add method to read from JSON into case class
  implicit val rw: ReadWriter[UnalignedFragment] = macroRW


case class ClusterInfo(item1: Int, item2: Int, height: Double, nodeType: NodeTypes)

object ClusterInfo:
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


def read_data: List[UnalignedFragment] =
  val wd = os.pwd
  println(wd)

  val datafile_path = os.pwd / "src" / "main" / "data" / "unaligned_data.json"
  val fileContents = os.read(datafile_path)
  val darwin = read[List[UnalignedFragment]](fileContents)
  darwin

def vectorize_readings(node: UnalignedFragment): Array[Array[Double]] =
  // we have got to convert each reading into a bag of words
  // that is a map/transform operation
  // have to join tokens into a string again for this library to work
  // Then we vectorize each reading using the bag of words of that reading
  // and the terms of the combined keys of all the bags of words
  // By default filter removes stopwords and stemmer stems, so turn those off
  // NB: filter has to be set to empty string, and not to None, for no obvious reason
  /*
  * StandardScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
  *   1) vectorize
  *   2) convert to DataFrame
  *   3) scale (still a DataFrame)
  *   4) convert to Array[Array[Double]] and return
  * */
  val list_bags_of_readings = node.readings.map(reading => pimpString(reading.mkString(" ")).bag(filter = "", stemmer = None))
  // println(list_bags_of_readings)

  // calculate combined keys of bags; vectorization requires all words, even those not present in a reading
  val keys = list_bags_of_readings.map(bag => bag.keySet) // list of sets of keys, one per reading, without counts
  // println(keys)
  val terms = keys.reduce(_.union(_)).toArray.sorted // merge the sit sets into one without duplicates
  // println(terms.mkString("Array(", ", ", ")")) // arrays don't have a print method; prefix, divider, postfix

  val vectors = list_bags_of_readings.map(smile.nlp.vectorize(terms, _))
  // vectors.foreach(e => println(e.mkString("Array(", ", ", ")")))
  val df = DataFrame.of(vectors.toArray)
  // println("My awesome dataframe")
  // println(df)
  val scaler = WinsorScaler.fit(df, 0.05, 0.95)
  val transformed = scaler.apply(df)
  // println(transformed)
  val df_array = transformed.toArray()
  // df_array.foreach(e => println(e.mkString(",")))
  df_array

def cluster_readings(data: Array[Array[Double]]): List[ClusterInfo] =
  val clustering = hclust(data, "ward")
  val array1 = clustering.tree
  val array2 = clustering.height
  val hc_data_as_array = array1 zip array2
  val hc_data = hc_data_as_array.map(e => ClusterInfo.of(e(0)(0), e(0)(1), e(1), data.length))
  hc_data.toList

@main def unaligned_dev(): Unit =
  val darwin: List[UnalignedFragment] = read_data
  // we know there's only one, so we could have told it to find the first
  //   and then skipped the foreach()
  darwin.filter(_.nodeno == 1146)
    .map(node => node -> vectorize_readings.andThen(cluster_readings)(node))
    .toMap
    .foreach {
      case(node, clusters) => println(node.nodeno+":"+clusters)
    }




