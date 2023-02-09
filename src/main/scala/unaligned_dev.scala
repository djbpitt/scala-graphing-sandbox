import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsArray, JsObject, Json, Reads, __}

import scala.annotation.unused
import smile.nlp.pimpString
case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

// object is used, but somehow IDE doesn't detect it's usage.
@unused
object UnalignedFragment:
  implicit val reads: Reads[UnalignedFragment] = (
    (__ \ "nodeno").read[Int] and
      (__ \ "readings").read[List[List[String]]]
    )(UnalignedFragment.apply _)

def read_data: List[UnalignedFragment] = {
  val wd = os.pwd
  println(wd)

  val datafile_path = os.pwd / "src" / "main" / "data" / "unaligned_data.json"

  val fileContents = os.read(datafile_path)
  val parsedJsValue = Json.parse(fileContents)
  val parsed = Json.fromJson[List[UnalignedFragment]](parsedJsValue)
  val darwin = parsed.get
  //  println(darwin)
  darwin
}
def vectorize_unaligned_fragment(node: UnalignedFragment): Unit = {
  // we have got to convert each reading into a bag of words
  // that is a map/transform operation
  // have to join tokens into a string again for this library to work
  // Then we vectorize each reading using the bag of words of that reading
  // and the terms of the combined keys of all the bags of words

  val list_bags_of_readings_joined = node.readings.map(reading => pimpString(reading.mkString(" ")).bag(filter = "", stemmer = None))
  println(list_bags_of_readings_joined)

  // calculate combined keys of bags.
  val keys = list_bags_of_readings_joined.map(bag => bag.keySet)
  println(keys)
  val terms = keys.reduce(_.union(_)).toList.sorted
  println(terms)
}
@main def unaligned_dev():Unit =
  val darwin: List[UnalignedFragment] = read_data

  darwin.filter(_.nodeno == 1146).foreach(vectorize_unaligned_fragment)



