import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsArray, JsObject, Json, Reads, __}

import scala.annotation.unused

case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

// object is used, but somehow IDE doesn't detect it's usage.
@unused
object UnalignedFragment {
  implicit val reads: Reads[UnalignedFragment] = (
    (__ \ "nodeno").read[Int] and
      (__ \ "readings").read[List[List[String]]]
    )(UnalignedFragment.apply _)
}
@main def unaligned_dev():Unit =
  val wd = os.pwd
  println(wd)

  val datafile_path = os.pwd/"src"/"main"/"data"/"unaligned_data.json"

  val fileContents = os.read(datafile_path)
  val parsedJsValue = Json.parse(fileContents)
  val parsed = Json.fromJson[List[UnalignedFragment]](parsedJsValue)
  val darwin = parsed.get
//  println(darwin)

  for node <- darwin
    if node.nodeno == 1146 do
      println(node.readings.head)
      // have to join tokens into a string again for this library to work
      val tokens = node.readings.head
      val joined = tokens.mkString(" ")

      val bagOfWords = smile.nlp.pimpString(joined).bag(filter="", stemmer = None)
      println(bagOfWords)




