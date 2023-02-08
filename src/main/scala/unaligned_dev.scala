import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsArray, JsObject, Json, Reads, __}

case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

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
  // println(fileContents)
  val parsedJsValue = Json.parse(fileContents)
  val parsed = Json.fromJson[List[UnalignedFragment]](parsedJsValue)
  println(parsed)



//  for node in darwin: # Each unaligned zone is its own node
//    readings = node["readings"] # list of lists
//    current_linkage_object, current_cophenetic = create_linkage_object(readings)

