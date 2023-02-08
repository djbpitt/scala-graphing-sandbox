import play.api.libs.json.Json

import scala.io.Source
import scala.util.Using

// import scala.util.parsing.json._
@main def unaligned_dev():Unit =
  val wd = os.pwd
  println(wd)

  val datafile_path = (os.pwd/"src"/"main"/"data"/"unaligned_data.json").toString

  val fileContents = Using(Source.fromFile(datafile_path)) { source => source.mkString }

  println(fileContents)

//  val source: String = Source.fromFile("app/assets/jsons/countriesToCities.json").map(Json.parse)
//  val json: JsValue = Json.parse(source)
//
//
//
//  val result = JSON.parseFull(jsonStr)
//  result match {
//
//    // Matches if jsonStr is valid JSON and represents a Map of Strings to Any
//    case Some(map: Map[String, Any]) => println(map)
//    case None => println("Parsing failed")
//    case other => println("Unknown data structure: " + other)
//  }

//with open("unaligned_data.json", "r") as f:
//  darwin = json.load(f)
//
//  for node in darwin: # Each unaligned zone is its own node
//    readings = node["readings"] # list of lists
//    current_linkage_object, current_cophenetic = create_linkage_object(readings)

