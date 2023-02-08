import play.api.libs.json.{JsArray, JsObject, Json}

@main def unaligned_dev():Unit =
  val wd = os.pwd
  println(wd)

  val datafile_path = os.pwd/"src"/"main"/"data"/"unaligned_data.json"

  val fileContents = os.read(datafile_path)
  // println(fileContents)
  val json = Json.parse(fileContents)
  // println(json)

  json match
    case JsArray(arr) =>
      for node <- arr do
        node match
          case JsObject(obj) =>
            val readings = obj("readings")
            println(readings)


//  for node in darwin: # Each unaligned zone is its own node
//    readings = node["readings"] # list of lists
//    current_linkage_object, current_cophenetic = create_linkage_object(readings)

