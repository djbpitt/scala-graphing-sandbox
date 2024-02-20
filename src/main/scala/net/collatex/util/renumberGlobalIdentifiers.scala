package net.collatex.util

val gPattern = """"g": 0""".r.unanchored
@main def renumber(): Unit =
  val datafilePath =
    os.pwd / "src" / "main" / "data" / "unaligned_data_node_296_tokenized.json"
  val fileContents = os.read(datafilePath).split("\n")
  var counter = 0
  val results = fileContents.map(e => {
    counter += 1
    if gPattern.matches(e) then s"          \"g\": $counter\n"
    else e + "\n"
  })
  println(results.mkString)




