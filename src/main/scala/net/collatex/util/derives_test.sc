import upickle.default.ReadWriter as Codec
import upickle.default.{write as writeJson, read as readJson}

case class Pet(
                name: String,
                kind: String
              ) derives Codec // enable coding Pet to and from text

val coco = Pet(name = "Coco", kind = "Cat")

val message = writeJson(coco)

// display the message
message

// convert message back to a Pet!
readJson[Pet](message)