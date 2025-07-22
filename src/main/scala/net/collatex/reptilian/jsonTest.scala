package net.collatex.reptilian

def witnessToJson(witness: WitnessJsonData): ujson.Value = witness match
  case WitnessJsonData.FromContent(data) =>
    ujson.Obj(
      "siglum" -> data.siglum,
      "color" -> data.color.map(ujson.Str(_)).getOrElse(ujson.Null),
      "content" -> data.content
    )
  case WitnessJsonData.FromTokens(siglum, tokens) =>
    ujson.Obj(
      "siglum" -> siglum,
      "tokens" -> ujson.Arr(
        tokens.map { token =>
          val base = ujson.Obj(
            "t" -> token.t,
            "n" -> token.n,
            "w" -> token.w,
            "g" -> token.g
          )
          // Add any unknown fields from 'other'
          token.other.foreach { case (k, v) =>
            base(k) = v
          }
          base
        }*
      )
    )

@main def stuff(): Unit =
  val result = retrieveWitnessDataJson(
    ujson.read(os.read(os.pwd / "src" / "main" / "data" / "manifest" / "sample-manifest.json")),
    ManifestData(
      ManifestSource.Local(os.pwd / "src" / "main" / "data" / "manifest" / "sample-manifest.json"),
      ManifestFormat.Json
    )
  )
  result match
    case Right(witnesses) =>
      val jsonArr = ujson.Arr(witnesses.map(witnessToJson)*)
      println(ujson.write(jsonArr, indent = 2))
    case Left(error) =>
      println(s"Error: $error")

