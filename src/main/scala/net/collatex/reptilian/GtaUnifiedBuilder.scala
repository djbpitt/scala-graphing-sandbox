package net.collatex.reptilian

private[reptilian] object GtaUnifiedBuilder

/** Entry point used by comparison tests. */
def build(md: ManifestData): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
  md match
    case ManifestData(source, ManifestFormat.Json) =>
      for
        json <- retrieveManifestJson(source)
        witnesses <- retrieveWitnessDataJson(json, md) // you already resolve root/witness font here
        out <- buildFromJsonWitnesses(witnesses)
      yield out

    case ManifestData(source, ManifestFormat.Xml) =>
      for
        xml <- retrieveManifestXml(source)
        witnesses <- retrieveWitnessDataXml(xml, md) // you already resolve root/witness font here
        out <- buildFromXmlWitnesses(witnesses)
      yield out

// Implement JSON path first; wire XML later
private def buildFromJsonWitnesses(
    wits: Seq[WitnessJsonData]
): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
  Right(???)

private def buildFromXmlWitnesses(
    wits: Seq[CollateXWitnessData]
): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] =
  Right(???)
