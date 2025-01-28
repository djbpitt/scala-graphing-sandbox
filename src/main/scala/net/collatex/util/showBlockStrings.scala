package net.collatex.util

import net.collatex.reptilian.{FullDepthBlock, Siglum, TokenEnum, TokenRange, makeTokenizer, readData, tokenize}

import scala.util.matching.Regex

val sampleBlocks = // default (sample) blocks
  List(
    FullDepthBlock(Vector(70893, 56567, 42406),1),
    FullDepthBlock(Vector(42483, 70967),4),
    FullDepthBlock(Vector(70855, 42367),36),
    FullDepthBlock(Vector(70947, 42461),20),
    FullDepthBlock(Vector(70936, 42448),9),
    FullDepthBlock(Vector(56566, 42405),2),
    FullDepthBlock(Vector(70945, 42458),2),
    FullDepthBlock(Vector(70922, 42433),7),
    FullDepthBlock(Vector(42491, 70977),5),
    FullDepthBlock(Vector(70896, 42407),25),
    FullDepthBlock(Vector(70929, 42441),6),
    FullDepthBlock(Vector(42481, 56641),2)
  )

val sampleGTa = // default (sample) gTa
  val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin"
  val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = makeTokenizer(
    tokenPattern
  ) // Tokenizer function with user-supplied regex
  val witnessInputInfo: List[(String, String)] = readData(
    pathToDarwin
  ) // One string per witness
  val witnessStrings: List[String] = witnessInputInfo.map(_._2)
  val sigla: List[Siglum] = witnessInputInfo.map(_._1).map(Siglum(_))
  val gTa: Vector[TokenEnum] = tokenize(tokenizer)(witnessStrings) // global token array
  gTa

def showBlocks(blocks: List[FullDepthBlock], gTa: Vector[TokenEnum]): Unit =
  val blockTokenRanges: List[Vector[TokenRange]] = blocks
    .map(block => block
      .instances
      .map(instance => TokenRange(instance, instance + block.length, gTa))
    )
  val result = blockTokenRanges.map(
      btr => btr.map(tr => tr.tString)
    )
  result.foreach(println)

@main
def runWithSampleData(): Unit = showBlocks(sampleBlocks, sampleGTa)
