package net.collatex.reptilian

import org.virtuslab.yaml.*

import scala.util.Using
import scala.util.matching.Regex

// For config.yaml
case class Config(
    tokensPerWitnessLimit: Option[Int] = None,
    tokenPattern: String,
    defaultColors: List[String],
    defaultPort: Int
) derives YamlCodec
case class ResolvedConfig(
    tokensPerWitnessLimit: Int,
    tokenPattern: Regex,
    defaultColors: List[String],
    defaultPort: Int
)
object ResolvedConfig {
  def from(c: Config): ResolvedConfig = ResolvedConfig(
    tokensPerWitnessLimit = c.tokensPerWitnessLimit.getOrElse(Int.MaxValue),
    tokenPattern = Regex(c.tokenPattern),
    defaultColors = c.defaultColors,
    defaultPort = c.defaultPort
  )
}
def loadResolvedConfig(resourceName: String = "config.yaml"): Either[String, ResolvedConfig] =
  // 1) safely read resource (wrap IOException into Left)
  val read: Either[String, String] =
    try Right(Using.resource(io.Source.fromResource(resourceName))(_.mkString))
    catch case e: Throwable => Left(s"Cannot read $resourceName: ${e.getMessage}")

  // 2) decode + resolve in a single for-comprehension
  for
    yaml <- read
    config <- yaml.as[Config].left.map(_.toString)
  yield ResolvedConfig.from(config)
