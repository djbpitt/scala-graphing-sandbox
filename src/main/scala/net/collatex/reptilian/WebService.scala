package net.collatex.reptilian

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxEither
import cats.syntax.all.catsSyntaxApplicativeId
import cats.syntax.traverse.toTraverseOps
import com.comcast.ip4s.{Port, ipv4}
import net.collatex.reptilian.GtaBuilder.{buildFromWitnessData, jsonToWitnessData}
import net.collatex.reptilian.display.DisplayFunctions.displayDispatch
import org.http4s.headers.`Content-Type`
import org.http4s.{DecodeResult, EntityDecoder, InvalidMessageBodyFailure, MediaType}
import org.http4s.{Entity, HttpRoutes, Response, Status}
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.multipart.*
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import org.http4s.server.staticcontent.resourceServiceBuilder
import org.typelevel.log4cats.{Logger, LoggerFactory}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}


object WebService extends IOApp {
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("reptilian")

  // Custom decoder for ujson.Value
  given jsonDecoder: EntityDecoder[IO, ujson.Value] = EntityDecoder[IO, String].flatMapR { str =>
    try {
      DecodeResult.success(IO.pure(ujson.read(str)))
    } catch {
      case e: Exception => DecodeResult.failure(IO.pure(InvalidMessageBodyFailure("Invalid JSON", Some(e))))
    }
  }

  // Define routes relative to web service
  private val helloWorldService = HttpRoutes.of[IO] { case req @ GET -> Root / "hello" / name =>
    for {
      _ <- Logger[IO].debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}")
      resp <- Ok(s"Hello, $name.")
    } yield resp
  }

  /*Responds to:
   * curl -X POST http://localhost:8082/multipart \
   *   -F 'formats=ribbon,svg' \
   *   -F 'manifest=@validManifest.json;type=application/json'
   *
   * Note that the manifest requires a name
   */

  // TODO: Currently just echoes input (all fields)
  private val multiPartService = HttpRoutes.of[IO] { case req @ POST -> Root / "multipart" =>
    for { // Needs Logger[IO].debug rather than logger.debug
      _ <- Logger[IO].debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}")
      m <- req.as[Multipart[IO]]
      lines <- m.parts.traverse { p => p.bodyText.compile.string.map(b => s"${p.name.getOrElse("<unnamed>")}: $b") }
      resp <- Ok(lines.mkString("\n"))
    } yield resp
  }

  // TODO: Apparently creates alignment table using regular display functions,
  // which write to stdout (or disk) as side effect without returning value,
  // so returns `()`
  private val reptilianService = HttpRoutes.of[IO] { case req @ POST -> Root =>
    val responseIO: IO[Response[IO]] = req
      .as[ujson.Value]
      .flatMap { json =>
        // val readableJson = json.render(indent = 2)
        val jsonString = json.toString
        val cfg = GtaBuilder.BuildConfig.Default
        val witnessData = jsonToWitnessData(jsonString, cfg).getOrElse(throw new RuntimeException("Oops"))
        val defaultColors = List("peru", "orange", "yellow", "green", "blue", "violet")
        val (gTa: Vector[TokenEnum], displaySigla: List[Siglum], colors: List[String], fonts: List[Option[String]]) =
          buildFromWitnessData(witnessData, defaultColors).getOrElse(throw new RuntimeException("Oops!"))
        val root = createAlignmentRibbon(gTa, Set(), false)
        val table: Unit = displayDispatch(root, gTa, displaySigla, colors, Map.empty)
        Ok(table.toString).map(_.withContentType(`Content-Type`(MediaType.application.json)))
      }
      .handleErrorWith { _ =>
        BadRequest("Invalid or missing JSON in request body")
      }

    responseIO.flatMap { response =>
      for {
        _ <- Logger[IO].debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}")
        _ <- Logger[IO].debug(s"Response status: ${response.status.code} ${response.status.reason}")
        resp <- IO.pure(response)
      } yield resp
    }
  }

  // Default route for 404
  private def defaultRoute(): HttpRoutes[IO] = HttpRoutes.of[IO] { request =>
    for {
      _ <- Logger[IO].debug(s"404: ${request.method} ${request.uri} from ${request.remoteAddr.getOrElse("unknown")}")
      resp <- Response[IO](
        status = Status.NotFound,
        entity = Entity.stream(
          fs2.Stream.emits(s"404: The requested resource (${request.uri}) was not found!".getBytes).covary[IO]
        )
      ).pure[IO]
    } yield resp
  }

  // Wrap resourceServiceBuilder in Resource.eval to get Resource[IO, HttpRoutes[IO]]
  // Contains index.xhtml main page
  private val staticService: Resource[IO, HttpRoutes[IO]] =
    Resource.eval(resourceServiceBuilder[IO]("/static").toRoutes)

  // Define myRoutes as Resource[IO, HttpRoutes[IO]]
  private val myRoutes: Resource[IO, HttpRoutes[IO]] = staticService.map { static =>
    Router.define(
      "/" -> reptilianService,
      "/" -> helloWorldService,
      "/" -> multiPartService,
      "/static" -> static
    )(defaultRoute())
  }

  // Parse port from args or environment, default to 8082 from config.yaml
  // Include source of final port (command line, environment, config.yaml) to aid in debugging
  private def getPort(args: List[String], defaultPort: Int): IO[(Int, String)] = IO {
    val portFromArgs = args
      .sliding(2)
      .collectFirst { case List("--port", portStr) => // collectFirst stops after first hit
        portStr.toIntOption
      }
      .flatten
      .filter(p => p >= 0 && p <= 65535) // Valid port numbers

    portFromArgs match {
      case Some(port) => (port, "command line")
      case None =>
        sys.env.get("REPTILIAN_PORT").flatMap(_.toIntOption) match {
          case Some(port) => (port, "environment (REPTILIAN_PORT)")
          case None       => (defaultPort, "config.yaml")
        }
    }
  }.handleErrorWith { e =>
    for {
      _ <- Logger[IO].error(s"Failed to parse port, using default $defaultPort from config.yaml: ${e.getMessage}")
      resp <- IO.pure((defaultPort, "config.yaml"))
    } yield resp

  }

  // Construct serverResource to produce HttpApp[IO]
  def run(args: List[String]): IO[ExitCode] = {
    val serverResource: Resource[IO, ExitCode] = for {
      config <- Resource.eval(
        IO.fromEither(loadResolvedConfig().leftMap(e => new RuntimeException(e)))
      )
      ResolvedConfig(tokensPerWitnessLimit, tokenPattern, defaultColors, defaultPort) = config
      portAndSource <- Resource.eval(getPort(args, defaultPort))
      (port, portSource) = portAndSource
      httpApp <- myRoutes.flatMap { routes =>
        Resource.eval(
          CORS.policy.withAllowOriginAll.withAllowMethodsAll.withAllowHeadersAll
            .httpRoutes(routes)
            .map(_.orNotFound)
        )
      }
      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(Port.fromInt(port).getOrElse(throw new RuntimeException("Oops!")))
        .withHttpApp(httpApp)
        .build
        .evalMap { server =>
          Logger[IO]
            .info(
              s"Server started at http://${server.address} using port from $portSource"
            )
            .as(ExitCode.Success)
        }
    } yield ExitCode.Success
    serverResource.use(_ => IO.never)
  }
}
