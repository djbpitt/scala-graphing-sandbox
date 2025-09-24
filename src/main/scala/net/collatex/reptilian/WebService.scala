package net.collatex.reptilian

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxEither
import cats.syntax.all.catsSyntaxApplicativeId
import com.comcast.ip4s.{Port, ipv4}
import org.http4s.headers.`Content-Type`
import org.http4s.{DecodeResult, EntityDecoder, InvalidMessageBodyFailure, MediaType}
import org.http4s.{Entity, HttpRoutes, Response, Status}
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import org.http4s.server.staticcontent.resourceServiceBuilder
import org.typelevel.log4cats.{Logger, LoggerFactory}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}

object WebService extends IOApp {
  given loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

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
    logger.debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
      Ok(s"Hello, $name.")
  }

  // TODO: Currently just echoes JSON input
  private val reptilianService = HttpRoutes.of[IO] { case req @ POST -> Root =>
    val responseIO: IO[Response[IO]] = req
      .as[ujson.Value]
      .flatMap { json =>
        val readableJson = json.render(indent = 2)
        Ok(readableJson).map(_.withContentType(`Content-Type`(MediaType.application.json)))
      }
      .handleErrorWith { _ =>
        BadRequest("Invalid or missing JSON in request body")
      }

    responseIO.flatMap { response =>
      logger.debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
        logger.debug(s"Response status: ${response.status.code} ${response.status.reason}") *>
        IO.pure(response)
    }
  }

  // Default route for 404
  private def defaultRoute(): HttpRoutes[IO] = HttpRoutes.of[IO] { request =>
    logger.debug(s"404: ${request.method} ${request.uri} from ${request.remoteAddr.getOrElse("unknown")}") *>
      Response[IO](
        status = Status.NotFound,
        entity = Entity.stream(
          fs2.Stream.emits(s"404: The requested resource (${request.uri}) was not found!".getBytes).covary[IO]
        )
      ).pure[IO]
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
    logger.error(s"Failed to parse port, using default $defaultPort from config.yaml: ${e.getMessage}") *>
      IO.pure((defaultPort, "config.yaml"))
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
        .withPort(Port.fromInt(port).get)
        .withHttpApp(httpApp)
        .build
        .evalMap { server =>
          logger
            .info(
              s"Server started at http://${server.address} using port from $portSource"
            )
            .as(ExitCode.Success)
        }
    } yield ExitCode.Success

    serverResource.use(_ => IO.never)
  }
}
