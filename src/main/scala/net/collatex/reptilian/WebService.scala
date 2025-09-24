package net.collatex.reptilian

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.catsSyntaxApplicativeId
import com.comcast.ip4s.{Port, ipv4, port}
import org.http4s.headers.`Content-Type`
import org.http4s.{DecodeResult, EntityDecoder, InvalidMessageBodyFailure, MediaType}
// import org.http4s.FormDataDecoder.formEntityDecoder
import org.http4s.{Entity, HttpApp, HttpRoutes, Response, Status}
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.typelevel.log4cats.{Logger, LoggerFactory}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}

//noinspection IllegalOptionGet
// `extends IOApp` creates a Cats Effect runtime (don't use `@main`); we use
//   `IOApp` instead of `IOApp` simple because we need command-line arguments
// Because IOApp automatically calls its run method (`run: IO[Unit]`)
//   we define the method without having to call it explicitly
object WebService extends IOApp {
  given loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  // Define default port as a constant
  private val DefaultPort = 8082

  // Custom decoder for ujson.Value
  // Explicit `using` clause not needed because http4s `as` method is
  // defined as `def as[T](implicit ev: EntityDecoder[F, T]): F[T]`, which
  // subsumes `using` behavior
  given jsonDecoder: EntityDecoder[IO, ujson.Value] = EntityDecoder[IO, String].flatMapR { str =>
    try {
      DecodeResult.success(IO.pure(ujson.read(str)))
    } catch {
      case e: Exception => DecodeResult.failure(IO.pure(InvalidMessageBodyFailure("Invalid JSON", Some(e))))
    }
  }

  // Define routes relative to web service
  // E.g., URL/api/hello/name regards URL/api as Root (see myRoutes, below)
  private val helloWorldService = HttpRoutes.of[IO] { case req @ GET -> Root / "hello" / name =>
    logger.debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
      Ok(s"Hello, $name.")
  }

  // TODO: Currently just echoes JSON input
  private val reptilianService = HttpRoutes.of[IO] { case req @ POST -> Root =>
    // Process the request and get the response
    val responseIO: IO[Response[IO]] = req
      .as[ujson.Value]
      .flatMap { json =>
        val readableJson = json.render(indent = 2)
        Ok(readableJson).map(_.withContentType(`Content-Type`(MediaType.application.json)))
      }
      .handleErrorWith { _ =>
        BadRequest("Invalid or missing JSON in request body")
      }

    // Log the status after computing the response
    responseIO.flatMap { response =>
      logger.debug(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
        logger.debug(s"Response status: ${response.status.code} ${response.status.reason}") *>
        IO.pure(response)
    }
  }

  private def defaultRoute(): HttpRoutes[IO] = HttpRoutes.of[IO] { request =>
    logger.debug(s"404: ${request.method} ${request.uri} from ${request.remoteAddr.getOrElse("unknown")}") *>
      Response[IO](
        status = Status.NotFound,
        entity = Entity.stream(
          fs2.Stream.emits(s"404: The requested resource (${request.uri}) was not found!".getBytes).covary[IO]
        )
      ).pure[IO]
  }

  private val myRoutes = Router.define(
    "/" -> reptilianService,
    "/" -> helloWorldService
  )(defaultRoute())

  // Parse port from args or environment, default to 8082
  private def getPort(args: List[String]): IO[Int] = IO {
    // Check command-line args first (e.g., "--port 8083")
    // collectFirst stops after first match
    val portFromArgs = args
      .sliding(2)
      .collectFirst { case List("--port", portStr) =>
        portStr.toIntOption
      }
      .flatten
      .filter(p => p >= 0 && p <= 65535) // Validate port range
    // Check environment variable "REPTILIAN_PORT" if args don't provide a valid port
    portFromArgs.orElse(sys.env.get("REPTILIAN_PORT").flatMap(_.toIntOption)).getOrElse(DefaultPort)
  }.handleErrorWith { e =>
    logger.error(s"Failed to parse port, using default 8082: ${e.getMessage}")
    IO.pure(DefaultPort)
  }

  // Combine routes
  private val httpApp: HttpApp[IO] = HttpApp[IO]({ request =>
    myRoutes.run(request).value.map(_.get)
  })

  // Server setup
  def run(args: List[String]): IO[ExitCode] = for {
    port <- getPort(args)
    server <- EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0") // listens on *all* network addresses (EmberServerBuilder)
      .withPort(Port.fromInt(port).get) // Safe because port is validated above
      .withHttpApp(httpApp)
      .build
      .use { server =>
        logger.info(s"Server started at http://${server.address}") *>
          IO.never // Keeps the server running indefinitely
      }
  } yield ExitCode.Success
}
