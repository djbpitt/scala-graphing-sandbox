package net.collatex.reptilian

import cats.effect.{IO, IOApp}
import cats.syntax.all.catsSyntaxApplicativeId
import com.comcast.ip4s.{ipv4, port}
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
// `extends IOApp.Simple` creates a Cats Effect runtime (don't use `@main`)
// Because IOApp.Simple automatically calls its run method (`run: IO[Unit]`)
// we define the method without having to call it explicitly
object WebService extends IOApp.Simple {
  given loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

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

  // Combine routes
  private val httpApp: HttpApp[IO] = HttpApp[IO]({ request =>
    myRoutes.run(request).value.map(_.get)
  })

  // Server setup
  val run: IO[Unit] = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8082")
    .withHttpApp(httpApp)
    .build // at this point creates `Resource[IO, Server], which is used immediately
    .use { server =>
      logger.info(s"Server started at http://${server.address}") *>
        IO.never // Keeps the server running indefinitely by suspending IO
    }
}
