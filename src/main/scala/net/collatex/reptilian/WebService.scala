package net.collatex.reptilian

import cats.effect.{IO, IOApp}
import cats.syntax.all.catsSyntaxApplicativeId
import com.comcast.ip4s.{ipv4, port}
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

  // Define routes relative to web service
  // E.g., URL/api/hello/name regards URL/api as Root (see myRoutes, below)
  private val helloWorldService = HttpRoutes.of[IO] { case req @ GET -> Root / "hello" / name =>
    logger.info(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
      Ok(s"Hello, $name.")
  }
  private val goodbyeWorldService = HttpRoutes.of[IO] { case req @ GET -> Root / "goodbye" / name =>
    logger.info(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
      Ok(s"Goodbye, $name.")
  }

  private def defaultRoute(): HttpRoutes[IO] = HttpRoutes.of[IO] { request =>
    logger.error(s"404: ${request.method} ${request.uri} from ${request.remoteAddr.getOrElse("unknown")}") *>
      Response[IO](
        status = Status.NotFound,
        entity = Entity.stream(
          fs2.Stream.emits(s"404: The requested resource (${request.uri}) was not found!".getBytes).covary[IO]
        )
      ).pure[IO]
  }

  private val myRoutes = Router.define(
    "/" -> helloWorldService,
    "/api" -> helloWorldService,
    "/" -> goodbyeWorldService,
    "/api" -> goodbyeWorldService
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
        logger.debug("Debugging server routes") *>
        IO.never // Keeps the server running indefinitely by suspending IO
    }
}
