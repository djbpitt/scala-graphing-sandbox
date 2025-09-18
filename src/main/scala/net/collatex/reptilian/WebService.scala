package net.collatex.reptilian

import cats.effect.{IO, IOApp}
import com.comcast.ip4s.{ipv4, port}
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.typelevel.log4cats.{Logger, LoggerFactory}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}

// `extends IOApp.Simple` creates a Cats Effect runtime (don't use `@main`)
// Because IOApp.Simple automatically calls its run method (`run: IO[Unit]`)
// we define the method without having to call it explicitly
object WebService extends IOApp.Simple {
  given loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  // Define routes
  private val helloWorldService = HttpRoutes.of[IO] { case req @ GET -> Root / "hello" / name =>
    logger.info(s"Request: ${req.method} ${req.uri} from ${req.remoteAddr.getOrElse("unknown")}") *>
      Ok(s"Hello, $name.")
  }

  // Combine routes
  private val httpApp = Router(
    "/" -> helloWorldService,
    "/api" -> helloWorldService // Note: Using same service for both; adjust if needed
  ).orNotFound

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
