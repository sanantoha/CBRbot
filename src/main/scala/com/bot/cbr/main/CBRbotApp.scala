package com.bot.cbr.main

import cats.effect._
import com.bot.cbr.config.Config
import com.bot.cbr.service.{BotServiceImpl, CBRbot, CurrencyServiceImpl}
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import cats.syntax.functor._
import cats.syntax.flatMap._

object CBRbotApp extends IOApp {

  def runBot[F[_]: ConcurrentEffect: ContextShift]: F[ExitCode] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        for {
          logger <- Slf4jLogger.create
          config <- Config.load
          botService = new BotServiceImpl[F](config, logger)
          curService = new CurrencyServiceImpl[F](config, logger)
          bot = new CBRbot(botService, curService, logger)
          _ <- bot.launch.compile.drain
        } yield ExitCode.Success
    }

  override def run(args: List[String]): IO[ExitCode] = runBot[IO]
}
