package com.bot.cbr.main

import cats.effect._
import cats.temp.par._
import com.bot.cbr.config.Config
import com.bot.cbr.service._
import fs2.Stream
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder
import cats.syntax.functor._

object CBRbotApp extends IOApp {

  def runBot[F[_]: ConcurrentEffect: ContextShift: Par](): F[ExitCode] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val res = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          config <- Stream.eval(Config.load)
          botService = new BotServiceImpl[F](config, client, logger)
          curService = new CurrencyServiceImpl[F](config, client, logger)
          metalParser = new MetalParserImpl[F, Throwable](identity)
          metalService = new MetalServiceImpl[F](config, client, metalParser, logger)
          moexCurrencyService = new MoexCurrencyServiceImpl[F](config, client, logger)
          bot = new CBRbot(botService, curService, metalService, moexCurrencyService, logger)
          _ <- bot.launch
        } yield ExitCode.Success

        res.compile.lastOrError
    }

  override def run(args: List[String]): IO[ExitCode] = runBot[IO]()
}
