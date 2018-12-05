package com.bot.cbr.main

import cats.{ApplicativeError, Traverse}
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.effect._
import com.bot.cbr.config.Config
import com.bot.cbr.service._
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import cats.syntax.functor._
import com.bot.cbr.domain.CBRError
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder

object CBRbotApp extends IOApp {

  def runBot[F[_]: ConcurrentEffect: ContextShift,
             G[_]: Traverse: ApplicativeError[?[_], NonEmptyChain[CBRError]]]: F[ExitCode] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val res = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          config <- Stream.eval(Config.load)
          botService = new BotServiceImpl[F](config, client, logger)
          curService = new CurrencyServiceImpl[F](config, client, logger)
          metalParser = new MetalParserImpl[G, NonEmptyChain[CBRError]](NonEmptyChain.one)
          metalService = new MetalServiceImpl[F, G](config, client, metalParser, logger)
          bot = new CBRbot(botService, curService, metalService, logger)
          _ <- bot.launch
        } yield ExitCode.Success

        res.compile.lastOrError
    }

  override def run(args: List[String]): IO[ExitCode] = runBot[IO, ValidatedNec[CBRError, ?]]
}
