package com.bot.cbr.main

import cats.Parallel
import cats.effect._
import com.bot.cbr.config.Config
import com.bot.cbr.service._
import doobie.util.ExecutionContexts
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder

object CBRbotApp extends IOApp {

  val poolSize = 5

  def runBot[F[_]: ConcurrentEffect: ContextShift: Parallel](): Resource[F, CBRbot[F]] =
    for {
      serverEc <- ExecutionContexts.cachedThreadPool[F]
      connEc <- ExecutionContexts.fixedThreadPool[F](poolSize)
      blocker = Blocker.liftExecutionContext(connEc)
      client <- BlazeClientBuilder[F](serverEc).resource
      logger <- Resource.liftF(Slf4jLogger.create)
      config <- Resource.liftF(Config.load(blocker))
      botService = new BotServiceImpl[F](config, client, logger)
      curService = new CurrencyServiceImpl[F](config, client, logger)
      metalParser = new MetalParserImpl[F, Throwable](identity)
      metalService = new MetalServiceImpl[F](config, client, metalParser, logger)
      moexCurrencyService = new MoexCurrencyServiceImpl[F](config, client, logger)
      bot = new CBRbot(botService, curService, metalService, moexCurrencyService, logger)
    } yield bot


  override def run(args: List[String]): IO[ExitCode] =
    runBot[IO]().use(_.launch.compile.lastOrError).as(ExitCode.Success)
}
