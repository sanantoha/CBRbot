package com.bot.cbr.main

import java.time.LocalDate

import cats.effect._
import com.bot.cbr.domain.{CBRError, Currency}
import scalacache._
import scalacache.caffeine._
import cats.syntax.functor._
import cats.syntax.flatMap._
import scalacache.CatsEffect.modes._
import cats.syntax.option._
import cats.instances.option._

import scala.concurrent.duration._
import cats.syntax.show._
import Currency._
import cats.data.EitherNec
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import com.bot.cbr.cache.CurrencyCache._
import cats.syntax.either._
import cats.instances.vector._


object ScalaCacheDemo extends IOApp {

  def runCache[F[_]: cats.effect.Async: Timer](): F[Option[Vector[EitherNec[CBRError, Currency]]]] = for {
    logger <- Slf4jLogger.create
    _ <- put[F, Vector[EitherNec[CBRError, Currency]]]("usd", LocalDate.now)(
      Vector(Currency("usd", BigDecimal(1), BigDecimal(66), 85, "USD").rightNec),
      ttl = 2.seconds.some)

    cur <- get[F, Vector[EitherNec[CBRError, Currency]]]("usd", LocalDate.now)
    _ <- logger.info(s"first cur invoke: ${cur.mkString(",")}")
    _ <- Timer[F].sleep(3.seconds)
    cur1 <- get[F, Vector[EitherNec[CBRError, Currency]]]("usd", LocalDate.now)
    _ <- logger.info(s"second cur invoke: ${cur1.mkString(",")}")
    cur2 <- caching[F, Vector[EitherNec[CBRError, Currency]]]("eur", LocalDate.now)(None)(
      Vector(Currency("eur", BigDecimal(1), BigDecimal(76), 95, "EUR").rightNec)
    )
    _ <- logger.info(s"third cur invoke: ${cur2.mkString(",")}")
  } yield cur2.some

  override def run(args: List[String]): IO[ExitCode] = {

    runCache[IO]() map println as ExitCode.Success
  }
}
