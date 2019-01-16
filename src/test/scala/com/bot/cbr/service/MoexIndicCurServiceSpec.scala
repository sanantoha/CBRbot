package com.bot.cbr.service

import java.time.{LocalDate, LocalDateTime}
import java.util.concurrent.Executors

import cats.data.EitherNec
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import com.bot.cbr.{ReadData, UnitSpec}
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.{CBRError, MoexIndicCurrency}
import com.bot.cbr.utils.mkClient
import fs2.Stream
import cats.syntax.functor._
import cats.syntax.flatMap._
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.log4cats.noop.NoOpLogger
import io.chrisdavenport.linebacker.contexts.{Executors => E}
import cats.syntax.either._

import scala.concurrent.ExecutionContext

class MoexIndicCurServiceSpec extends UnitSpec {

  val expIndicCur = Vector(
    MoexIndicCurrency("EUR/RUB", LocalDateTime.of(2019, 1, 11, 18, 30, 0), BigDecimal(76.9564)).rightNec[CBRError],
    MoexIndicCurrency("EUR/RUB", LocalDateTime.of(2019, 1, 11, 13, 45, 0), BigDecimal(77.1318)).rightNec[CBRError],
    MoexIndicCurrency("EUR/RUB", LocalDateTime.of(2019, 1, 10, 18, 30, 0), BigDecimal(77.3575)).rightNec[CBRError],
    MoexIndicCurrency("EUR/RUB", LocalDateTime.of(2019, 1, 10, 13, 45, 0), BigDecimal(77.2179)).rightNec[CBRError]
  )

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  "MoexInicCurService" should "return indicative currency for particular date" in {
    runTest[IO]().unsafeRunSync() shouldBe expIndicCur
  }

  def runMoexIndicCurService[F[_]: ConcurrentEffect](response: String): F[Vector[EitherNec[CBRError, MoexIndicCurrency]]] = {
    val metals = for {
      client <- Stream.emit(mkClient[F](response)).covary[F]
      logger <- Stream.emit(NoOpLogger.impl[F]).covary[F]
      config = Config("url", "url", "url", "url", MoexCurrencyUrlConfig("url", "url"))
      moexIdicCurService = new MoexIndicCurServiceImpl[F](config, client, logger)
      res <- moexIdicCurService.getCurrencies("EUR/RUB", LocalDate.now)
    } yield res
    metals.compile.toVector
  }

  def runTest[F[_]: ConcurrentEffect: ContextShift](): F[Vector[EitherNec[CBRError, MoexIndicCurrency]]] = {
    E.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        for {
          response <- new ReadData[F]("src/test/resources/moex_indic_cur_data.xml").apply()
          res <- runMoexIndicCurService[F](response)
        } yield res
    }
  }

}
