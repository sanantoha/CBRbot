package com.bot.cbr.service

import java.time.{LocalDate, LocalDateTime}
import java.util.concurrent.Executors

import cats.data.EitherNec
import cats.effect.{Blocker, ConcurrentEffect, ContextShift, IO, Resource}
import com.bot.cbr.{ReadData, UnitSpec}
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.{CBRError, MoexIndicCurrency}
import com.bot.cbr.utils.mkClient
import fs2.Stream
import io.chrisdavenport.log4cats.noop.NoOpLogger
import cats.syntax.either._
import doobie.util.ExecutionContexts

import scala.concurrent.ExecutionContext

class MoexIndicCurServiceSpec extends UnitSpec {

  val poolSize = 3

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
    val currencies = for {
      client <- Stream.emit(mkClient[F](response)).covary[F]
      logger <- Stream.emit(NoOpLogger.impl[F]).covary[F]
      config = Config("url", "url", "url", "url", MoexCurrencyUrlConfig("url", "url"))
      moexIdicCurService = new MoexIndicCurServiceImpl[F](config, client, logger)
      res <- moexIdicCurService.getCurrencies("EUR/RUB", LocalDate.now)
    } yield res
    currencies.compile.toVector
  }

  def runTest[F[_]: ConcurrentEffect: ContextShift](): F[Vector[EitherNec[CBRError, MoexIndicCurrency]]] = {
    val resource = for {
        connEc <- ExecutionContexts.fixedThreadPool[F](poolSize)
        blocker = Blocker.liftExecutionContext(connEc)
        response <- Resource.liftF(new ReadData[F]("src/test/resources/moex_indic_cur_data.xml", blocker).apply())
    } yield response

    resource.use(runMoexIndicCurService[F])
  }

}
