package com.bot.cbr.service

import java.time.{LocalDate => LD}
import java.util.concurrent.Executors

import cats.data.EitherNec
import cats.effect.{Blocker, ConcurrentEffect, ContextShift, IO, Resource, Sync}
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.{CBRError, MoexCurrency, MoexCurrencyType}
import com.bot.cbr.domain.MoexCurrencyType._
import com.bot.cbr.utils.mkClient
import com.bot.cbr.{ReadData, UnitSpec}
import fs2.Stream
import io.chrisdavenport.log4cats.noop.NoOpLogger
import cats.syntax.either._
import doobie.util.ExecutionContexts

import scala.concurrent.ExecutionContext

class MoexCurrencyServiceSpec extends UnitSpec {

  val poolSize = 3

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  val expMoexUSDCurs = Vector(
    MoexCurrency(USD, LD.of(2019, 1, 15), BigDecimal(66.9875), BigDecimal(-0.0875)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 14), BigDecimal(67.075), BigDecimal(0.2)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 11), BigDecimal(66.875), BigDecimal(-0.035)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 10), BigDecimal(66.91), BigDecimal(0.18)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 9), BigDecimal(66.73), BigDecimal(-0.155)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 8), BigDecimal(66.885), BigDecimal(-0.855)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 4), BigDecimal(67.74), BigDecimal(-0.9)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2019, 1, 3), BigDecimal(68.64), BigDecimal(-1.1775)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2018, 12, 29), BigDecimal(69.8175), BigDecimal(0.3)).rightNec[CBRError],
    MoexCurrency(USD, LD.of(2018, 12, 28), BigDecimal(69.5175), BigDecimal(-0.0425)).rightNec[CBRError]
  )

  val expMoexEURCurs = Vector(
    MoexCurrency(EUR, LD.of(2019, 1, 16), BigDecimal(76.2225), BigDecimal(-0.1825)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 15), BigDecimal(76.405), BigDecimal(-0.4975)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 14), BigDecimal(76.9025), BigDecimal(0.2025)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 11), BigDecimal(76.7), BigDecimal(-0.205)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 10), BigDecimal(76.905), BigDecimal(-0.1925)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 9), BigDecimal(77.0975), BigDecimal(0.5475)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 8), BigDecimal(76.55), BigDecimal(-0.7475)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 4), BigDecimal(77.2975), BigDecimal(-0.955)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2019, 1, 3), BigDecimal(78.2525), BigDecimal(-1.7375)).rightNec[CBRError],
    MoexCurrency(EUR, LD.of(2018, 12, 29), BigDecimal(79.99), BigDecimal(0.32)).rightNec[CBRError]
  )

  "MoexCurrencyService" should "return USD currency" in {
    runTest[IO](USD).unsafeRunSync() shouldBe expMoexUSDCurs
  }

  it should "return EUR currency" in {
    runTest[IO](EUR).unsafeRunSync() shouldBe expMoexEURCurs
  }

  def runMoexCurrencyService[F[_] : Sync](response: String, moexCurrencyType: MoexCurrencyType): F[Vector[EitherNec[CBRError, MoexCurrency]]] = {
    val metals = for {
      client <- Stream.emit(mkClient[F](response)).covary[F]
      logger <- Stream.emit(NoOpLogger.impl[F]).covary[F]
      config = Config("url", "url", "url", "url", MoexCurrencyUrlConfig("url", "url"))
      moexCurrency = new MoexCurrencyServiceImpl[F](config, client, logger)
      res <- moexCurrency.getCurrencies(moexCurrencyType)
    } yield res
    metals.compile.toVector
  }


  def runTest[F[_] : ConcurrentEffect : ContextShift](moexCurrencyType: MoexCurrencyType): F[Vector[EitherNec[CBRError, MoexCurrency]]] = {
    val resource = for {
      connEc <- ExecutionContexts.fixedThreadPool[F](poolSize)
      dataFile = moexCurrencyType match {
        case USD => "usd_data.xml"
        case EUR => "eur_data.xml"
      }
      blocker = Blocker.liftExecutionContext(connEc)
      data <- Resource.liftF(new ReadData[F](s"src/test/resources/$dataFile", blocker).apply())
    } yield data

    resource.use(runMoexCurrencyService[F](_, moexCurrencyType))
  }
}
