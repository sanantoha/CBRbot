package com.bot.cbr.service

import java.time.LocalDate
import java.util.concurrent.Executors

import cats.effect.{ConcurrentEffect, ContextShift, IO}
import com.bot.cbr.{ReadData, UnitSpec}
import com.bot.cbr.utils._
import com.bot.cbr.config.Config
import com.bot.cbr.domain.{CBRError, Currency}
import fs2.Stream
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.{Executors => E}
import io.chrisdavenport.log4cats.noop.NoOpLogger
import cats.syntax.functor._
import cats.syntax.flatMap._


import scala.concurrent.ExecutionContext


class CurrencyServiceSpec extends UnitSpec {

  val expCurrencies = Vector(
    Right(Currency("Австралийский доллар", 1, 48.4192, 36, "AUD")),
    Right(Currency("Азербайджанский манат", 1, 39.4045, 944, "AZN")),
    Right(Currency("Фунт стерлингов Соединенного королевства", 1, 87.0316, 826, "GBP")),
    Right(Currency("Армянский драм", 100, 13.7268, 51, "AMD")),
    Right(Currency("Белорусский рубль", 1, 31.3701, 933, "BYN")),
    Right(Currency("Болгарский лев", 1, 38.7579, 975, "BGN")),
    Right(Currency("Бразильский реал", 1, 17.7768, 986, "BRL")),
    Right(Currency("Венгерский форинт", 100, 23.5885, 348, "HUF")),
    Right(Currency("Гонконгский доллар", 10, 85.3851, 344, "HKD")),
    Right(Currency("Датская крона", 1, 10.1635, 208, "DKK")),
    Right(Currency("Доллар США", 1, 66.8497, 840, "USD")),
    Right(Currency("Евро", 1, 75.8076, 978, "EUR")),
    Right(Currency("Индийская рупия", 100, 91.9623, 356, "INR")),
    Right(Currency("Казахстанский тенге", 100, 17.9544, 398, "KZT")),
    Right(Currency("Канадский доллар", 1, 50.6783, 124, "CAD")),
    Right(Currency("Киргизский сом", 100, 95.9588, 417, "KGS")),
    Right(Currency("Китайский юань", 10, 96.2642, 156, "CNY")),
    Right(Currency("Молдавский лей", 10, 39.2195, 498, "MDL")),
    Right(Currency("Норвежская крона", 10, 79.3327, 578, "NOK")),
    Right(Currency("Польский злотый", 1, 17.6692, 985, "PLN")),
    Right(Currency("Румынский лей", 1, 16.2683, 946, "RON")),
    Right(Currency("СДР (специальные права заимствования)", 1, 92.7881, 960, "XDR")),
    Right(Currency("Сингапурский доллар", 1, 48.5192, 702, "SGD")),
    Right(Currency("Таджикский сомони", 10, 70.9266, 972, "TJS")),
    Right(Currency("Турецкая лира", 1, 12.0949, 949, "TRY")),
    Right(Currency("Новый туркменский манат", 1, 19.1272, 934, "TMT")),
    Right(Currency("Узбекский сум", 10000, 81.0304, 860, "UZS")),
    Right(Currency("Украинская гривна", 10, 23.9390, 980, "UAH")),
    Right(Currency("Чешская крона", 10, 29.2431, 203, "CZK")),
    Right(Currency("Шведская крона", 10, 73.7570, 752, "SEK")),
    Right(Currency("Швейцарский франк", 1, 66.3323, 756, "CHF")),
    Right(Currency("Южноафриканский рэнд", 10, 46.8073, 710, "ZAR")),
    Right(Currency("Вон Республики Корея", 1000, 59.2259, 410, "KRW")),
    Right(Currency("Японская иена", 100, 58.7380, 392, "JPY"))
  )

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)


  "CurrencyService" should "return parsed currency by service url" in {

    runTest[IO]().unsafeRunSync() shouldBe expCurrencies
  }

  def runTest[F[_]: ConcurrentEffect: ContextShift](): F[Vector[Either[CBRError, Currency]]] = {
    E.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        for {
          response <- new ReadData[F]("src/test/resources/currency_data.xml").apply()
          res <- runCurrencyService[F](response)
        } yield res
    }
  }

  def runCurrencyService[F[_] : ConcurrentEffect](response: String): F[Vector[Either[CBRError, Currency]]] = {
    val currencies = for {
      client <- Stream.emit(mkClient(response)).covary[F]
      logger <- Stream.emit(NoOpLogger.impl[F]).covary[F]
      config = Config("token", "url", "url", "url")
      service = new CurrencyServiceImpl[F](config, client, logger)
      res <- service.getCurrencies(LocalDate.now())
    } yield res
    currencies.compile.toVector
  }
}