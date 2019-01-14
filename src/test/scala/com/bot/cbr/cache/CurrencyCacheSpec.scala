package com.bot.cbr.cache

import java.time.LocalDate
import java.util.concurrent.Executors

import com.bot.cbr.UnitSpec
import CurrencyCache._
import cats.data.EitherNec
import cats.effect.{ContextShift, IO}
import scalacache._
import com.bot.cbr.domain.{CBRError, Currency}
import cats.syntax.either._
import scalacache.CatsEffect.modes._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import scala.concurrent.ExecutionContext

class CurrencyCacheSpec extends UnitSpec {

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  val expCurs = Vector(
    Currency("Доллар США", 1, 66.8497, 840, "USD").rightNec[CBRError],
    Currency("Евро", 1, 75.8076, 978, "EUR").rightNec[CBRError]
  )

  "CurrencyCache" should "save and retrieve value from cache" in {
    runTestPutAndGet[IO](expCurs).unsafeRunSync() shouldBe expCurs
  }

  it should "save and retrieve value for one action" in {
    runTestCaching[IO](expCurs).unsafeRunSync() shouldBe expCurs
  }

  def runTestPutAndGet[F[_]: cats.effect.Async](currencies: Vector[EitherNec[CBRError, Currency]]): F[Vector[EitherNec[CBRError, Currency]]] = for {
    _ <- put(LocalDate.now)(currencies)
    res <- get(LocalDate.now)
  } yield res.getOrElse(Vector.empty)

  def runTestCaching[F[_]: cats.effect.Async](currencies: Vector[EitherNec[CBRError, Currency]]): F[Vector[EitherNec[CBRError, Currency]]] = for {
    res <- caching(LocalDate.now)(none)(currencies)
  } yield res
}
