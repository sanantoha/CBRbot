package com.bot.cbr.cache

import java.time.LocalDate
import java.util.concurrent.Executors

import cats.data.EitherNec
import cats.effect.{ContextShift, IO}
import com.bot.cbr.UnitSpec
import com.bot.cbr.domain.{CBRError, Metal}
import com.bot.cbr.domain.MetalType.{Gold, Silver}
import cats.syntax.either._
import scalacache._
import scalacache.CatsEffect.modes._
import scala.concurrent.ExecutionContext
import MetalCache._
import cats.syntax.flatMap._
import cats.syntax.functor._

class MetalCacheSpec extends UnitSpec {

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  val expMetals = Vector(
    Metal(Gold, LocalDate.of(2018, 12, 1), BigDecimal(2610.66), BigDecimal(2610.66)).rightNec[CBRError],
    Metal(Silver, LocalDate.of(2018, 12, 1), BigDecimal(30.51), BigDecimal(30.51)).rightNec[CBRError]
  )

  "MetalCache" should "save and retrieve value from cache" in {
    runTestPutAndGet[IO](expMetals).unsafeRunSync() shouldBe expMetals
  }

  def runTestPutAndGet[F[_]: cats.effect.Async](vec: Vector[EitherNec[CBRError, Metal]]): F[Vector[EitherNec[CBRError, Metal]]] = for {
    _ <- put(LocalDate.now, LocalDate.now.plusDays(1))(vec)
    res <- get(LocalDate.now, LocalDate.now.plusDays(1))
  } yield res.getOrElse(Vector.empty)
}
