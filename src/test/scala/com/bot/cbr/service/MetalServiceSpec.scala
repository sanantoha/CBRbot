package com.bot.cbr.service

import java.time.LocalDate
import java.util.concurrent.Executors

import cats.data.EitherNec
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.temp.par._
import com.bot.cbr.config.Config
import com.bot.cbr.domain.MetalType.{Gold, Palladium, Platinum, Silver}
import com.bot.cbr.domain.{CBRError, Metal}
import com.bot.cbr.utils._
import com.bot.cbr.{ReadData, UnitSpec}
import fs2.Stream
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.{Executors => E}
import io.chrisdavenport.log4cats.noop.NoOpLogger

import scala.concurrent.ExecutionContext

class MetalServiceSpec extends UnitSpec {

  val expMetals = Vector(
    Metal(Gold, LocalDate.of(2018, 12, 1), BigDecimal(2610.66), BigDecimal(2610.66)).rightNec[CBRError],
    Metal(Silver, LocalDate.of(2018, 12, 1), BigDecimal(30.51), BigDecimal(30.51)).rightNec[CBRError],
    Metal(Platinum, LocalDate.of(2018, 12, 1), BigDecimal(1732.67), BigDecimal(1732.67)).rightNec[CBRError],
    Metal(Palladium, LocalDate.of(2018, 12, 1), BigDecimal(2549.81), BigDecimal(2549.81)).rightNec[CBRError]
  )

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  "Metal service" should "return list of metals" in {
    runTest[IO]().unsafeRunSync() shouldBe expMetals
  }

  def runMetalService[F[_]: ConcurrentEffect: Par](response: String): F[Vector[EitherNec[CBRError, Metal]]] = {
    val metals = for {
      client <- Stream.emit(mkClient[F](response)).covary[F]
      logger <- Stream.emit(NoOpLogger.impl[F]).covary[F]
      parser = new MetalParserImpl[F, Throwable](identity)
      metalService = new MetalServiceImpl[F](Config("url", "url", "url", "url"), client, parser, logger)
      res <- metalService.getMetals(LocalDate.now, LocalDate.now)
    } yield res
    metals.compile.toVector
  }

  def runTest[F[_]: ConcurrentEffect: ContextShift: Par](): F[Vector[EitherNec[CBRError, Metal]]] = {
    E.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        for {
          response <- new ReadData[F]("src/test/resources/metal_data.xml").apply()
          res <- runMetalService[F](response)
        } yield res
    }
  }
}
