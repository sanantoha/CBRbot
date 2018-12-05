package com.bot.cbr.service

import java.time.LocalDate
import java.util.concurrent.Executors

import cats.data.Validated.Valid
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.{ApplicativeError, Traverse}
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import com.bot.cbr.{ReadData, UnitSpec}
import com.bot.cbr.domain.{CBRError, Metal}
import io.chrisdavenport.linebacker.contexts.{Executors => E}
import io.chrisdavenport.linebacker.Linebacker
import cats.syntax.functor._
import cats.syntax.flatMap._
import com.bot.cbr.config.Config
import com.bot.cbr.domain.MetalType.{Gold, Palladium, Platinum, Silver}
import com.bot.cbr.utils._
import fs2.Stream
import io.chrisdavenport.log4cats.noop.NoOpLogger

import scala.concurrent.ExecutionContext

class MetalServiceSpec extends UnitSpec {

  val expMetals = Vector(
    Valid(Metal(Gold, LocalDate.of(2018, 12, 1), BigDecimal(2610.66), BigDecimal(2610.66))),
    Valid(Metal(Silver, LocalDate.of(2018, 12, 1), BigDecimal(30.51), BigDecimal(30.51))),
    Valid(Metal(Platinum, LocalDate.of(2018, 12, 1), BigDecimal(1732.67), BigDecimal(1732.67))),
    Valid(Metal(Palladium, LocalDate.of(2018, 12, 1), BigDecimal(2549.81), BigDecimal(2549.81)))
  )

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  "Metal service" should "return list of metals" in {
    runTest[IO, ValidatedNec[CBRError, ?]]().unsafeRunSync() shouldBe expMetals
  }

  def runMetalService[F[_]: ConcurrentEffect,
                      G[_]: Traverse: ApplicativeError[?[_], NonEmptyChain[CBRError]]](response: String): F[Vector[G[Metal]]] = {
    val metals = for {
      client <- Stream.emit(mkClient[F](response)).covary[F]
      logger <- Stream.emit(NoOpLogger.impl[F]).covary[F]
      parser = new MetalParserImpl[G, NonEmptyChain[CBRError]](NonEmptyChain.one)
      metalService = new MetalServiceImpl[F, G](Config("url", "url", "url"), client, parser, logger)
      res <- metalService.getMetals(LocalDate.now, LocalDate.now)
    } yield res
    metals.compile.toVector
  }

  def runTest[F[_]: ConcurrentEffect: ContextShift,
              G[_]: Traverse: ApplicativeError[?[_], NonEmptyChain[CBRError]]](): F[Vector[G[Metal]]] = {
    E.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        for {
          response <- new ReadData[F]("src/test/resources/metal_data.xml").apply()
          res <- runMetalService[F, G](response)
        } yield res
    }
  }
}
