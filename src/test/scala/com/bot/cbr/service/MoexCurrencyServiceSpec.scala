package com.bot.cbr.service

import java.text.DecimalFormat
import java.util.Locale
import java.util.concurrent.Executors

import cats.effect.{ConcurrentEffect, ContextShift, IO, Sync}
import com.bot.cbr.{ReadData, UnitSpec}
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.{Executors => E}
import cats.syntax.functor._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.concurrent.ExecutionContext
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import cats.syntax.option._
import net.ruippeixotog.scalascraper.model.Element
import cats.syntax.show._
import cats.instances.option._
import cats.instances.string._
import cats.instances.bigDecimal._
import cats.instances.double._

class MoexCurrencyServiceSpec extends UnitSpec {

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  Locale.setDefault(new Locale("ru", "RU"))

  "test" should "test" in {
    runTest[IO]().unsafeRunSync() shouldBe (())
  }

  def runTest[F[_]: ConcurrentEffect: ContextShift](): F[Unit] = {
    E.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        for {
          logger <- Slf4jLogger.create
          response <- new ReadData[F]("src/test/resources/eur_data.xml").apply()
          _ <- logger.info(response)
          browser = JsoupBrowser()
          eiDoc <- Sync[F].delay(browser.parseString(response)).attempt
//          _ <- logger.info(eiDoc.toString)
          optElems <- eiDoc match {
//            case Right(doc) => Sync[F].delay(doc >> elementList(".quote__data tr td.quote__value"))
            case Right(doc) => Sync[F].delay(doc >> elementList(".quote__data tr"))
            case Left(e) => logger.error(e)(s"Error ${e.getMessage}") as List[Element]()
          }

          _ <- logger.info(optElems.toString)

          lst = optElems.drop(1).map { elem =>
            val date = (elem >> element(".quote__date")).text

            val df = new DecimalFormat()
            val value = (elem >> element(".quote__value")).text
            val cur = BigDecimal(df.parse(value).doubleValue())

            val signValue = (elem >> element(".quote__change")).text
            val sign = BigDecimal(df.parse(signValue).doubleValue())
            (date, cur, sign)
          }

          _ <- logger.info(lst.mkString(","))

//          df = new DecimalFormat()
//          v = optElem.headOption.map(_.text)
//            .map(df.parse)
//            .map(_.doubleValue())
//            .map(BigDecimal(_))
//            .getOrElse(BigDecimal(0))
//          _ <- logger.info(v.show)
        } yield ()
    }
  }
}
