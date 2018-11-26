package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.ApplicativeError
import cats.effect._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.show._
import com.bot.cbr.algebra.CurrencyService
import com.bot.cbr.config.Config
import com.bot.cbr.domain.CBRError.{WrongUrl, WrongXMLFormat}
import com.bot.cbr.domain.{CBRError, Currency}
import fs2.Stream
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.{Header, Headers, Method, Request, Uri}
import cats.instances.string._

import scala.xml.XML


class CurrencyServiceImpl[F[_] : ConcurrentEffect](config: Config, client: Client[F], logger: Logger[F])
  extends CurrencyService[F] with Http4sClientDsl[F] {

  def url: Either[CBRError, Uri] =
    Uri.fromString(config.urlCurrency).leftMap(p => WrongUrl(p.message))

  def query(date: LocalDate): String =
    s"""|<?xml version="1.0" encoding="utf-8"?>
        |<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">
        |<soap12:Body>
        | <GetCursOnDate xmlns="http://web.cbr.ru/">
        |  <On_date>${date.format(DateTimeFormatter.ISO_LOCAL_DATE)}</On_date>
        | </GetCursOnDate>
        |</soap12:Body>
        |</soap12:Envelope>""".stripMargin.trim

  def request(date: LocalDate): Stream[F, Request[F]] =
    Stream.eval(ApplicativeError[F, Throwable].fromEither(
      url.map { x =>
        Request[F](
          method = Method.POST,
          uri = x,
          headers = Headers(
            Header("Host", "www.cbr.ru"),
            Header("Content-Type", "application/soap+xml; charset=utf-8"),
            Header("Content-Length", "length")
          ),
          body = Stream.emits(query(date).getBytes)
        )
      }
    ))

  override def getCurrencies(date: LocalDate): Stream[F, Either[CBRError, Currency]] = for {
    req <- request(date)
    s <- client.stream(req).flatMap(_.body.chunks.through(fs2.text.utf8DecodeC)).foldMonoid

    ei <- Stream.eval(Sync[F].delay(XML.loadString(s)).attempt)
    _ <- Stream.eval(logger.debug(ei.fold(e => s"Error load XML: ${e.getMessage}", el => s"XML loaded: ${el.toString}")))

    cur <- ei match {
      case Right(xml) => for {
        vcd <- Stream.eval(Sync[F].delay((xml \\ "ValuteCursOnDate").toList))
        _ <- Stream.eval(logger.debug(vcd.mkString(",")))
        node <- Stream.emits(vcd).covary[F]
        cur <- Stream.eval(Sync[F].delay {
          val name = (node \\ "Vname").text.trim
          val nom = BigDecimal((node \\ "Vnom").text)
          val curs = BigDecimal((node \\ "Vcurs").text)
          val code = (node \\ "Vcode").text.toInt
          val chCode = (node \\ "VchCode").text
          Currency(name, nom, curs, code, chCode)
        }).attempt.map(_.leftMap(e => WrongXMLFormat(e.getMessage): CBRError))
        _ <- Stream.eval(logger.debug(cur.fold(_.show, c => s"Successfully read: ${c.show}")))
      } yield cur
      case Left(e) => Stream.eval(WrongXMLFormat(e.getMessage).asLeft[Currency].pure[F])
    }
  } yield cur

}

object CurrencyClient extends IOApp {

  def runCurrencyService[F[_] : ConcurrentEffect]: F[Vector[Either[CBRError, Currency]]] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val currencies = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          config = Config("token", "https://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx", "url")
          service = new CurrencyServiceImpl[F](config, client, logger)
          res <- service.getCurrencies(LocalDate.now())
        } yield res

        currencies.compile.toVector
    }


  override def run(args: List[String]): IO[ExitCode] = {
    runCurrencyService[IO] map println as ExitCode.Success
  }
}