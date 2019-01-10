package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.ApplicativeError
import cats.data.EitherNec
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
import org.http4s.{Header, Headers, Method, Request, Uri}
import cats.instances.string._
import cats.syntax.parallel._
import cats.instances.parallel._

import scala.xml.{Node, XML}


class CurrencyServiceImpl[F[_] : ConcurrentEffect](config: Config, client: Client[F], logger: Logger[F]) extends CurrencyService[F] {

  type E[A] = EitherNec[CBRError, A]

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

  override def getCurrencies(date: LocalDate): Stream[F, EitherNec[CBRError, Currency]] = for {
    req <- request(date)
    _ <- Stream.eval(logger.info(s"invoke getCurrencies($date)"))
    s <- client.stream(req).flatMap(_.body.chunks.through(fs2.text.utf8DecodeC)).foldMonoid

    ei <- Stream.eval(Sync[F].delay(XML.loadString(s)).attempt)
    _ <- Stream.eval(logger.info(ei.fold(e => s"Error load XML: ${e.getMessage}", el => s"XML loaded: ${el.toString}")))

    cur <- ei match {
      case Right(xml) => for {
        vcd <- Stream.eval(Sync[F].delay(parseField((xml \\ "ValuteCursOnDate").toList)))
        _ <- Stream.eval(logger.info(vcd.fold(_.show, lst => lst.mkString(","))))
        node <- vcd.traverse(Stream.emits(_))
        cur <- node match {
          case Right(n) => Stream.eval(Sync[F].delay(parseCurrency(n)))
          case Left(nec) => Stream.eval(logger.error(s"Errors: ${nec.show}")).drain ++
              Stream.emit(nec.asLeft[Currency]).covary[F]
        }

        _ <- Stream.eval(logger.info(cur.fold(_.show, c => s"parse: ${c.show}")))

      } yield cur
      case Left(e) => Stream.eval(WrongXMLFormat(e.getMessage).leftNec[Currency].pure[F])
    }
  } yield cur

  def parseCurrency(node: Node): E[Currency] = {
    val name: E[String] = parseField((node \ "Vname").text.trim)
    val nom: E[BigDecimal] = parseField(BigDecimal((node \ "Vnom").text))
    val curs: E[BigDecimal] = parseField(BigDecimal((node \ "Vcurs").text))
    val code: E[Int] = parseField((node \ "Vcode").text.toInt)
    val chCode: E[String] = parseField((node \ "VchCode").text)
    (name, nom, curs, code, chCode).parMapN(Currency.apply)
  }

}

object CurrencyClient extends IOApp {

  def runCurrencyService[F[_] : ConcurrentEffect]: F[Vector[EitherNec[CBRError, Currency]]] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val currencies = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          config = Config("token", "https://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx", "url", "url")
          service = new CurrencyServiceImpl[F](config, client, logger)
          res <- service.getCurrencies(LocalDate.now())
        } yield res

        currencies.compile.toVector
    }


  override def run(args: List[String]): IO[ExitCode] = {
    runCurrencyService[IO] map println as ExitCode.Success
  }
}