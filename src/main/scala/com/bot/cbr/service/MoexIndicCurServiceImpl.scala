package com.bot.cbr.service

import java.time.{LocalDate, LocalDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

import cats.data.EitherNec
import cats.effect._
import com.bot.cbr.algebra.MoexIndicCurService
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.CBRError.{WrongUrl, WrongXMLFormat}
import com.bot.cbr.domain.{CBRError, MoexIndicCurrency}
import io.chrisdavenport.log4cats.Logger
import org.http4s.client.Client
import fs2.Stream
import org.http4s.Uri
import cats.syntax.either._
import cats.syntax.parallel._
import cats.syntax.functor._
import cats.instances.parallel._
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder
import cats.instances.list._
import cats.instances.either._

import scala.xml.{Elem, Node, XML}

class MoexIndicCurServiceImpl[F[_]: ConcurrentEffect](config: Config, client: Client[F], logger: Logger[F]) extends MoexIndicCurService[F] {

  type E[A] = EitherNec[CBRError, A]

  val dateTimeFormatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .parseCaseInsensitive.append(DateTimeFormatter.ISO_LOCAL_DATE)
    .appendLiteral(' ').append(DateTimeFormatter.ISO_LOCAL_TIME)
    .toFormatter

  def url: F[Uri] =
    Uri.fromString(config.urlMoexCurrency).leftMap(p => WrongUrl(p.message): Throwable).raiseOrPure[F]

  override def getCurrencies(exchangeType: String, date: LocalDate): Stream[F, EitherNec[CBRError, MoexIndicCurrency]] = for {
    baseUri <- Stream.eval(url)

    uri = baseUri.withQueryParam("language", "ru")
      .withQueryParam("currency", exchangeType)
      .withQueryParam("moment_start", DateTimeFormatter.ISO_DATE.format(date))
      .withQueryParam("moment_end", DateTimeFormatter.ISO_DATE.format(date))

    _ <- Stream.eval(logger.info(s"getCurrencies MOEX uri: $uri"))
    s <- Stream.eval(client.expect[String](uri))

    ieXml <- Stream.eval(Sync[F].delay(XML.loadString(s))).attempt
    cur <- ieXml match {
      case Right(xml) => parseCurrency(xml)
      case Left(e) =>
        Stream.eval(logger.error(e)(s"Error: ${e.getMessage}")).drain ++
          Stream.emit((WrongXMLFormat(e.getMessage): CBRError).leftNec[MoexIndicCurrency]).covary[F]
    }
  } yield cur

  def parseCurrency(xml: Elem): Stream[F, EitherNec[CBRError, MoexIndicCurrency]] = {
    val eiLst: E[List[Node]] = parseField((xml \ "rates" \ "rate").toList)
    val eiEt: E[String] = parseField(xml \@ "exchange-type")
    (eiEt, eiLst).parMapN {
      case (et, lst) => lst.parTraverse(createMoexCurrency(et))
    }.flatMap(identity).traverse(Stream.emits(_))
  }

  def createMoexCurrency(exchangeType: String)(node: Node): E[MoexIndicCurrency] = {
    val cur: E[BigDecimal] = parseField(BigDecimal(node \@ "value"))
    val dateTime: E[LocalDateTime] = parseField {
      val dateTime = node \@ "moment"
      LocalDateTime.parse(dateTime, dateTimeFormatter)
    }
    (dateTime, cur).parMapN {
      case (dt, c) => MoexIndicCurrency(exchangeType, dt, c)
    }
  }
}


object MoexCurrencyServiceClient extends IOApp {

  def runCurrencyService[F[_]: ConcurrentEffect]: F[Vector[EitherNec[CBRError, MoexIndicCurrency]]] = {
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val currencies = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          config = Config("token", "url", "https://www.moex.com/export/derivatives/currency-rate.aspx", "url", MoexCurrencyUrlConfig("url", "url"))
          service = new MoexIndicCurServiceImpl[F](config, client, logger)
          cur <- service.getCurrencies("EUR/RUB", LocalDate.now.minusDays(1))
          _ <- Stream.eval(logger.info(cur.show))
        } yield cur

        currencies.compile.toVector
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    runCurrencyService[IO] map println as ExitCode.Success
}