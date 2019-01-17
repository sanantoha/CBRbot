package com.bot.cbr.service

import java.text.DecimalFormat

import cats.data.EitherNec
import cats.effect._
import com.bot.cbr.algebra.MoexCurrencyService
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.CBRError.{WrongDateFormat, WrongUrl}
import com.bot.cbr.domain.{CBRError, MoexCurrency, MoexCurrencyType}
import io.chrisdavenport.log4cats.Logger
import fs2.Stream
import org.http4s.Uri
import cats.syntax.either._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Document, Element}
import org.http4s.client.Client
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elementList}
import com.bot.cbr.domain.date._
import java.time.LocalDate
import java.util.Locale
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.instances.parallel._
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder


class MoexCurrencyServiceImpl[F[_]: Sync](config: Config, client: Client[F], logger: Logger[F]) extends MoexCurrencyService[F] {

  type E[A] = EitherNec[CBRError, A]

  Locale.setDefault(new Locale("ru", "RU"))

  def url(moexCurType: MoexCurrencyType): F[Uri] = {
    val base = moexCurType match {
      case MoexCurrencyType.USD => config.moexCurUrlConfig.urlUsd
      case MoexCurrencyType.EUR => config.moexCurUrlConfig.urlEur
    }

    Uri.fromString(base).leftMap(p => WrongUrl(p.message): Throwable).raiseOrPure[F]
  }

  override def getCurrencies(moexCurType: MoexCurrencyType): Stream[F, EitherNec[CBRError, MoexCurrency]] = for {
    uri <- Stream.eval(url(moexCurType))
    _ <- Stream.eval(logger.info(s"getCurrencies($moexCurType) uri: $uri"))
    s <- Stream.eval(client.expect[String](uri))

    browser = JsoupBrowser()
    eiDoc <- Stream.eval(Sync[F].delay(browser.parseString(s))).attempt
    cur <- eiDoc match {
      case Right(doc) => parseMoexCurrencies(moexCurType, doc)
      case Left(e) => Stream.eval(logger.error(e)(s"Error parsing ${e.getMessage}")).drain ++
        Stream.emit(WrongDateFormat(e.getMessage).leftNec[MoexCurrency])
    }
  } yield cur


  def parseMoexCurrencies(moexCurType: MoexCurrencyType, doc: Document): Stream[F, EitherNec[CBRError, MoexCurrency]] = {
    for {
      quoteElem <- parseField(doc >> elementList(".quote__data tr")).traverse(Stream.emits(_)).drop(1)
      _ <- Stream.eval(logger.debug(s"parse element: ${quoteElem.fold(_.toChain.toList.mkString(","), _.toString)}"))
      cur <- quoteElem match {
        case Right(elem) => Stream.emit(parseMoexCurrency(moexCurType, elem)).covary[F]
        case Left(nec) =>
          Stream.eval(logger.error(s"Errors: ${nec.toChain.toList.mkString(",")}")).drain ++
            Stream.emit(nec.asLeft[MoexCurrency]).covary[F]
      }
    } yield cur
  }

  def parseMoexCurrency(moexCurType: MoexCurrencyType, elem: Element): E[MoexCurrency] = {
    val df = new DecimalFormat()
    val date: E[LocalDate] = parseField(LocalDate.parse((elem >> element(".quote__date")).text, dateFormatShort))
    val value: E[BigDecimal] = parseField {
      val quoteValue = (elem >> element(".quote__value")).text
      BigDecimal(df.parse(quoteValue).doubleValue())
    }

    val change: E[BigDecimal] = parseField {
      val quoteChange = (elem >> element(".quote__change")).text
      BigDecimal(df.parse(quoteChange).doubleValue())
    }

    val cur = (date, value, change).parMapN(MoexCurrency(moexCurType, _, _, _))
    cur
  }
}

object MoexCurrencyServiceClient extends IOApp {

  def runMoexCurrencyService[F[_]: ConcurrentEffect](): F[Vector[EitherNec[CBRError, MoexCurrency]]] = {
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val currencies = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          config = Config("token", "url", "https://www.moex.com/export/derivatives/currency-rate.aspx", "url",
            MoexCurrencyUrlConfig("https://news.yandex.ru/quotes/2002.html", "https://news.yandex.ru/quotes/2000.html")
          )
          service = new MoexCurrencyServiceImpl[F](config, client, logger)
          cur <- service.getCurrencies(MoexCurrencyType.EUR)
          _ <- Stream.eval(logger.info(cur.show))
        } yield cur

        currencies.compile.toVector
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    runMoexCurrencyService[IO]() map println as ExitCode.Success
}