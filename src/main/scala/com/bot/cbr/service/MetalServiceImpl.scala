package com.bot.cbr.service

import java.time.LocalDate

import cats.data.{EitherNec, NonEmptyChain}
import cats.effect._
import com.bot.cbr.algebra.MetalService
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.CBRError.{WrongMetalData, WrongUrl, WrongXMLFormat}
import com.bot.cbr.domain.{CBRError, Metal}
import io.chrisdavenport.log4cats.Logger
import org.http4s.Uri
import org.http4s.client.Client
import cats.syntax.either._
import cats.syntax.applicative._
import fs2.Stream
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder
import cats.syntax.functor._
import cats.temp.par._
import com.bot.cbr.domain.CBRError._
import cats.syntax.show._
import com.bot.cbr.domain.date._
import scala.xml.XML

class MetalServiceImpl[F[_]: Sync](config: Config,
                                   client: Client[F],
                                   parser: MetalParserImpl[F, Throwable],
                                   logger: Logger[F]) extends MetalService[F] {

  def url: F[Uri] =
    Uri.fromString(config.urlMetal).leftMap(p => WrongUrl(p.message): Throwable).raiseOrPure[F]

  override def getMetals(start: LocalDate, end: LocalDate): Stream[F, EitherNec[CBRError, Metal]] = for {
    baseUri <- Stream.eval(url)
    uri = baseUri.withQueryParam("date_req1", start.format(dateFormatSlash)).withQueryParam("date_req2", end.format(dateFormatSlash))
    _ <- Stream.eval(logger.info(s"getMetals uri: $uri"))
    s <- Stream.eval(client.expect[String](uri))
    _ <- Stream.eval(logger.info(s"getMetals returns string: $s"))
    eiXml <- Stream.eval(Sync[F].delay(XML.loadString(s))).attempt

    metal <- eiXml match {
      case Right(xml) => for {
        records <- Stream.eval(Sync[F].delay(parseField((xml \\ "Record").toList)))
        _ <- Stream.eval(logger.info(records.fold(_.show, n => s"Try to parse line: ${n.mkString(",")}")))
        record <- records.traverse(Stream.emits(_))

        eMetal <- record match {
          case Right(node) => Stream.eval(parser.parse(node)).attempt
            .map(_.leftMap(e => NonEmptyChain.one(WrongMetalData(e.getMessage): CBRError)))
          case Left(nec) => Stream.eval(logger.error(s"Errors: $nec")).drain ++
              Stream.emit(nec.asLeft[Metal]).covary[F]
        }
      } yield eMetal

      case Left(e) =>
        Stream.eval(logger.error(e)(s"Error while parse xml")).drain ++
          Stream.eval((WrongXMLFormat(e.getMessage): CBRError).leftNec[Metal].pure[F])
    }
  } yield metal

}

object MetalServiceClient0 extends IOApp {

  def runMetalService[F[_] : ConcurrentEffect: Par](): F[Vector[EitherNec[CBRError, Metal]]] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val metals = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          parser = new MetalParserImpl[F, Throwable](identity)
          metalService = new MetalServiceImpl[F](Config("url", "url", "url", "http://www.cbr.ru/scripts/xml_metall.asp", MoexCurrencyUrlConfig("url", "url")), client, parser, logger)
          eiMetal <- metalService.getMetals(LocalDate.now.minusDays(4), LocalDate.now.minusDays(1))


        } yield eiMetal

        metals.compile.toVector
    }

  override def run(args: List[String]): IO[ExitCode] = {
    runMetalService[IO]() map println as ExitCode.Success
  }
}