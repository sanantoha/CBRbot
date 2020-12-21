package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.effect._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.show._
import com.bot.cbr.algebra.MetalService3
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.CBRError.{WrongUrl, WrongXMLFormat}
import com.bot.cbr.domain.Metal
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import cats.Parallel
import doobie.util.ExecutionContexts

import scala.xml.XML


class MetalServiceImpl3[F[_]: ConcurrentEffect, E](config: Config,
                                                client: Client[F],
                                                logger: Logger[F],
                                                parser: MetalParserImpl[F, E]) extends MetalService3[F]{

  val dateFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY")
  val dateFormatMetal = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  def url: F[Uri] =
    Uri.fromString(config.urlMetal).leftMap(p => WrongUrl(p.message): Throwable).liftTo[F]

  override def getMetals(start: LocalDate, end: LocalDate): Stream[F, Metal] = for {
    baseUri <- Stream.eval(url)
    uri = baseUri.withQueryParam("date_req1", start.format(dateFormat)).withQueryParam("date_req2", end.format(dateFormat))
    _ <- Stream.eval(logger.info(s"getMetals uri: $uri"))
    s <- Stream.eval(client.expect[String](uri))
    _ <- Stream.eval(logger.info(s"getMetals returns string: $s"))
    eiXml <- Stream.eval(Sync[F].delay(XML.loadString(s))).attempt

    metal <- eiXml match {
      case Right(xml) => for {
        records <- Stream.eval(Sync[F].delay((xml \\ "Record").toList))
        record <- Stream.emits(records).covary[F]
        _ <- Stream.eval(logger.info("Try to parse line: " + record.toString()))
        eMetal <- Stream.eval(parser.parse(record))//.attempt
//        met <- eMetal match {
//          case Right(m) => Stream.emit(m).covary[F]
//          case Left(e) => Stream.eval(logger.error(e)(e.getMessage)).drain
//        }
      } yield eMetal

      case Left(e) =>
        Stream.eval(logger.error(e)(s"Error while parse xml")).drain ++
          Stream.eval((WrongXMLFormat(e.getMessage): Throwable).raiseError[F, Metal])
    }

  } yield metal
}


object MetalServiceClient3 extends IOApp {

  def runMetalService[F[_]: ConcurrentEffect: Parallel](): F[Vector[Either[Throwable, Metal]]] = {
        val metals = for {
          serverEc <- ExecutionContexts.cachedThreadPool[F]
          client <- BlazeClientBuilder[F](serverEc).resource
          logger <- Resource.liftF(Slf4jLogger.create)
          parser = new MetalParserImpl[F, Throwable](identity)
          metalService = new MetalServiceImpl3[F, Throwable](
            Config("url", "url", "url", "http://www.cbr.ru/scripts/xml_metall.asp", MoexCurrencyUrlConfig("url", "url")),
            client,
            logger,
            parser)
        } yield metalService

        metals.use { service =>
          val r = for {
            logger <- Stream.eval(Slf4jLogger.create)
            eiMetal <- service.getMetals(LocalDate.now.minusDays(5), LocalDate.now).attempt
            _ <- Stream.eval(logger.info(eiMetal.fold(nec => "Error: " + nec.toString, _.show)))
          } yield eiMetal
          r.compile.toVector
        }
    }

  override def run(args: List[String]): IO[ExitCode] = {
    runMetalService[IO]() map println as ExitCode.Success
  }

}