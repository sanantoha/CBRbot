package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.ApplicativeError
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import cats.data.NonEmptyChain
import cats.effect._
import com.bot.cbr.algebra.MetalService2
import com.bot.cbr.config.Config
import com.bot.cbr.domain.CBRError.WrongUrl
import com.bot.cbr.domain.{CBRError, Metal}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import org.http4s.Uri
import org.http4s.client.Client
import cats.syntax.either._
import cats.syntax.applicativeError._
import cats.syntax.functor._
import cats.syntax.show._
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder

import scala.xml.XML


class MetalServiceImpl2[F[_]: ConcurrentEffect: ApplicativeError[?[_], NonEmptyChain[CBRError]]]
                              (config: Config,
                              client: Client[F],
                              logger: Logger[F],
                              parser: MetalParser[F]) extends MetalService2[F]{

  val dateFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY")
  val dateFormatMetal = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  def url: Either[CBRError, Uri] =
    Uri.fromString(config.urlMetal).leftMap(p => WrongUrl(p.message))

  def getMetalsUri: Stream[F, Uri] =
    Stream.eval(ApplicativeError[F, Throwable].fromEither(url))

  override def getMetals(start: LocalDate, end: LocalDate): Stream[F, Metal] = for {
    baseUri <- getMetalsUri
    uri = baseUri.withQueryParam("date_req1", start.format(dateFormat)).withQueryParam("date_req2", end.format(dateFormat))
    _ <- Stream.eval(logger.info(s"getMetals uri: $uri"))
    s <- Stream.eval(client.expect[String](uri))
    _ <- Stream.eval(logger.info(s"getMetals returns string: $s"))
    ieXml <- Stream.eval(Sync[F].delay(XML.loadString(s))).attempt

    res <- ieXml match {
      case Right(xml) => for {
        records <- Stream.eval(Sync[F].delay((xml \\ "Record").toList))
        record <- Stream.emits(records).covary[F]
        _ <- Stream.eval(logger.debug("Try to parse line: " + record.toString()))
        eMetal <- Stream.eval(parser.parse(record))
      } yield eMetal

      case Left(e) =>
        Stream.eval(logger.error(e)(s"Error while parse xml")).drain ++
          Stream.eval(NonEmptyChain.one(WrongXMLFormat(e.getMessage): CBRError).raiseError[F, Metal])
    }

//    _ <- Stream.eval(res.attempt.traverse {
//      case Right(metal) => logger.info(s"getMetals returns metal: $metal")
//      case Left(nec) => nec.toChain.traverse_(e => logger.error(e)(e.msg))
//    })

  } yield res
}


object MetalServiceClient2 extends IOApp {

  def runMetalService[F[_] : ConcurrentEffect: ApplicativeError[?[_], NonEmptyChain[CBRError]]](): F[Vector[Metal]] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val metals = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          parser = new MetalParser[F]()
          metalService = new MetalServiceImpl2[F](Config("url", "url", "http://www.cbr.ru/scripts/xml_metall.asp"), client, logger, parser)
          eiMetal <- metalService.getMetals(LocalDate.now, LocalDate.now)
          _ <- Stream.eval(logger.info(eiMetal.show))
//          _ <- Stream.eval(logger.info(eiMetal.fold(nec => nec.toString, _.show)))
//          _ <- Stream.eval(eiMetal.traverse(m => logger.info(m.show)))
        } yield eiMetal

        metals.compile.toVector
    }

  override def run(args: List[String]): IO[ExitCode] = {
    ???
//    runMetalService[IO]() map println as ExitCode.Success
  }

}