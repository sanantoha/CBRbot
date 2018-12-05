package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.data.Validated.{Invalid, Valid}
import cats.{ApplicativeError, ~>}
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import cats.data.{NonEmptyChain, ValidatedNec}
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
import cats.syntax.functor._
import cats.syntax.show._
import io.chrisdavenport.linebacker.Linebacker
import io.chrisdavenport.linebacker.contexts.Executors
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder
import cats.syntax.either._

import scala.xml.XML


class MetalServiceImpl2[F[_]: ConcurrentEffect,
                        G[_]: ApplicativeError[?[_], E], E]
                              (config: Config,
                              client: Client[F],
                              logger: Logger[F],
                              parser: MetalParserImpl[G, E])(implicit nt: G ~> F, mkError: CBRError => E)
  extends MetalService2[F]{

  val dateFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY")
  val dateFormatMetal = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  def url: F[Uri] =
    Uri.fromString(config.urlMetal).leftMap(p => WrongUrl(p.message): Throwable).raiseOrPure

  override def getMetals(start: LocalDate, end: LocalDate): Stream[F, Either[CBRError, Metal]] = for {
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
        eMetal <- Stream.eval(nt(parser.parse(record))).attempt
        met <- eMetal match {
          case Right(x) => Stream.emit(x.asRight[CBRError]).covary[F]
          case Left(e) => Stream.emit((WrongXMLFormat(e.getMessage): CBRError).asLeft[Metal]).covary[F]
        }
      } yield met

      case Left(e) =>
        Stream.eval(logger.error(e)(s"Error while parse xml")).drain ++
          Stream.emit((WrongXMLFormat(e.getMessage): CBRError).asLeft[Metal]).covary[F]
//          Stream.eval(nt(mkError(WrongXMLFormat(e.getMessage): CBRError).raiseError[G, Metal]))
    }
//    res <- eiMetal match {
//      case Right(metal) =>
//        Stream.eval(logger.info(s"getMetals returns metal: $metal")).drain ++ Stream.emit(metal).covary[F]
//      case Left(e) => Stream.e
//    }

  } yield metal
}


object MetalServiceClient2 extends IOApp {

  def runMetalService[F[_] : ConcurrentEffect, G[_]: ApplicativeError[?[_], E], E](implicit nt: G ~> F, mkError: CBRError => E): F[Vector[Either[CBRError, Metal]]] =
    Executors.unbound[F].map(Linebacker.fromExecutorService[F]).use {
      implicit linebacker: Linebacker[F] =>
        val metals = for {
          client <- BlazeClientBuilder[F](linebacker.blockingContext).stream
          logger <- Stream.eval(Slf4jLogger.create)
          parser = new MetalParserImpl[G, E](mkError)
          metalService = new MetalServiceImpl2[F, G, E](
            Config("url", "url", "http://www.cbr.ru/scripts/xml_metall.asp"),
            client,
            logger,
            parser)
          eiMetal <- metalService.getMetals(LocalDate.now, LocalDate.now)
//          _ <- Stream.eval(logger.info(eiMetal.show))
          _ <- Stream.eval(logger.info(eiMetal.fold(nec => "Error: " + nec.toString, _.show)))
//          _ <- Stream.eval(eiMetal.traverse(m => logger.info(m.show)))
        } yield eiMetal

        metals.compile.toVector
    }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.syntax.apply._

    implicit val nt = new (ValidatedNec[CBRError, ?] ~> IO) {
      override def apply[A](fa: ValidatedNec[CBRError, A]): IO[A] = fa match {
        case Valid(x) => IO(x)
        case Invalid(nec) => IO{println("NT: " + nec.toString)} *> nec.tail.foldLeft(IO.raiseError(nec.head)) {
          case (acc, x) => acc *> IO.raiseError(x)
        }
      }
    }

    implicit val mkError: CBRError => NonEmptyChain[CBRError] = NonEmptyChain.one

    runMetalService[IO, ValidatedNec[CBRError, ?], NonEmptyChain[CBRError]] map println as ExitCode.Success
  }

}