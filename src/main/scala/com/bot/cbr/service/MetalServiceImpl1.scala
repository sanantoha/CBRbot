package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.{ApplicativeError, Parallel, Traverse, ~>}
import cats.data.{EitherNec, NonEmptyChain}
import cats.effect._
import cats.syntax.either._
import com.bot.cbr.algebra.MetalService1
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.CBRError.{WrongUrl, WrongXMLFormat}
import com.bot.cbr.domain.{CBRError, Metal}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import cats.syntax.applicativeError._
import cats.syntax.traverse._
import cats.syntax.show._
import Metal._
import cats.syntax.foldable._
import cats.instances.either._
import doobie.util.ExecutionContexts

import scala.xml.XML


class MetalService1Impl[F[_] : ConcurrentEffect,
                       G[_]: Traverse: ApplicativeError[*[_], NonEmptyChain[CBRError]]]
                            (config: Config,
                             client: Client[F],
                             parser: MetalParserImpl[G, NonEmptyChain[CBRError]],
                             logger: Logger[F]
                            ) extends MetalService1[F, G] {

  val dateFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY")
  val dateFormatMetal = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  def url: F[Uri] =
    Uri.fromString(config.urlMetal).leftMap(p => WrongUrl(p.message): Throwable).liftTo[F]

  override def getMetals(start: LocalDate, end: LocalDate): Stream[F, G[Metal]] = for {
    baseUri <- Stream.eval(url)
    uri = baseUri.withQueryParam("date_req1", start.format(dateFormat)).withQueryParam("date_req2", end.format(dateFormat))
    _ <- Stream.eval(logger.info(s"getMetals uri: $uri"))
    s <- Stream.eval(client.expect[String](uri))
    _ <- Stream.eval(logger.info(s"getMetals returns string: $s"))
    eiXml <- Stream.eval(Sync[F].delay(XML.loadString(s))).attempt

    res <- eiXml match {
      case Right(xml) => for {
        records <- Stream.eval(Sync[F].delay((xml \\ "Record").toList))
        record <- Stream.emits(records).covary[F]
        _ <- Stream.eval(logger.debug("Try to parse line: " + record.toString()))
        eMetal <- Stream.emit(parser.parse(record)).covary[F]
      } yield eMetal

      case Left(e) =>
        Stream.eval(logger.error(e)(s"Error while parse xml")).drain ++
          Stream.emit(NonEmptyChain.one(WrongXMLFormat(e.getMessage): CBRError).raiseError[G, Metal]).covary[F]
    }

    _ <- Stream.eval(res.attempt.traverse {
      case Right(metal) => logger.info(s"getMetals returns metal: $metal")
      case Left(nec) => nec.toChain.traverse_(e => logger.error(e)(e.msg))
    })

  } yield res
}


object MetalService1Client extends IOApp {

  import cats.syntax.applicative._

  implicit val nt = new (EitherNec[CBRError, *] ~> IO) {
    override def apply[A](fa: EitherNec[CBRError, A]): IO[A] = fa match {
      case Right(x) => x.pure[IO]
      case Left(nec) => IO.raiseError(new RuntimeException(nec.foldLeft("")((acc, e2) => acc + "\n" + e2.show)))
    }
  }

  def runMetalService[F[_] : ConcurrentEffect,
                      G[_]: Traverse: ApplicativeError[*[_], NonEmptyChain[CBRError]]: Parallel](): F[Vector[G[Metal]]] = {
        val metals = for {
          serverEc <- ExecutionContexts.cachedThreadPool[F]
          client <- BlazeClientBuilder[F](serverEc).resource
          logger <- Resource.liftF(Slf4jLogger.create)
          parser = new MetalParserImpl[G, NonEmptyChain[CBRError]](NonEmptyChain.one)
          metalService = new MetalService1Impl[F, G](
            Config("url", "url", "url", "http://www.cbr.ru/scripts/xml_metall.asp", MoexCurrencyUrlConfig("url", "url")),
            client,
            parser,
            logger)
        } yield metalService

        metals.use { service =>
          val r = for {
            logger <- Stream.eval(Slf4jLogger.create)
            eiMetal <- service.getMetals(LocalDate.now.minusDays(3), LocalDate.now)
            _ <- Stream.eval(eiMetal.traverse(m => logger.info(m.show)))
          } yield eiMetal
          r.compile.toVector
        }
    }

  override def run(args: List[String]): IO[ExitCode] = {

    runMetalService[IO, EitherNec[CBRError, *]]() map println as ExitCode.Success
  }

}