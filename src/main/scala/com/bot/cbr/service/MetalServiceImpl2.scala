package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.{ApplicativeError, ~>}
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import cats.data.{EitherNec, NonEmptyChain}
import cats.effect._
import com.bot.cbr.algebra.MetalService2
import com.bot.cbr.config.{Config, MoexCurrencyUrlConfig}
import com.bot.cbr.domain.CBRError.WrongUrl
import com.bot.cbr.domain.{CBRError, Metal}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import org.http4s.Uri
import org.http4s.client.Client
import cats.syntax.functor._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze.BlazeClientBuilder
import cats.syntax.either._
import cats.syntax.applicativeError._
import cats.Parallel
import cats.instances.either._
import doobie.util.ExecutionContexts
//import io.chrisdavenport.log4cats.extras.implicits._
//import cats.syntax.show._
import cats.syntax.applicative._
import scala.xml.XML


class MetalServiceImpl2[F[_]: ConcurrentEffect,
                        G[_]: ApplicativeError[*[_], E], E]
                              (config: Config,
                              client: Client[F],
                              logger: Logger[F],
                              parser: MetalParserImpl[G, E])(implicit mkError: CBRError => E)
  extends MetalService2[F, G]{

  val dateFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY")
  val dateFormatMetal = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  val sLogger: Logger[Stream[F, *]] = logger.mapK(λ[F ~> Stream[F, *]](Stream.eval(_)))

  def url: F[Uri] =
    Uri.fromString(config.urlMetal).leftMap(p => WrongUrl(p.message): Throwable).liftTo[F]

  override def getMetals(start: LocalDate, end: LocalDate): Stream[F, G[Metal]] = for {
    baseUri <- Stream.eval(url)
    uri = baseUri.withQueryParam("date_req1", start.format(dateFormat)).withQueryParam("date_req2", end.format(dateFormat))
    _ <- sLogger.info(s"getMetals uri: $uri")
    s <- Stream.eval(client.expect[String](uri))
    _ <- sLogger.info(s"getMetals returns string: $s")
    eiXml <- Stream.eval(Sync[F].delay(XML.loadString(s))).attempt

    metal <- eiXml match {
      case Right(xml) => for {
        records <- Stream.eval(Sync[F].delay((xml \\ "Record").toList))
        record <- Stream.emits(records).covary[F]
        _ <- sLogger.info("Try to parse line: " + record.toString())
        eMetal = parser.parse(record)
      } yield eMetal

      case Left(e) =>
        sLogger.error(e)(s"Error while parse xml").drain ++
          Stream.eval(mkError(WrongXMLFormat(e.getMessage): CBRError).raiseError[G, Metal].pure[F])
    }
  } yield metal

}


object MetalServiceClient2 extends IOApp {

  def app[F[_]: ConcurrentEffect,
          G[_]: ApplicativeError[*[_], E]: Parallel, E]()
      (implicit mkError: CBRError => E): Resource[F, (Logger[F], MetalService2[F, G])] =

    for {
      serverEc <- ExecutionContexts.cachedThreadPool[F]
      client <- BlazeClientBuilder[F](serverEc).resource
      logger <- Resource.liftF(Slf4jLogger.create)
      parser = new MetalParserImpl[G, E](mkError)
      metalService = new MetalServiceImpl2[F, G, E](
        Config("url", "url", "url", "http://www.cbr.ru/scripts/xml_metall.asp", MoexCurrencyUrlConfig("url", "url")),
        client,
        logger,
        parser)
    } yield (logger, metalService)

  def runApp[F[_]: Sync, G[_]: ApplicativeError[*[_], E], E]: (Logger[F], MetalService2[F, G]) => F[Vector[G[Metal]]] = {
    case (_, metalService) =>  for {
      eiMetal <- metalService.getMetals(LocalDate.now.minusDays(3), LocalDate.now)
         .compile
         .toVector
//        _ <- logger.info(eiMetal.map(_.fold(_.getMessage, _.show)).mkString(","))
    } yield eiMetal
  }

  override def run(args: List[String]): IO[ExitCode] = {
//    import cats.syntax.applicative._

//    implicit val nt = new (EitherNec[CBRError, *] ~> IO) {
//      override def apply[A](fa: EitherNec[CBRError, A]): IO[A] = fa match {
//        case Right(x) => x.pure[IO]
//        case Left(nec) => IO.raiseError(new RuntimeException(nec.foldLeft("")((acc, e2) => acc + "\n" + e2.show)))
//      }
//    }
    implicit val mkError: CBRError => NonEmptyChain[CBRError] = NonEmptyChain.one

    app[IO, EitherNec[CBRError, *], NonEmptyChain[CBRError]]().use(runApp[IO, EitherNec[CBRError, *], NonEmptyChain[CBRError]].tupled) map println as ExitCode.Success
  }

}