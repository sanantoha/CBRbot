package com.bot.cbr.service

import java.text.DecimalFormat
import java.time.LocalDate
import java.util.Locale

import com.bot.cbr.algebra.MetalParser
import cats.ApplicativeError
import cats.data.NonEmptyChain
import cats.effect.{ExitCode, IO, IOApp}
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import com.bot.cbr.domain.{CBRError, Metal, MetalType}
import com.bot.cbr.domain.MetalType.lookUpMetalType
import cats.syntax.either._
import cats.temp.par._
import cats.syntax.parallel._
import com.bot.cbr.domain.date._
import scala.xml.{Node, XML}

class MetalParserImpl[F[_]: ApplicativeError[?[_], E]: Par, E](mkError: CBRError => E) extends MetalParser[F] {

  Locale.setDefault(new Locale("ru", "RU"))

  def parse(record: Node): F[Metal] = {
    (parseMetalType(record),
      parseDate(record),
      parseBigDecimal(record, "Buy"),
      parseBigDecimal(record, "Sell")
    ).parMapN(Metal.apply)
  }

  def parseMetalType(record: Node): F[MetalType] =
    Either.catchNonFatal(lookUpMetalType((record \ "@Code").text.toInt))
      .leftMap(e => mkError(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure[F]

  def parseDate(record: Node): F[LocalDate] =
    Either.catchNonFatal {
      LocalDate.parse((record \ "@Date").text, dateFormat)
    }.leftMap(e => mkError(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure[F]

  def parseBigDecimal(record: Node, name: String): F[BigDecimal] =
    Either.catchNonFatal {
      val df = new DecimalFormat()
      BigDecimal(df.parse((record \\ name).text).doubleValue())
//      BigDecimal((record \\ name).text)
    }.leftMap(e => mkError(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure[F]
}

object MetalParserImpl2Demo extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    import cats.syntax.functor._
    import cats.instances.either._
    import cats.instances.parallel._

    val xml = XML.loadString("<Record Date=\"29.11.2018\" Code=\"1\"><Buy>2611,15</Buy><Sell>2611,15</Sell></Record>")
    val parser = new MetalParserImpl[Either[NonEmptyChain[CBRError], ?], NonEmptyChain[CBRError]](NonEmptyChain.one)

    IO {
      println(parser.parse(xml))
    } as ExitCode.Success
  }
}
