package com.bot.cbr.service

import java.text.DecimalFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import cats.ApplicativeError
import cats.effect.{ExitCode, IO, IOApp}
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import com.bot.cbr.domain.{CBRError, Metal, MetalType}
import com.bot.cbr.domain.MetalType.lookUpMetalType
import cats.syntax.apply._
import cats.syntax.either._
import com.bot.cbr.algebra.MetalParser

import scala.xml.{Node, XML}

class MetalParserImpl[F[_]: ApplicativeError[?[_], E], E](mkError: CBRError => E) extends MetalParser[F] {

  Locale.setDefault(new Locale("ru", "RU"))

  val dateFormatMetal = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  def parse(record: Node): F[Metal] = {
    (parseMetalType(record),
      parseDate(record),
      parseBigDecimal(record, "Buy"),
      parseBigDecimal(record, "Sell")
    ).mapN(Metal.apply)
  }

  def parseMetalType(record: Node): F[MetalType] =
    Either.catchNonFatal(lookUpMetalType((record \ "@Code").text.toInt))
      .leftMap(e => mkError(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure

  def parseDate(record: Node): F[LocalDate] =
    Either.catchNonFatal {
      LocalDate.parse((record \ "@Date").text, dateFormatMetal)
    }.leftMap(e => mkError(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure

  def parseBigDecimal(record: Node, name: String): F[BigDecimal] =
    Either.catchNonFatal {
      val df = new DecimalFormat()
      BigDecimal(df.parse((record \\ name).text).doubleValue())
//      println(df)
//      BigDecimal((record \\ name).text)
    }.leftMap(e => mkError(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure
}

object MetalParserImpl2Demo extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    import cats.syntax.functor._
    import cats.instances.either._

    val xml = XML.loadString("<Record Date=\"29.11.2018\" Code=\"1\"><Buy>2611,15</Buy><Sell>2611,15</Sell></Record>")
    val parser = new MetalParserImpl[Either[CBRError, ?], CBRError](identity)

    IO {
      println(parser.parse(xml))
    } as ExitCode.Success
  }
}
