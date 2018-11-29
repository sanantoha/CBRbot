package com.bot.cbr.service

import java.text.DecimalFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import cats.{ApplicativeError, Apply}
import cats.data.NonEmptyChain
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import com.bot.cbr.domain.{CBRError, Metal, MetalType}
import com.bot.cbr.domain.MetalType.lookUpMetalType
import cats.syntax.apply._
import cats.syntax.either._
import com.bot.cbr.algebra.MetalParser

import scala.xml.Node

class MetalParserImpl[F[_]: Apply: ApplicativeError[?[_], NonEmptyChain[CBRError]]]() extends MetalParser[F] {

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
      .leftMap(e => NonEmptyChain.one(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure

  def parseDate(record: Node): F[LocalDate] =
    Either.catchNonFatal {
      LocalDate.parse((record \ "@Date").text, dateFormatMetal)
    }.leftMap(e => NonEmptyChain.one(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure

  def parseBigDecimal(record: Node, name: String): F[BigDecimal] =
    Either.catchNonFatal {
      val df = new DecimalFormat()
      BigDecimal(df.parse((record \\ name).text).doubleValue())
////      println(df)
//      BigDecimal((record \\ name).text)
    }.leftMap(e => NonEmptyChain.one(WrongXMLFormat(e.getMessage): CBRError)).raiseOrPure
}
