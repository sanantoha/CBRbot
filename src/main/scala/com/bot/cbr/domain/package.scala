package com.bot.cbr

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

import cats.syntax.either._
import cats.syntax.semigroupk._
import cats.instances.option._
import domain.date._


package object domain {

  val helpMsg = "?"
  val startMsg = "/start"
  val currencyMsg = "/currency"

  val metalMsg = "/metal"
  val moexCurMsg = "/moex"

  object date {
    val dateFormatISO = DateTimeFormatter.ISO_DATE
    val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    val dateFormatSlash = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    val dateFormatHyphen = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val dateFormatShort = DateTimeFormatter.ofPattern("dd.MM.yy")

    val dateTimeWhiteSpaceDelimiter: DateTimeFormatter = new DateTimeFormatterBuilder()
      .parseCaseInsensitive.append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral(' ').append(DateTimeFormatter.ISO_LOCAL_TIME)
      .toFormatter
  }

  def parseDate(str: String): LocalDate = str match {
    case "today" => LocalDate.now
    case "tomorrow" => LocalDate.now.plusDays(1)
    case "yesterday" => LocalDate.now.minusDays(1)
    case x => (
      parse(x, dateFormatISO) <+> parse(x, dateFormat) <+>
        parse(x, dateFormatSlash) <+> parse(x, dateFormatHyphen)
      ).getOrElse(LocalDate.now())
  }

  private def parse(str: String, formatter: DateTimeFormatter): Option[LocalDate] =
    Either.catchNonFatal(LocalDate.parse(str, formatter)).toOption
}