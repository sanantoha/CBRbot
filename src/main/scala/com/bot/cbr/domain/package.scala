package com.bot.cbr

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import cats.syntax.either._
import cats.syntax.semigroupk._
import cats.instances.option._


package object domain {

  val helpMsg = "?"
  val startMsg = "/start"
  val currencyMsg = "/currency"

  val metalMsg = "/metal"

  val dateFormatISO = DateTimeFormatter.ISO_DATE
  val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  val dateFormatSlash = DateTimeFormatter.ofPattern("dd/MM/yyy")
  val dateFormatHyphen = DateTimeFormatter.ofPattern("dd-MM-yyyy")

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