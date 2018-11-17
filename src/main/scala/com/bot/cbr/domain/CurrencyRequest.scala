package com.bot.cbr.domain

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.bot.cbr.codec.CurrencyDecoder
import com.bot.cbr.domain.CBRError.WrongCommandInstruction
import cats.syntax.either._
import cats.syntax.semigroupk._
import cats.instances.option._

case class CurrencyRequest(name: String, date: LocalDate)

object CurrencyRequest {

  val help = "?"
  val start = "/start"
  val currency = "/currency"

  val dateFormatISO = DateTimeFormatter.ISO_DATE
  val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  val dateFormatSlash = DateTimeFormatter.ofPattern("dd/MM/yyy")
  val dateFormatHyphen = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  implicit val currencyDecoder: CurrencyDecoder[CurrencyRequest] = new CurrencyDecoder[CurrencyRequest] {
    override def decode(s: String): Either[CBRError, CurrencyRequest] =
      s.replaceAll(currency, "").trim.split(" ").map(_.trim) match {
        case Array(cur, date) => CurrencyRequest(cur, parseDate(date)).asRight[CBRError]
        case Array(cur) if !cur.isEmpty => CurrencyRequest(cur, LocalDate.now).asRight[CBRError]
        case msg => WrongCommandInstruction(s"Could not parse currency ${msg.mkString(",")}").asLeft[CurrencyRequest]
      }
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

  def parse(str: String, formatter: DateTimeFormatter): Option[LocalDate] =
    Either.catchNonFatal(LocalDate.parse(str, formatter)).toOption
}
