package com.bot.cbr.domain

import java.time.LocalDate

import cats.data.EitherNec
import com.bot.cbr.codec.Decoder
import com.bot.cbr.domain.CBRError.WrongCommandInstruction
import cats.syntax.either._

final case class CurrencyRequest(name: String, date: LocalDate)

object CurrencyRequest {

  implicit val currencyDecoder: Decoder[CurrencyRequest] = new Decoder[CurrencyRequest] {
    override def decode(s: String): EitherNec[CBRError, CurrencyRequest] =
      s.replaceAll(currencyMsg, "").trim.split(" ").map(_.trim) match {
        case Array(cur, date) => CurrencyRequest(cur, parseDate(date)).rightNec[CBRError]
        case Array(cur) if !cur.isEmpty => CurrencyRequest(cur, LocalDate.now).rightNec[CBRError]
        case msg => WrongCommandInstruction(s"Could not parse currency ${msg.mkString(",")}").leftNec[CurrencyRequest]
      }
  }
}
