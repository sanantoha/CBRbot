package com.bot.cbr.domain

import java.time.LocalDate

import cats.data.EitherNec
import com.bot.cbr.codec.Decoder
import com.bot.cbr.domain.CBRError.WrongCommandInstruction
import cats.syntax.either._

final case class MoexCurrencyRequest(name: String, date: LocalDate)

object MoexCurrencyRequest {

  implicit val moexCurrencyDecoder: Decoder[MoexCurrencyRequest] = new Decoder[MoexCurrencyRequest] {
    override def decode(s: String): EitherNec[CBRError, MoexCurrencyRequest] = {
      s.replaceAll(moexCurMsg, "").trim.split(" ").map(_.trim) match {
        case Array(cur, date) => MoexCurrencyRequest(cur, parseDate(date)).rightNec[CBRError]
        case Array(cur) if !cur.isEmpty => MoexCurrencyRequest(cur, LocalDate.now).rightNec[CBRError]
        case arr => WrongCommandInstruction(s"Could not parse moex currency ${arr.mkString(",")}").leftNec[MoexCurrencyRequest]
      }
    }
  }
}
