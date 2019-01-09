package com.bot.cbr.domain

import java.time.LocalDateTime

import cats.Show

final case class MoexCurrency(exchangeType: String, dateTime: LocalDateTime, curs: BigDecimal)

object MoexCurrency {

  implicit val currencyShow: Show[MoexCurrency] = Show.fromToString[MoexCurrency]
}
