package com.bot.cbr.domain

import java.time.LocalDateTime

import cats.Show

final case class MoexIndicCurrency(exchangeType: String, dateTime: LocalDateTime, curs: BigDecimal)

object MoexIndicCurrency {

  implicit val moexIndicCurrencyShow: Show[MoexIndicCurrency] = Show.fromToString[MoexIndicCurrency]
}
