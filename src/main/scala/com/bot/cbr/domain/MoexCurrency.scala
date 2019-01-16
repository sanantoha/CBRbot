package com.bot.cbr.domain

import java.time.LocalDate

import cats.Show

sealed trait MoexCurrencyType extends Product with Serializable

object MoexCurrencyType {

  final case object USD extends MoexCurrencyType
  final case object EUR extends MoexCurrencyType
}

final case class MoexCurrency(curType: MoexCurrencyType, date: LocalDate, value: BigDecimal, change: BigDecimal)

object MoexCurrency {

  implicit val moexCurrencyShow: Show[MoexCurrency] = Show.fromToString[MoexCurrency]
}
