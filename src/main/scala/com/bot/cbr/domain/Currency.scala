package com.bot.cbr.domain

import cats.Show

final case class Currency(name: String, nom: BigDecimal, curs: BigDecimal, code: Int, chCode: String)

object Currency {

  implicit val currencyShow: Show[Currency] = Show.fromToString[Currency]

}