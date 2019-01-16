package com.bot.cbr.algebra

import cats.data.EitherNec
import com.bot.cbr.domain.{CBRError, MoexCurrency, MoexCurrencyType}
import fs2.Stream

trait MoexCurrencyService[F[_]] {

  def getCurrencies(moexCurType: MoexCurrencyType): Stream[F, EitherNec[CBRError, MoexCurrency]]
}
