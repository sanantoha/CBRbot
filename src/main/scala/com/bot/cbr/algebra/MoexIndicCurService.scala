package com.bot.cbr.algebra

import java.time.LocalDate

import cats.data.EitherNec
import com.bot.cbr.domain.{CBRError, MoexIndicCurrency}
import fs2.Stream

trait MoexIndicCurService[F[_]] {

  def getCurrencies(exchangeType: String, date: LocalDate): Stream[F, EitherNec[CBRError, MoexIndicCurrency]]
}
