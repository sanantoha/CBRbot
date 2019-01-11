package com.bot.cbr.algebra

import java.time.LocalDate

import cats.data.EitherNec
import com.bot.cbr.domain.{CBRError, Metal}
import fs2.Stream

trait MetalService[F[_]] {

  def getMetals(start: LocalDate, end: LocalDate): Stream[F, EitherNec[CBRError, Metal]]
}