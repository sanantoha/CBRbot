package com.bot.cbr.algebra

import java.time.LocalDate

import com.bot.cbr.domain.Metal
import fs2.Stream

trait MetalService1[F[_], G[_]] {

  def getMetals(start: LocalDate, end: LocalDate): Stream[F, G[Metal]]

}