package com.bot.cbr.domain

import java.time.LocalDate

import cats.Show

sealed trait MetalType extends Product with Serializable

object MetalType {
  final case object Gold extends MetalType
  final case object Silver extends MetalType
  final case object Platinum extends MetalType
  final case object Palladium extends MetalType

  val lookUpMetalType = Map(
    1 -> Gold,
    2 -> Silver,
    3 -> Platinum,
    4 -> Palladium
  )
}


case class Metal(metalType: MetalType, date: LocalDate, buy: BigDecimal, sell: BigDecimal)

object Metal {

  implicit val metalShow: Show[Metal] = Show.fromToString[Metal]

}