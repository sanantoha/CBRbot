package com.bot.cbr.domain

import cats.Show

import scala.util.control.NoStackTrace

sealed abstract class CBRError(val msg: String) extends Exception(msg) with NoStackTrace

object CBRError {

  implicit val cbrError: Show[CBRError] = Show.fromToString[CBRError]

  final case class WrongUrl(override val msg: String) extends CBRError(msg)
  final case class WrongDateFormat(override val msg: String) extends CBRError(msg)
  final case class WrongXMLFormat(override val msg: String) extends CBRError(msg)
  final case class WrongCommandInstruction(override val msg: String) extends CBRError(msg)
  final case class WrongMetalData(override val msg: String) extends CBRError(msg)
}


