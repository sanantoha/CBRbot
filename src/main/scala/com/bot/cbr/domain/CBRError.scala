package com.bot.cbr.domain

import cats.Show

sealed abstract class CBRError(val msg: String) extends Throwable(msg) with Product with Serializable

object CBRError {

  implicit val cbrError: Show[CBRError] = Show.fromToString[CBRError]

  final case class WrongUrl(override val msg: String) extends CBRError(msg)
  final case class WrongDateFormat(override val msg: String) extends CBRError(msg)
  final case class WrongXMLFormat(override val msg: String) extends CBRError(msg)
  final case class WrongCommandInstruction(override val msg: String) extends CBRError(msg)
}


