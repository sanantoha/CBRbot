package com.bot.cbr.domain

import cats.Show

sealed trait CBRError extends Throwable with Product with Serializable

object CBRError {

  implicit val cbrError: Show[CBRError] = Show.fromToString[CBRError]

  final case class WrongUrl(msg: String) extends CBRError
  final case class WrongXMLFormat(msg: String) extends CBRError
  final case class WrongCommandInstruction(msg: String) extends CBRError
}


