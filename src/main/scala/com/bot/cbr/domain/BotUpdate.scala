package com.bot.cbr.domain

import cats.Show

final case class BotUpdate(update_id: Long, message: Option[BotMessage])

object BotUpdate {
  implicit val botUpdateShow: Show[BotUpdate] = Show.fromToString[BotUpdate]
}
