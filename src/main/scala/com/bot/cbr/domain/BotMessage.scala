package com.bot.cbr.domain

import cats.Show

final case class BotMessage(message_id: Long,
                            chat: Chat,
                            text: Option[String])

object BotMessage {
  implicit val botMessageShow: Show[BotMessage] = Show.fromToString[BotMessage]
}
