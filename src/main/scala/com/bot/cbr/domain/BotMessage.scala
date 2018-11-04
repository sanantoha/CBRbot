package com.bot.cbr.domain

final case class BotMessage(message_id: Long,
                            chat: Chat,
                            text: Option[String])
