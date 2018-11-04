package com.bot.cbr.domain

final case class BotUpdate(update_id: Long, message: Option[BotMessage])
