package com.bot.cbr.domain

final case class BotResponse[T](ok: Boolean, result: T)
