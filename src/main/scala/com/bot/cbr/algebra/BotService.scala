package com.bot.cbr.algebra

import com.bot.cbr.domain.BotUpdate
import fs2.Stream

trait BotService[F[_]] {

  def sendMessage(chatId: Long, message: String): Stream[F, Unit]

  def pollUpdates(fromOffset: Long): Stream[F, BotUpdate]
}