package com.bot.cbr.service

import java.time.LocalDate

import cats.effect.SyncIO
import com.bot.cbr.UnitSpec
import com.bot.cbr.algebra.{BotService, CurrencyService}
import com.bot.cbr.domain.{BotUpdate, CBRError, Currency}
import io.chrisdavenport.log4cats.noop.NoOpLogger
import fs2.Stream
import cats.syntax.option._

class CBRbotSpec extends UnitSpec {

  val ld = LocalDate.of(2018, 10, 12)
  val ldOpt = ld.some

  val logger = NoOpLogger.impl[SyncIO]

  val botService = new BotService[SyncIO] {
    override def sendMessage(chatId: Long, message: String): Stream[SyncIO, Unit] = Stream.empty

    override def pollUpdates(fromOffset: Long): Stream[SyncIO, BotUpdate] = Stream.empty
  }

  val currencyService = new CurrencyService[SyncIO] {
    override def requestCurrencies(date: LocalDate): Stream[SyncIO, Either[CBRError, Currency]] = Stream.empty
  }

  val cbtBot = new CBRbot[SyncIO](botService, currencyService, logger)

  // some test should be here
}
