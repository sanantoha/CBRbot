package com.bot.cbr.service

import java.time.LocalDate

import cats.effect.SyncIO
import com.bot.cbr.UnitSpec
import com.bot.cbr.algebra.{BotService, CurrencyService}
import com.bot.cbr.domain.{BotUpdate, CBRError, Currency}
import io.chrisdavenport.log4cats.noop.NoOpLogger
import fs2.Stream
import cats.syntax.option._
import cats.syntax.either._
import com.bot.cbr.domain.CBRError.WrongCommandInstruction

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

  "parseCurrency" should "take currency instruction string and parse it" in {
    cbtBot.parseCurrency("/currency usd 12.10.2018") shouldBe ("usd", ld).asRight[CBRError]
  }

  it should "take empty currency instruction string and parse it" in {
    cbtBot.parseCurrency("") shouldBe WrongCommandInstruction("Could not parse currency ").asLeft[(String, LocalDate)]
  }

  it should "take currency without date" in {
    cbtBot.parseCurrency("/currency eur") shouldBe ("eur", LocalDate.now).asRight[CBRError]
  }

  "parse" should "take date format 12.10.2018 and parse it" in {
    cbtBot.parse("12.10.2018", cbtBot.dateFormat) shouldBe ldOpt
  }

  it should "take date format 2018-10-12 and parse it" in {
    cbtBot.parse("2018-10-12", cbtBot.dateFormatISO) shouldBe ldOpt
  }

  it should "take date format 12/10/2018 and parse it" in {
    cbtBot.parse("12/10/2018", cbtBot.dateFormatDash) shouldBe ldOpt
  }

  it should "take date format and parse it" in {
    cbtBot.parse("12-10-2018", cbtBot.dateFormatSlash) shouldBe ldOpt
  }

  "parseDate" should "take today and parse it" in {
    cbtBot.parseDate("today") shouldBe LocalDate.now
  }

  it should "take yesterday and parse it" in {
    cbtBot.parseDate("yesterday") shouldBe LocalDate.now.minusDays(1)
  }

  it should "take tomorrow and parse it" in {
    cbtBot.parseDate("tomorrow") shouldBe LocalDate.now.plusDays(1)
  }


}
