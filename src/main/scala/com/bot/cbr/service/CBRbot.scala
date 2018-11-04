package com.bot.cbr.service

import java.time.LocalDate

import com.bot.cbr.algebra.{BotService, CurrencyService}
import com.bot.cbr.command.CurrencyBotCommand
import com.bot.cbr.command.CurrencyBotCommand._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import cats.syntax.eq._
import cats.instances.string._
import com.bot.cbr.domain.Currency

class CBRbot[F[_]](botService: BotService[F], currencyService: CurrencyService[F], logger: Logger[F]) {

  def launch: Stream[F, Unit] = pollCommands.flatMap(handleCommand)

  def pollCommands: Stream[F, CurrencyBotCommand] = for {
    update <- botService.pollUpdates(0)

    opt = for {
      botMessage <- update.message
      msg <- botMessage.text
    } yield (botMessage.chat.id, msg)

    (chatId, message) <- Stream.emits(opt.toSeq)

  } yield CurrencyBotCommand.fromRawMessage(chatId, message)


  def handleCommand(command: CurrencyBotCommand): Stream[F, Unit] = command match {
    case ShowHelp(chatId) => showHelp(chatId)
    case ShowCurrency(chatId, cur) => showCurrency(chatId, cur)
    case ShowAllCurrency(chatId) => showAllCurrency(chatId)
    case Unknown(_) => Stream.empty
  }

  def showHelp(chatId: Long): Stream[F, Unit] =
    botService.sendMessage(chatId,
      "This bot stores your show currencies:\n" +
        s"`$help` - show this help message\n" +
        s"`$currency` usd - show usd currency\n" +
        s"`$currency` - show all currencies"
    )

  def showAllCurrency(chatId: Long): Stream[F, Unit] = for {
    _ <- Stream.eval(logger.info("showAllCurrency invokes"))
    eiCur <- currencyService.requestCurrencies(LocalDate.now())
    _ <- eiCur match {
      case Right(cur) => botService.sendMessage(chatId, prettyString(cur))
      case Left(e) => Stream.eval(logger.error(e)(s"Error: ${e.getMessage}"))
    }
  } yield ()


  def showCurrency(chatId: Long, currency: String): Stream[F, Unit] = for {
    _ <- Stream.eval(logger.info(s"showCurrency($currency) invokes"))
    eiCur <- currencyService.requestCurrencies(LocalDate.now())
    cur <- eiCur match {
      case Right(cur) => Stream.emit(cur).covary[F]
      case Left(e) => Stream.eval(logger.error(e)(s"Error: ${e.getMessage}")).drain ++ Stream.empty
    }
    _ <- if (cur.chCode.toUpperCase === currency.toUpperCase) botService.sendMessage(chatId, prettyString(cur))
         else Stream.empty
  } yield ()

  def prettyString(cur: Currency): String =
    s"${cur.name} for ${cur.nom} get ${cur.curs}"
}
