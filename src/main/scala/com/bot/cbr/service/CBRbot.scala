package com.bot.cbr.service

import java.time.LocalDate

import cats.instances.string._
import cats.syntax.eq._
import com.bot.cbr.algebra.{BotService, CurrencyService}
import com.bot.cbr.domain.{Currency, CurrencyRequest}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import com.bot.cbr.codec.syntax._
import com.bot.cbr.domain.CurrencyRequest._

class CBRbot[F[_]](botService: BotService[F], currencyService: CurrencyService[F], logger: Logger[F]) {

  import CBRbot._

  def launch: Stream[F, Unit] = pollCommands.flatMap {
    case (chatId, message) => handleRawMessage(chatId, message)
  }

  def pollCommands: Stream[F, (Long, String)] = for {
    update <- botService.pollUpdates(0)

    opt = for {
      botMessage <- update.message
      msg <- botMessage.text
    } yield (botMessage.chat.id, msg)

    (chatId, message) <- Stream.emits(opt.toSeq)

  } yield (chatId, message)

  def handleRawMessage(chatId: Long, message: String): Stream[F, Unit] = message match {
    case `start` | `help` => showHelp(chatId)
    case `currency` => showCurrency(chatId, message + " all today")
    case msg if msg.startsWith(currency) => showCurrency(chatId, msg)
    case msg => handleUnknown(chatId, msg)
  }

  def handleUnknown(chatId: Long, msg: String): Stream[F, Unit] =
    Stream.eval(logger.error(s"Command unknown for chatId=$chatId message=$msg"))

  def showHelp(chatId: Long): Stream[F, Unit] =
    botService.sendMessage(chatId,
      "This bot shows currencies:\n" +
        s"`$help` - show this help message\n" +
        s"`$currency` usd - show usd currency\n" +
        s"`$currency` eur 06.11.2018 - show eur currency on the 6th of November in 2018 year\n" +
        s"`$currency` - show all currencies on today\n" +
        s"`$currency` all - show all currencies on today\n" +
        s"`$currency` all 2018-11-06 - show all currencies on the 6th of November in 2018 year\n"
    )

  def showCurrency(chatId: Long, message: String): Stream[F, Unit] = {

    val currencyRequest: Stream[F, CurrencyRequest] = message.decode[CurrencyRequest] match {
      case Right(cr) => Stream.emit(cr).covary[F]
      case Left(e) => Stream.eval(logger.error(s"Error parsing command: $e")).drain ++ Stream.empty
    }

    val currencies: Stream[F, Currency] = for {
      CurrencyRequest(currency, date) <- currencyRequest
      _ <- Stream.eval(logger.info(s"showCurrency($chatId, $currency, $date) invokes"))
      eiCur <- currencyService.requestCurrencies(date)
      cur <- eiCur match {
        case Right(cur) => Stream.emit(cur).covary[F]
        case Left(e) => Stream.eval(logger.error(e)(s"Error: ${e.getMessage}")).drain ++ Stream.empty
      }
    } yield cur

    for {
      CurrencyRequest(currency, date) <- currencyRequest
      msg <- if (currency === "all") {
        currencies.fold("")((acc, cur) => acc + prettyString(cur, date) + "\n")
      } else {
        currencies.find(_.chCode.toUpperCase === currency.toUpperCase).map(prettyString(_, date))
      }
      _ <- botService.sendMessage(chatId, msg)
    } yield ()
  }
}

object CBRbot {

  def prettyString(cur: Currency, date: LocalDate): String =
    s"стоимость ${cur.nom} ${cur.name} на $date составляет ${cur.curs}"
}