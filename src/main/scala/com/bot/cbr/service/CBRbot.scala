package com.bot.cbr.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.instances.option._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.semigroupk._
import com.bot.cbr.algebra.{BotService, CurrencyService}
import com.bot.cbr.domain.CBRError.WrongCommandInstruction
import com.bot.cbr.domain.{CBRError, Currency}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger

class CBRbot[F[_]](botService: BotService[F], currencyService: CurrencyService[F], logger: Logger[F]) {

  val help = "?"
  val start = "/start"
  val currency = "/currency"

  val dateFormatISO = DateTimeFormatter.ISO_DATE
  val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  val dateFormatDash = DateTimeFormatter.ofPattern("dd/MM/yyy")
  val dateFormatSlash = DateTimeFormatter.ofPattern("dd-MM-yyyy")


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
    case `currency` => showAllCurrency(chatId)
    case curInst if curInst.startsWith(currency) => showCurrency(chatId, message)
    case msg => handleUnknown(chatId, msg)
  }

  def handleUnknown(chatId: Long, msg: String): Stream[F, Unit] =
    Stream.eval(logger.error(s"Command unknown for chatId=$chatId message=$msg"))

  def showHelp(chatId: Long): Stream[F, Unit] =
    botService.sendMessage(chatId,
      "This bot stores your show currencies:\n" +
        s"`$help` - show this help message\n" +
        s"`$currency` usd - show usd currency\n" +
        s"`$currency` eur 06.11.2018 - show eur currency on the 10th of November in 2018 year\n" +
        s"`$currency` - show all currencies"
    )

  def showAllCurrency(chatId: Long): Stream[F, Unit] = {
     val currencies: Stream[F, Currency] = for {
      _ <- Stream.eval(logger.info("showAllCurrency invokes"))
      eiCur <- currencyService.requestCurrencies(LocalDate.now())
      cur <- eiCur match {
        case Right(cur) => Stream.emit(cur).covary[F]
        case Left(e) =>
          Stream.eval(logger.error(e)(s"Error: ${e.getMessage}")).drain ++ Stream.empty
      }
    } yield cur

    for {
      msg <- currencies.fold("")((acc, cur) => acc + prettyString(cur) + "\n")
      _ <- botService.sendMessage(chatId, msg)
    } yield ()
  }

  def showCurrency(chatId: Long, message: String): Stream[F, Unit] = {
    val currencyStream: Stream[F, (String, LocalDate)] = parseCurrency(message) match {
      case Right((cur, date)) => Stream.emit((cur, date)).covary[F]
      case Left(e) => Stream.eval(logger.error(s"Error parsing command: $e")).drain ++ Stream.empty
    }

    for {
      (currency, date) <- currencyStream
      _ <- Stream.eval(logger.info(s"showCurrency($chatId, $currency, $date) invokes"))
      eiCur <- currencyService.requestCurrencies(date)
      cur <- eiCur match {
        case Right(cur) => Stream.emit(cur).covary[F]
        case Left(e) => Stream.eval(logger.error(e)(s"Error: ${e.getMessage}")).drain ++ Stream.empty
      }
      _ <- if (cur.chCode.toUpperCase === currency.toUpperCase) botService.sendMessage(chatId, prettyString(cur))
      else Stream.empty
    } yield ()
  }

  def prettyString(cur: Currency): String =
    s"${cur.name} for ${cur.nom} get ${cur.curs}"

  def parseCurrency(message: String): Either[CBRError, (String, LocalDate)] = {
    message.replaceAll(currency, "").trim.split(" ").map(_.trim) match {
      case Array(cur, date) => (cur, parseDate(date)).asRight[CBRError]
      case Array(cur) if !cur.isEmpty => (cur, LocalDate.now).asRight[CBRError]
      case msg => WrongCommandInstruction(s"Could not parse currency ${msg.mkString(",")}").asLeft[(String, LocalDate)]
    }
  }

  def parseDate(str: String): LocalDate = str match {
    case "today" => LocalDate.now
    case "tomorrow" => LocalDate.now.plusDays(1)
    case "yesterday" => LocalDate.now.minusDays(1)
    case x => (
      parse(x, dateFormatISO) <+> parse(x, dateFormat) <+>
        parse(x, dateFormatDash) <+> parse(x, dateFormatSlash)
      ).getOrElse(LocalDate.now())
  }

  def parse(str: String, formatter: DateTimeFormatter): Option[LocalDate] =
    Either.catchNonFatal(LocalDate.parse(str, formatter)).toOption
}
