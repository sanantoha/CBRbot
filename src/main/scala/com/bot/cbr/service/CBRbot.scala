package com.bot.cbr.service

import java.time.LocalDate

import cats.{ApplicativeError, Traverse}
import cats.data.NonEmptyChain
import cats.effect.Sync
import cats.instances.string._
import cats.syntax.eq._
import com.bot.cbr.algebra.{BotService, CurrencyService, MetalService}
import com.bot.cbr.codec.Decoder
import com.bot.cbr.domain.{Currency, CurrencyRequest}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import com.bot.cbr.codec.syntax._
import com.bot.cbr.domain._
import com.bot.cbr.domain.CurrencyRequest._
import cats.syntax.traverse._
import cats.syntax.applicative._


class CBRbot[F[_],
             G[_]](botService: BotService[F],
                   currencyService: CurrencyService[F],
                   metalService: MetalService[F, G],
                   logger: Logger[F])
                  (implicit val F: Sync[F], T: Traverse[G], AE: ApplicativeError[G, NonEmptyChain[CBRError]]) {

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
    case `startMsg` | `helpMsg` => showHelp(chatId)
    case `currencyMsg` => showCurrency(chatId, message + " all today")
    case `metalMsg` => showMetal(chatId, message + "all today")
    case msg if msg.startsWith(currencyMsg) => showCurrency(chatId, msg)
    case msg if msg.startsWith(metalMsg) => showMetal(chatId, msg)
    case msg => handleUnknown(chatId, msg)
  }

  def handleUnknown(chatId: Long, msg: String): Stream[F, Unit] =
    Stream.eval(logger.error(s"Command unknown for chatId=$chatId message=$msg"))

  def showHelp(chatId: Long): Stream[F, Unit] =
    botService.sendMessage(chatId,
      "This bot shows currencies:\n" +
        s"`$helpMsg` - show this help message\n" +
        s"`$currencyMsg` usd - show usd currency\n" +
        s"`$currencyMsg` eur 06.11.2018 - show eur currency on the 6th of November in 2018 year\n" +
        s"`$currencyMsg` - show all currencies on today\n" +
        s"`$currencyMsg` all - show all currencies on today\n" +
        s"`$currencyMsg` all 2018-11-06 - show all currencies on the 6th of November in 2018 year\n"
    )

  def showMetal(chatId: Long, message: String): Stream[F, Unit] = {
    val metalRequest: Stream[F, MetalRequest] = decode[MetalRequest](message)

    val metals = for {
      MetalRequest(name, startDate, endDate) <- metalRequest
      _ <- Stream.eval(logger.info(s"showMetal($chatId, $name, $startDate, $endDate)"))
      gMetal <- metalService.getMetals(startDate, endDate)
    } yield gMetal


    for {
      MetalRequest(name, _, _) <- metalRequest
      msg <- if (name.toLowerCase === "all") {
        metals.fold("".pure[G])((acc, gm) => AE.map2(acc, T.map(gm)(prettyStringMetal))(_ + "\n" + _))
      } else {
        metals.map(gm => T.find(gm)(_.metalType.toString.toLowerCase === name).map(prettyStringMetal).fold("".pure[G])(_.pure[G]))
      }
      _ <- Stream.eval(msg.traverse(x => if (x.isEmpty) F.unit else botService.sendMessage(chatId, x).compile.drain))
    } yield ()
  }

  def showCurrency(chatId: Long, message: String): Stream[F, Unit] = {
    val currencyRequest: Stream[F, CurrencyRequest] = decode[CurrencyRequest](message)

    val currencies: Stream[F, Currency] = for {
      CurrencyRequest(currency, date) <- currencyRequest
      _ <- Stream.eval(logger.info(s"showCurrency($chatId, $currency, $date) invokes"))
      eiCur <- currencyService.getCurrencies(date)
      cur <- eiCur match {
        case Right(cur) => Stream.emit(cur).covary[F]
        case Left(e) => Stream.eval(logger.error(e)(s"Error: ${e.getMessage}")).drain
      }
    } yield cur

    for {
      CurrencyRequest(currency, date) <- currencyRequest
      msg <- if (currency.toLowerCase === "all") {
        currencies.fold("")((acc, cur) => acc + prettyStringCurrency(cur, date) + "\n")
      } else {
        currencies.find(_.chCode.toLowerCase === currency.toLowerCase).map(prettyStringCurrency(_, date))
      }
      _ <- botService.sendMessage(chatId, msg)
    } yield ()
  }

  def decode[A: Decoder](message: String): Stream[F, A] =
    message.decode[A] match {
      case Right(cr) => Stream.emit(cr).covary[F]
      case Left(nec) =>
        Stream.emits(nec.toChain.toList).evalMap(e => logger.error(s"Error parsing command: $e")).drain
    }
}

object CBRbot {

  def prettyStringMetal(metal: Metal): String = {
    s"стоимость ${metal.metalType} на ${metal.date} покупка ${metal.buy}, продажа ${metal.sell}"
  }

  def prettyStringCurrency(cur: Currency, date: LocalDate): String =
    s"стоимость ${cur.nom} ${cur.name} на $date составляет ${cur.curs}"
}