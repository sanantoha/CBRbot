package com.bot.cbr.service

import java.time.LocalDate
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
import scalacache._
import com.bot.cbr.cache.CurrencyCache._
import com.bot.cbr.cache.MetalCache._
import cats.syntax.option._
import scalacache.CatsEffect.modes._


class CBRbot[F[_]: cats.effect.Async](botService: BotService[F],
                         currencyService: CurrencyService[F],
                         metalService: MetalService[F],
                         logger: Logger[F]) {

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
        s"`$currencyMsg` all 2018-11-06 - show all currencies on the 6th of November in 2018 year\n" +
        s"`$metalMsg` gold - show gold on today\n" +
        s"`$metalMsg` all - show all metals on today\n" +
        s"`$metalMsg` 2018-11-06 2018-11-08 - show all metals on 6, 7 and 8 of November\n"
    )

  def showMetal(chatId: Long, message: String): Stream[F, Unit] = {
    val metalRequest: Stream[F, MetalRequest] = decode[MetalRequest](message)

    val metals: Stream[F, Metal] = for {
      MetalRequest(name, startDate, endDate) <- metalRequest
      _ <- Stream.eval(logger.info(s"invoke showMetal($chatId, $name, $startDate, $endDate)"))
      vecEiMetal <- Stream.eval(cachingF(startDate, endDate)(none)(metalService.getMetals(startDate, endDate).compile.toVector))
      eiMetal <- Stream.emits(vecEiMetal).covary[F]
      metal <- eiMetal match {
        case Right(met) => Stream.emit(met).covary[F]
        case Left(nec) => Stream.eval(logger.error(s"Errors: ${nec.toChain.toList.mkString("\n")}")).drain
      }
    } yield metal


    for {
      MetalRequest(name, _, _) <- metalRequest
      msg <- if (name.toLowerCase === "all") {
        foldToMsgRes(metals)
      } else {
        foldToMsgRes(metals.filter(_.metalType.toString.toLowerCase === name))
      }
      _ <- botService.sendMessage(chatId, msg)
    } yield ()
  }

  def showCurrency(chatId: Long, message: String): Stream[F, Unit] = {
    val currencyRequest: Stream[F, CurrencyRequest] = decode[CurrencyRequest](message)

    val currencies: Stream[F, Currency] = for {
      CurrencyRequest(currency, date) <- currencyRequest
      _ <- Stream.eval(logger.info(s"invoke showCurrency($chatId, $currency, $date)"))
      vecEiCur <- Stream.eval(cachingF(date)(none)(currencyService.getCurrencies(date).compile.toVector))
      eiCur <- Stream.emits(vecEiCur).covary[F]
      cur <- eiCur match {
        case Right(cur) => Stream.emit(cur).covary[F]
        case Left(nec) => Stream.eval(logger.error(s"Errors: ${nec.toChain.toList.mkString("\n")}")).drain
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

  private def foldToMsgRes(metals: Stream[F, Metal]) = {
    metals.fold("")((acc, met) => acc + prettyStringMetal(met) + "\n")
  }
}

object CBRbot {

  def prettyStringMetal(metal: Metal): String = {
    s"стоимость ${metal.metalType} на ${metal.date} покупка ${metal.buy}, продажа ${metal.sell}"
  }

  def prettyStringCurrency(cur: Currency, date: LocalDate): String =
    s"стоимость ${cur.nom} ${cur.name} на $date составляет ${cur.curs}"
}