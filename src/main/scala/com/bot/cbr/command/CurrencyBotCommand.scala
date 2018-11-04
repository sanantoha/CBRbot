package com.bot.cbr.command

sealed trait CurrencyBotCommand

object CurrencyBotCommand {

  final case class ShowHelp(chatId: Long) extends CurrencyBotCommand
  final case class ShowAllCurrency(chatId: Long) extends CurrencyBotCommand
  final case class ShowCurrency(chatId: Long, currency: String) extends CurrencyBotCommand
  final case class Unknown(chatId: Long) extends CurrencyBotCommand

  def fromRawMessage(chatId: Long, message: String): CurrencyBotCommand = message match {
    case `start` | `help` => ShowHelp(chatId)
    case `currency` => ShowAllCurrency(chatId)
    case cur if cur.startsWith(currency) => ShowCurrency(chatId, cur.replaceAll(currency, "").trim)
    case _ => Unknown(chatId)
  }

  val help = "?"
  val start = "/start"
  val currency = "/currency"
}
