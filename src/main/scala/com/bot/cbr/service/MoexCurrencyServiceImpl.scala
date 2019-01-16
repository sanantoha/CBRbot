package com.bot.cbr.service

import cats.data.EitherNec
import cats.effect.Sync
import com.bot.cbr.algebra.MoexCurrencyService
import com.bot.cbr.config.Config
import com.bot.cbr.domain.CBRError.WrongUrl
import com.bot.cbr.domain.{CBRError, MoexCurrency, MoexCurrencyType}
import io.chrisdavenport.log4cats.Logger
import fs2.Stream
import org.http4s.Uri
import cats.syntax.either._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import org.http4s.client.Client

class MoexCurrencyServiceImpl[F[_]: Sync](config: Config, client: Client[F], logger: Logger[F]) extends MoexCurrencyService[F] {

  def url(moexCurType: MoexCurrencyType): F[Uri] = {
    val base = moexCurType match {
      case MoexCurrencyType.USD => config.moexCurUrlConfig.urlUsd
      case MoexCurrencyType.EUR => config.moexCurUrlConfig.urlEur
    }

    Uri.fromString(base).leftMap(p => WrongUrl(p.message): Throwable).raiseOrPure[F]
  }


  override def getCurrencies(moexCurType: MoexCurrencyType): Stream[F, EitherNec[CBRError, MoexCurrency]] = for {
    uri <- Stream.eval(url(moexCurType))
    s <- Stream.eval(client.expect[String](uri))

    browser = JsoupBrowser()
    eiDoc <- Stream.eval(Sync[F].delay(browser.parseString(s))).attempt
  } yield ???

}
