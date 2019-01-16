package com.bot.cbr.config

import cats.effect.{ContextShift, Sync}
import io.chrisdavenport.linebacker.Linebacker
import pureconfig.module.catseffect._

final case class MoexCurrencyUrlConfig(urlUsd: String, urlEur: String)

final case class Config(urlBotapi: String,
                        urlCurrency: String,
                        urlMoexCurrency: String,
                        urlMetal: String,
                        moexCurUrlConfig: MoexCurrencyUrlConfig)

object Config {

  def load[F[_]: Sync: ContextShift](implicit linebacker: Linebacker[F]): F[Config] =
    linebacker.blockCS(loadConfigF[F, Config]("cbr"))
}
