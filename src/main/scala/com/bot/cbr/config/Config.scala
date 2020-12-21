package com.bot.cbr.config

import cats.effect.{Blocker, ContextShift, Sync}
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import pureconfig.generic.auto._

final case class MoexCurrencyUrlConfig(urlUsd: String, urlEur: String)

final case class Config(urlBotapi: String,
                        urlCurrency: String,
                        urlMoexCurrency: String,
                        urlMetal: String,
                        moexCurUrlConfig: MoexCurrencyUrlConfig)

object Config {

  def load[F[_]: Sync: ContextShift](blocker: Blocker): F[Config] =
    ConfigSource.default.at("cbr").loadF[F, Config](blocker)
}
