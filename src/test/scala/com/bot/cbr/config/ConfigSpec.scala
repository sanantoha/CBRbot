package com.bot.cbr.config

import java.util.concurrent.Executors

import cats.effect.{Blocker, ContextShift, IO}
import com.bot.cbr.UnitSpec
import doobie.util.ExecutionContexts

import scala.concurrent.ExecutionContext


class ConfigSpec extends UnitSpec {

  import Config._

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  "CBRConfig " should "read config" in {
    val ioRes: IO[Config] =
        ExecutionContexts.fixedThreadPool[IO](1).use(c => load[IO](Blocker.liftExecutionContext(c)))

    ioRes.unsafeRunSync() shouldBe Config(
      urlBotapi = "https://URL_BOT/botTOKEN_VALUE",
      urlCurrency = "URL_CURRENCY",
      urlMoexCurrency = "URL_MOEX_CURRENCY",
      urlMetal = "URL_METAL",
      moexCurUrlConfig = MoexCurrencyUrlConfig(urlUsd = "URL_MOEX_USD", urlEur = "URL_MOEX_EUR")
    )
  }
}
