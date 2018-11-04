package com.bot.cbr.config

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}
import com.bot.cbr.UnitSpec
import io.chrisdavenport.linebacker.contexts.{Executors => E}
import cats.syntax.functor._
import io.chrisdavenport.linebacker.Linebacker

import scala.concurrent.ExecutionContext


class ConfigSpec extends UnitSpec {

  import Config._

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  "CBRConfig " should "read config" in {
    val ioRes: IO[Config] = E.unbound[IO].map(Linebacker.fromExecutorService[IO]).use { implicit linebacker =>
      implicit val _: ContextShift[IO] = IO.contextShift(testEc)
      load[IO]
    }

    ioRes.unsafeRunSync() shouldBe Config(urlBotapi = "https://URL_BOT/botTOKEN_VALUE", urlCurrency = "URL_CURRENCY")
  }
}
