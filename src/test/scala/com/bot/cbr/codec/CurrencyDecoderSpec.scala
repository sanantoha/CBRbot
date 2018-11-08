package com.bot.cbr.codec

import java.time.LocalDate

import com.bot.cbr.UnitSpec
import com.bot.cbr.domain.{CBRError, CurrencyRequest}
import com.bot.cbr.codec.syntax._
import cats.syntax.either._
import com.bot.cbr.domain.CBRError.WrongCommandInstruction

class CurrencyDecoderSpec extends UnitSpec {

  val ld = LocalDate.of(2018, 12, 10)

  "Decoder" should "parse currency request" in {
    "/currency usd 10.12.2018".decode[CurrencyRequest] shouldBe CurrencyRequest("usd", ld).asRight[CBRError]
  }

  it should "parse currency request with different currency" in {
    "/currency eur 10-12-2018".decode[CurrencyRequest] shouldBe CurrencyRequest("eur", ld).asRight[CBRError]
  }

  it should "parse currency request without date" in {
    "/currency usd".decode[CurrencyRequest] shouldBe CurrencyRequest("usd", LocalDate.now).asRight[CBRError]
  }

  it should "parse empty string" in {
    "".decode[CurrencyRequest] shouldBe WrongCommandInstruction("Could not parse currency ").asLeft[CurrencyRequest]
  }

  it should "parse currency request with 10/12/2018" in {
    "/currency all 10/12/2018".decode[CurrencyRequest] shouldBe CurrencyRequest("all", ld).asRight[CBRError]
  }

  it should "parse currency request with 10-12-2018" in {
    "/currency all 10-12-2018".decode[CurrencyRequest] shouldBe CurrencyRequest("all", ld).asRight[CBRError]
  }

  it should "parse currency request with 2018-12-10" in {
    "/currency all 2018-12-10".decode[CurrencyRequest] shouldBe CurrencyRequest("all", ld).asRight[CBRError]
  }

  it should "parse currency request with today" in {
    "/currency all today".decode[CurrencyRequest] shouldBe CurrencyRequest("all", LocalDate.now).asRight[CBRError]
  }

  it should "parse currency request with yesterday" in {
    "/currency all yesterday".decode[CurrencyRequest] shouldBe
      CurrencyRequest("all", LocalDate.now.minusDays(1)).asRight[CBRError]
  }

  it should "parse currency request with tomorrow" in {
    "/currency all tomorrow".decode[CurrencyRequest] shouldBe
      CurrencyRequest("all", LocalDate.now.plusDays(1)).asRight[CBRError]
  }

}
