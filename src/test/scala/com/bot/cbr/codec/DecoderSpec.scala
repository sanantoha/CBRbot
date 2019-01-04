package com.bot.cbr.codec

import java.time.LocalDate

import com.bot.cbr.UnitSpec
import com.bot.cbr.domain.{CBRError, CurrencyRequest, MetalRequest}
import com.bot.cbr.codec.syntax._
import cats.syntax.either._
import com.bot.cbr.domain.CBRError.WrongCommandInstruction

class DecoderSpec extends UnitSpec {

  val ld = LocalDate.of(2018, 12, 10)

  "Decoder" should "parse currency request" in {
    "/currency usd 10.12.2018".decode[CurrencyRequest] shouldBe CurrencyRequest("usd", ld).rightNec[CBRError]
  }

  it should "parse currency request with different currency" in {
    "/currency eur 10-12-2018".decode[CurrencyRequest] shouldBe CurrencyRequest("eur", ld).rightNec[CBRError]
  }

  it should "parse currency request without date" in {
    "/currency usd".decode[CurrencyRequest] shouldBe CurrencyRequest("usd", LocalDate.now).rightNec[CBRError]
  }

  it should "parse empty string" in {
    "".decode[CurrencyRequest] shouldBe WrongCommandInstruction("Could not parse currency ").leftNec[CurrencyRequest]
  }

  it should "parse currency request with 10/12/2018" in {
    "/currency all 10/12/2018".decode[CurrencyRequest] shouldBe CurrencyRequest("all", ld).rightNec[CBRError]
  }

  it should "parse currency request with 10-12-2018" in {
    "/currency all 10-12-2018".decode[CurrencyRequest] shouldBe CurrencyRequest("all", ld).rightNec[CBRError]
  }

  it should "parse currency request with 2018-12-10" in {
    "/currency all 2018-12-10".decode[CurrencyRequest] shouldBe CurrencyRequest("all", ld).rightNec[CBRError]
  }

  it should "parse currency request with today" in {
    "/currency all today".decode[CurrencyRequest] shouldBe CurrencyRequest("all", LocalDate.now).rightNec[CBRError]
  }

  it should "parse currency request with yesterday" in {
    "/currency all yesterday".decode[CurrencyRequest] shouldBe
      CurrencyRequest("all", LocalDate.now.minusDays(1)).rightNec[CBRError]
  }

  it should "parse currency request with tomorrow" in {
    "/currency all tomorrow".decode[CurrencyRequest] shouldBe
      CurrencyRequest("all", LocalDate.now.plusDays(1)).rightNec[CBRError]
  }

  it should "parse metal request on today" in {
    "/metal all today today".decode[MetalRequest] shouldBe MetalRequest("all", LocalDate.now, LocalDate.now).rightNec[CBRError]
  }

  it should "parse metal request on today short version" in {
    "/metal all today".decode[MetalRequest] shouldBe MetalRequest("all", LocalDate.now, LocalDate.now).rightNec[CBRError]
  }

  it should "parse gold metal reques on tomorrow" in {
    val expDate = LocalDate.now.plusDays(1)

    "/metal gold tomorrow".decode[MetalRequest] shouldBe MetalRequest("gold", expDate, expDate).rightNec[CBRError]
  }

  it should "parse silver metal request on yesterday" in {
    val expDate = LocalDate.now.minusDays(1)

    "/metal silver yesterday".decode[MetalRequest] shouldBe MetalRequest("silver", expDate, expDate).rightNec[CBRError]
  }

  it should "parse platinum metal request on 10.12.2018 - 20.12.2018" in {
    val expDate = LocalDate.of(2018, 12, 20)

    "/metal platinum 10.12.2018 20.12.2018".decode[MetalRequest] shouldBe MetalRequest("platinum", ld, expDate).rightNec[CBRError]
  }

  it should "parse wrong metal command" in {
    "/metall all 11.11.2018 11.11.2018".decode[MetalRequest] shouldBe
      WrongCommandInstruction("Could not parse metal l,all,11.11.2018,11.11.2018").leftNec[MetalRequest]
  }
}