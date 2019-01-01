package com.bot.cbr.service

import java.time.LocalDate

import cats.data.{EitherNec, NonEmptyChain}
import com.bot.cbr.UnitSpec
import com.bot.cbr.domain.MetalType.{Gold, Palladium, Platinum, Silver}
import com.bot.cbr.domain.{CBRError, Metal}
import cats.syntax.either._
import cats.instances.either._
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import cats.instances.parallel._
import scala.xml.XML

class MetalParserSpec extends UnitSpec {

  val srcGold = "<Record Date=\"29.11.2018\" Code=\"1\"><Buy>2611,15</Buy><Sell>2611,15</Sell></Record>"
  val srcSilver = "<Record Date=\"30.11.2018\" Code=\"2\"><Buy>30,72</Buy><Sell>30,72</Sell></Record>"
  val srcPlatinum = "<Record Date=\"29.11.2018\" Code=\"3\"><Buy>1788,55</Buy><Sell>1788,55</Sell></Record>"
  val srcPalladium = "<Record Date=\"29.11.2018\" Code=\"4\"><Buy>2500,96</Buy><Sell>2700,96</Sell></Record>"

  val invalidMetal = "<Record Date=\"29-11-2018\" Code=\"10\"><Buy>wrong buy</Buy><Sell>wrong sell</Sell></Record>"

  "Parse Gold source" should "return expected metal" in {
    val node = XML.loadString(srcGold)
    val parser = new MetalParserImpl[EitherNec[CBRError, ?], NonEmptyChain[CBRError]](NonEmptyChain.one)

    val date = LocalDate.of(2018, 11, 29)
    val expMetal = Metal(Gold, date, BigDecimal(2611.15), BigDecimal(2611.15))

    parser.parse(node) shouldBe expMetal.rightNec[CBRError]
  }

  it should "return chain of errors" in {
    val node = XML.loadString(invalidMetal)
    val parser = new MetalParserImpl[EitherNec[CBRError, ?], NonEmptyChain[CBRError]](NonEmptyChain.one)

    val nec: NonEmptyChain[CBRError] = NonEmptyChain(
      WrongXMLFormat("key not found: 10"),
      WrongXMLFormat("Text '29-11-2018' could not be parsed at index 2"),
      WrongXMLFormat("Unparseable number: \"wrong buy\""),
      WrongXMLFormat("Unparseable number: \"wrong sell\"")
    )

    parser.parse(node).leftMap(_.toNonEmptyList) shouldBe nec.toNonEmptyList.asLeft[Metal]
  }

  "Parse Silver source" should "return expected metal" in {
    val node = XML.loadString(srcSilver)
    val parser = new MetalParserImpl[EitherNec[CBRError, ?], NonEmptyChain[CBRError]](NonEmptyChain.one)

    val date = LocalDate.of(2018, 11, 30)
    val expMetal = Metal(Silver, date, BigDecimal(30.72), BigDecimal(30.72))

    parser.parse(node) shouldBe expMetal.rightNec[CBRError]
  }

  "Parse Platinum" should "return expected metal" in {
    val node = XML.loadString(srcPlatinum)
    val parser = new MetalParserImpl[EitherNec[CBRError, ?], NonEmptyChain[CBRError]](NonEmptyChain.one)

    val date = LocalDate.of(2018, 11, 29)
    val expMetal = Metal(Platinum, date, BigDecimal(1788.55), BigDecimal(1788.55))

    parser.parse(node) shouldBe expMetal.rightNec[CBRError]
  }

  "Parse Palladium" should "return expected metal" in {
    val node = XML.loadString(srcPalladium)
    val parser = new MetalParserImpl[EitherNec[CBRError, ?], NonEmptyChain[CBRError]](NonEmptyChain.one)

    val date = LocalDate.of(2018, 11, 29)
    val expMetal = Metal(Palladium, date, BigDecimal(2500.96), BigDecimal(2700.96))

    parser.parse(node) shouldBe expMetal.rightNec[CBRError]
  }
}
