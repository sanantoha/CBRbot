package com.bot.cbr.codec

import com.bot.cbr.domain.CBRError
//import simulacrum._

//@typeclass
trait CurrencyDecoder[A] {
  def decode(s: String): Either[CBRError, A]
}

object CurrencyDecoder {
  def apply[A](implicit ev: CurrencyDecoder[A]): CurrencyDecoder[A] = ev
}

trait DecoderSyntax {
  implicit def toDecoderOps(s: String): CurrencyDecoderOps = new CurrencyDecoderOps(s)
}

final class CurrencyDecoderOps(val s: String) extends AnyVal {
  def decode[A: CurrencyDecoder]: Either[CBRError, A] = CurrencyDecoder[A].decode(s)
}
