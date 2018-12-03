package com.bot.cbr.codec

import cats.data.EitherNec
import com.bot.cbr.domain.CBRError


trait Decoder[A] {
  def decode(s: String): EitherNec[CBRError, A]
}

object Decoder {
  def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev
}

trait DecoderSyntax {
  implicit def toDecoderOps(s: String): DecoderOps = new DecoderOps(s)
}

final class DecoderOps(val s: String) extends AnyVal {
  def decode[A: Decoder]: EitherNec[CBRError, A] = Decoder[A].decode(s)
}
