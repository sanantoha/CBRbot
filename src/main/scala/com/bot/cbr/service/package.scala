package com.bot.cbr

import cats.data.{EitherNec, NonEmptyChain}
import com.bot.cbr.domain.CBRError
import com.bot.cbr.domain.CBRError.WrongXMLFormat
import cats.syntax.either._

package object service {

  def parseField[A](f: => A): EitherNec[CBRError, A] =
    Either.catchNonFatal(f).leftMap(e => NonEmptyChain.one(WrongXMLFormat(e.getMessage)))
}
