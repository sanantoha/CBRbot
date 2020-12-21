package com.bot.cbr

import java.nio.file.Paths

import cats.effect.{Async, Blocker, ContextShift}
import fs2.text
import cats.instances.string._
import fs2.io.file._

class ReadData[F[_]: Async: ContextShift](file: String, blocker: Blocker) extends (() => F[String]) {

  private val chunkSize = 4096

  override def apply(): F[String] =
    readAll[F](Paths.get(file), blocker, chunkSize)
      .through(text.utf8Decode)
      .through(text.lines)
      .foldMonoid
      .compile
      .foldMonoid

}
