package com.bot.cbr

import java.nio.file.Paths

import cats.effect.{ContextShift, Sync}
import fs2.{io, text}
import _root_.io.chrisdavenport.linebacker.Linebacker
import cats.instances.string._

class ReadData[F[_]: Sync: ContextShift](file: String)(implicit val linebacker: Linebacker[F]) extends (() => F[String]) {

  private val chunkSize = 4096

  override def apply(): F[String] =
    io.file
      .readAll[F](Paths.get(file), linebacker.blockingContext, chunkSize)
      .through(text.utf8Decode)
      .through(text.lines)
      .foldMonoid
      .compile
      .foldMonoid

}
