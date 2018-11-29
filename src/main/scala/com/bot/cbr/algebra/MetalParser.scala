package com.bot.cbr.algebra

import com.bot.cbr.domain.Metal

import scala.xml.Node

trait MetalParser[F[_]] {

  def parse(record: Node): F[Metal]
}
