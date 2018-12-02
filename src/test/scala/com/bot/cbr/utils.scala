package com.bot.cbr

import cats.effect.Sync
import org.http4s.{HttpApp, Response}
import org.http4s.client.Client
import org.http4s.dsl.io.Ok
import cats.syntax.applicative._

object utils {

  def mkClient[F[_] : Sync](response: String): Client[F] = {
    val httpApp = HttpApp[F](_ => Response[F](Ok).withEntity(response).pure[F])

    Client.fromHttpApp[F](httpApp)
  }
}
