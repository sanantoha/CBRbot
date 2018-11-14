package com.bot.cbr.service

import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{ConcurrentEffect, ContextShift, IO, Sync}
import com.bot.cbr.UnitSpec
import com.bot.cbr.config.Config
import io.chrisdavenport.log4cats.noop.NoOpLogger
import org.http4s.client.Client
import org.http4s.dsl.io.Ok
import org.http4s.{EntityEncoder, HttpApp, Response}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.bot.cbr.domain.{BotMessage, BotResponse, BotUpdate, Chat}
import io.circe.generic.auto._
import org.http4s.circe._
import cats.syntax.option._

import scala.concurrent.ExecutionContext

class BotServiceSpec extends UnitSpec {

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  val url = "url"
  val chatId = 1L
  val msg = "msg"

  "BotService" should "send message to chat" in {
    runSendMessage[IO].unsafeRunSync() shouldBe s"$url/sendMessage?chat_id=$chatId&parse_mode=Markdown&text=msg"
  }

  "BotService" should "receive messages from chat" in {
    println(runPollUpdate[IO].unsafeRunSync())
  }

  def runPollUpdate[F[_]: ConcurrentEffect]: F[(String, BotUpdate)] = for {
    ref <- Ref[F].of("")
    client = mkClientForPollUpdate(ref)
    logger = NoOpLogger.impl[F]
    config = Config(url, "url2")

    service = new BotServiceImpl[F](config, client, logger)
    botUpdate <- service.pollUpdates(0).take(1).compile.lastOrError
    expUrl <- ref.get
  } yield (expUrl, botUpdate)

  def mkClientForPollUpdate[F[_]: Sync](ref: Ref[F, String]): Client[F] = {
    implicit val encoder: EntityEncoder[F, BotResponse[List[BotUpdate]]] = jsonEncoderOf[F, BotResponse[List[BotUpdate]]]

    val httpApp = HttpApp[F] {
      r => ref.set(r.uri.toString) as Response[F](Ok)
        .withEntity(BotResponse(ok = true,
          List(BotUpdate(1L, BotMessage(123L, Chat(chatId), msg.some).some))
        ))
    }

    Client.fromHttpApp(httpApp)
  }

  def mkClientForSendingMsg[F[_] : Sync](ref: Ref[F, String]): Client[F] = {
    val httpApp = HttpApp[F](r => ref.set(r.uri.toString) as Response[F](Ok))

    Client.fromHttpApp[F](httpApp)
  }

  def runSendMessage[F[_] : ConcurrentEffect]: F[String] =
    for {
      ref <- Ref[F].of("")
      client = mkClientForSendingMsg[F](ref)
      logger = NoOpLogger.impl[F]
      config = Config(url, "url2")
      service = new BotServiceImpl[F](config, client, logger)

      _ <- service.sendMessage(chatId, msg).compile.drain
      res <- ref.get
    } yield res

}
