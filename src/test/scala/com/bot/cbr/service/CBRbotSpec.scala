package com.bot.cbr.service

import java.time.LocalDate
import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO, Sync}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.Apply
import com.bot.cbr.UnitSpec
import com.bot.cbr.algebra.{BotService, CurrencyService, MetalService}
import com.bot.cbr.domain.CBRError.WrongUrl
import com.bot.cbr.domain._
import fs2.Stream
import io.chrisdavenport.log4cats.noop.NoOpLogger

import scala.concurrent.ExecutionContext


class CBRbotSpec extends UnitSpec {

  val testEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val cs: ContextShift[IO] = IO.contextShift(testEc)

  val expShowHelpMsg =
    """This bot shows currencies:
      |`?` - show this help message
      |`/currency` usd - show usd currency
      |`/currency` eur 06.11.2018 - show eur currency on the 6th of November in 2018 year
      |`/currency` - show all currencies on today
      |`/currency` all - show all currencies on today
      |`/currency` all 2018-11-06 - show all currencies on the 6th of November in 2018 year
      |`/metal` gold - show gold on today
      |`/metal` all - show all metals on today
      |`/metal` 2018-11-06 2018-11-08 - show all metals on 6, 7 and 8 of November
      |""".stripMargin

  val defLocalDate = LocalDate.of(1970, 1, 1)

  "start or ?" should "invoke showHelp method" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "?".some).some)
    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, defLocalDate, expShowHelpMsg))
  }

  it should "invoke showHelp method also" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/start".some).some)
    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, defLocalDate, expShowHelpMsg))
  }

  "Unknown command" should "invoke handleUnknown" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "unknown message".some).some)
    runLaunch[IO](update).unsafeRunSync() shouldBe ((-1L, defLocalDate, ""))
  }

  "launch" should "invoke showCurrency for usd on today" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency usd".some).some)

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, LocalDate.now, s"стоимость 1 USD на ${LocalDate.now} составляет 65"))
  }

  it should "invoke showCurrency for eur on today" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency eur today".some).some)

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, LocalDate.now, s"стоимость 1 EUR на ${LocalDate.now} составляет 75"))
  }

  it should "invoke showCurrency for usd on yesterday" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency usd yesterday".some).some)

    val expDate = LocalDate.now.minusDays(1)

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, expDate, s"стоимость 1 USD на $expDate составляет 65"))
  }

  it should "invoke showCurrency for usd on tomorrow" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency usd tomorrow".some).some)

    val expDate = LocalDate.now.plusDays(1)

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, expDate, s"стоимость 1 USD на $expDate составляет 65"))
  }

  it should "invoke showCurrency for usd on 2018-11-15" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency usd 2018-11-15".some).some)

    val expLocalDate = LocalDate.of(2018, 11, 15)

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, expLocalDate, s"стоимость 1 USD на $expLocalDate составляет 65"))
  }

  it should "invoke showCurrency for all" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency all".some).some)

    val expLocalDate = LocalDate.now
    val expMsg = s"стоимость 1 USD на $expLocalDate составляет 65\n" +
      s"стоимость 1 EUR на $expLocalDate составляет 75\n" +
      s"стоимость 10 CZK на $expLocalDate составляет 29.53\n"

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, LocalDate.now, expMsg))
  }

  it should "invoke showCurrency for all on 2018-11-15" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency all 2018-11-15".some).some)

    val expLocalDate = LocalDate.of(2018, 11, 15)
    val expMsg = s"стоимость 1 USD на $expLocalDate составляет 65\n" +
      s"стоимость 1 EUR на $expLocalDate составляет 75\n" +
      s"стоимость 10 CZK на $expLocalDate составляет 29.53\n"

    runLaunch[IO](update).unsafeRunSync() shouldBe ((123L, expLocalDate, expMsg))
  }

  it should "invoke showCurrency for bad currency" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(123L), "/currency bad_currency".some).some)
    runLaunch[IO](update).unsafeRunSync() shouldBe ((-1L, LocalDate.now, ""))
  }

  def runLaunch[F[_]: Sync](botUpdate: BotUpdate): F[(Long, LocalDate, String)] = for {
    chatRef <- Ref.of(-1L)
    ldRef <- Ref.of(defLocalDate)
    msgRef <- Ref.of("")

    logger = NoOpLogger.impl[F]
    lcur = List(
      Currency("USD", 1, 65, 840, "USD").asRight[CBRError],
      Currency("EUR", 1, 75, 978, "EUR").asRight[CBRError],
      Currency("CZK", 10, 29.53, 203, "CZK").asRight[CBRError]
    )
    cs = currencyService[F](ldRef, lcur)
    bs = botService[F](chatRef, msgRef, botUpdate.some)
    ms = metalService[F]()

    cbtBot = new CBRbot[F](bs, cs, ms, logger)

    _ <- cbtBot.launch.compile.drain

    chatId <- chatRef.get
    ld <- ldRef.get
    msg <- msgRef.get
  } yield (chatId, ld, msg)

  def botService[F[_]: Apply](chatRef: Ref[F, Long],
                              msgRef: Ref[F, String],
                              botUpdate: Option[BotUpdate]): BotService[F] = new BotService[F] {
    override def sendMessage(chatId: Long, message: String): Stream[F, Unit] = {
      Stream.eval(chatRef.set(chatId) *> msgRef.set(message))
    }

    override def pollUpdates(fromOffset: Long): Stream[F, BotUpdate] =
      botUpdate.map(bu => Stream.emit(bu).covary[F]).getOrElse(Stream.empty)
  }

  def currencyService[F[_]](ldRef: Ref[F, LocalDate], res: List[Either[CBRError, Currency]]): CurrencyService[F] = new CurrencyService[F] {
    override def getCurrencies(date: LocalDate): Stream[F, Either[CBRError, Currency]] =
      Stream.eval(ldRef.set(date)).drain ++ Stream.emits(res).covary[F]
  }

  def emptyCurrencyService[F[_]: Sync]: F[CurrencyService[F]] =
    Ref.of(LocalDate.now).map(currencyService(_, List(Left(WrongUrl("url")))))

  def metalService[F[_]](): MetalService[F] = new MetalService[F] {
    override def getMetals(start: LocalDate, end: LocalDate): Stream[F, Either[CBRError, Metal]] = ???
  }
}
