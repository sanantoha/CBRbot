package com.bot.cbr.service

import java.time.LocalDate
import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{Async, ContextShift, IO, Sync}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.Apply
import cats.data.EitherNec
import com.bot.cbr.UnitSpec
import com.bot.cbr.algebra.{BotService, CurrencyService, MetalService}
import com.bot.cbr.domain.CBRError.WrongUrl
import com.bot.cbr.domain.MetalType.{Gold, Palladium, Platinum, Silver}
import com.bot.cbr.domain._
import fs2.Stream
import io.chrisdavenport.log4cats.noop.NoOpLogger
import org.scalatest.BeforeAndAfterEach
import com.bot.cbr.cache.CurrencyCache._
import scala.concurrent.ExecutionContext
import scalacache.CatsEffect.modes._


class CBRbotSpec extends UnitSpec with BeforeAndAfterEach {

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

  val chatId = 123L

  val lmet = List(
    Metal(Gold, LocalDate.of(2018, 12, 1), BigDecimal(2610.66), BigDecimal(2610.66)).rightNec[CBRError],
    Metal(Silver, LocalDate.of(2018, 12, 1), BigDecimal(30.51), BigDecimal(30.51)).rightNec[CBRError],
    Metal(Platinum, LocalDate.of(2018, 12, 1), BigDecimal(1732.67), BigDecimal(1732.67)).rightNec[CBRError],
    Metal(Palladium, LocalDate.of(2018, 12, 1), BigDecimal(2549.81), BigDecimal(2549.81)).rightNec[CBRError],
    Metal(Gold, LocalDate.of(2018, 12, 2), BigDecimal(2611.66), BigDecimal(2612.66)).rightNec[CBRError],
    Metal(Silver, LocalDate.of(2018, 12, 2), BigDecimal(31.51), BigDecimal(32.51)).rightNec[CBRError],
    Metal(Platinum, LocalDate.of(2018, 12, 2), BigDecimal(1733.67), BigDecimal(1734.67)).rightNec[CBRError],
    Metal(Palladium, LocalDate.of(2018, 12, 2), BigDecimal(2550.81), BigDecimal(2551.81)).rightNec[CBRError]
  )

  override protected def afterEach(): Unit = {
    currencyCache.removeAll[IO]().void.unsafeRunSync()
  }

  "start or ?" should "invoke showHelp method" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "?".some).some)
    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, defLocalDate, expShowHelpMsg))
  }

  it should "invoke showHelp method also" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/start".some).some)
    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, defLocalDate, expShowHelpMsg))
  }

  "Unknown command" should "invoke handleUnknown" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "unknown message".some).some)
    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((-1L, defLocalDate, ""))
  }

  "launch" should "invoke showCurrency for usd on today" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency usd".some).some)

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, LocalDate.now, s"стоимость 1 USD на ${LocalDate.now} составляет 65"))
  }

  it should "invoke showCurrency for eur on today" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency eur today".some).some)

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, LocalDate.now, s"стоимость 1 EUR на ${LocalDate.now} составляет 75"))
  }

  it should "invoke showCurrency for usd on yesterday" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency usd yesterday".some).some)

    val expDate = LocalDate.now.minusDays(1)

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, expDate, s"стоимость 1 USD на $expDate составляет 65"))
  }

  it should "invoke showCurrency for usd on tomorrow" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency usd tomorrow".some).some)

    val expDate = LocalDate.now.plusDays(1)

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, expDate, s"стоимость 1 USD на $expDate составляет 65"))
  }

  it should "invoke showCurrency for usd on 2018-11-15" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency usd 2018-11-15".some).some)

    val expLocalDate = LocalDate.of(2018, 11, 15)

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, expLocalDate, s"стоимость 1 USD на $expLocalDate составляет 65"))
  }

  it should "invoke showCurrency for all" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency all".some).some)

    val expLocalDate = LocalDate.now
    val expMsg = s"стоимость 1 USD на $expLocalDate составляет 65\n" +
      s"стоимость 1 EUR на $expLocalDate составляет 75\n" +
      s"стоимость 10 CZK на $expLocalDate составляет 29.53\n"

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, LocalDate.now, expMsg))
  }

  it should "invoke showCurrency for all on 2018-11-15" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency all 2018-11-15".some).some)

    val expLocalDate = LocalDate.of(2018, 11, 15)
    val expMsg = s"стоимость 1 USD на $expLocalDate составляет 65\n" +
      s"стоимость 1 EUR на $expLocalDate составляет 75\n" +
      s"стоимость 10 CZK на $expLocalDate составляет 29.53\n"

    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((chatId, expLocalDate, expMsg))
  }

  it should "invoke showCurrency for bad currency" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/currency bad_currency".some).some)
    runLaunchForCurrency[IO](update).unsafeRunSync() shouldBe ((-1L, LocalDate.now, ""))
  }

  it should "invoke showMetal for gold" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/metal gold 2018-12-01 2018-12-02".some).some)

    val expLocalDate = LocalDate.of(2018, 12, 1)
    val expLocalSecondDate = expLocalDate.plusDays(1)
    val expMsg = s"стоимость Gold на $expLocalDate покупка 2610.66, продажа 2610.66\n" +
                 s"стоимость Gold на $expLocalSecondDate покупка 2611.66, продажа 2612.66\n"

    runLaunchForMetal[IO](update, lmet).unsafeRunSync() shouldBe ((chatId, expLocalDate, expLocalSecondDate, expMsg))
  }

  it should "invoke showMetal for gold on 2018-11-11" in {
    val update = BotUpdate(1L, BotMessage(12L, Chat(chatId), "/metal gold 2018-11-11".some).some)

    val expLocalDate = LocalDate.of(2018, 11, 11)

    val expMsg = ""

    runLaunchForMetal[IO](update, Nil).unsafeRunSync() shouldBe ((chatId, expLocalDate, expLocalDate, expMsg))
  }

  def runLaunchForCurrency[F[_]: Async](botUpdate: BotUpdate): F[(Long, LocalDate, String)] = for {
    chatRef <- Ref.of(-1L)
    ldRef <- Ref.of(defLocalDate)
    dummyRef <- Ref.of(defLocalDate)
    msgRef <- Ref.of("")

    logger = NoOpLogger.impl[F]
    lcur = List(
      Currency("USD", 1, 65, 840, "USD").rightNec[CBRError],
      Currency("EUR", 1, 75, 978, "EUR").rightNec[CBRError],
      Currency("CZK", 10, 29.53, 203, "CZK").rightNec[CBRError]
    )
    cs = currencyService[F](ldRef, lcur)
    bs = botService[F](chatRef, msgRef, botUpdate.some)
    ms = metalService[F](dummyRef, dummyRef, Nil)

    cbtBot = new CBRbot[F](bs, cs, ms, logger)

    _ <- cbtBot.launch.compile.drain

    chatId <- chatRef.get
    ld <- ldRef.get
    msg <- msgRef.get
  } yield (chatId, ld, msg)

  def runLaunchForMetal[F[_]: Async](botUpdate: BotUpdate, lmet: List[EitherNec[CBRError, Metal]]): F[(Long, LocalDate, LocalDate, String)] = for {
    chatRef <- Ref.of(-1L)
    startRef <- Ref.of(defLocalDate)
    endRef <- Ref.of(defLocalDate)
    dummyRef <- Ref.of(defLocalDate)
    msgRef <- Ref.of("")

    logger = NoOpLogger.impl[F]

    cs = currencyService[F](dummyRef, Nil)
    bs = botService[F](chatRef, msgRef, botUpdate.some)
    ms = metalService[F](startRef, endRef, lmet)

    cbtBot = new CBRbot[F](bs, cs, ms, logger)

    _ <- cbtBot.launch.compile.drain

    chatId <- chatRef.get
    start <- startRef.get
    end <- endRef.get
    msg <- msgRef.get
  } yield (chatId, start, end, msg)

  def botService[F[_]: Apply](chatRef: Ref[F, Long],
                              msgRef: Ref[F, String],
                              botUpdate: Option[BotUpdate]): BotService[F] = new BotService[F] {
    override def sendMessage(chatId: Long, message: String): Stream[F, Unit] = {
      Stream.eval(chatRef.set(chatId) *> msgRef.set(message))
    }

    override def pollUpdates(fromOffset: Long): Stream[F, BotUpdate] =
      botUpdate.map(bu => Stream.emit(bu).covary[F]).getOrElse(Stream.empty)
  }

  def currencyService[F[_]](ldRef: Ref[F, LocalDate], res: List[EitherNec[CBRError, Currency]]): CurrencyService[F] = new CurrencyService[F] {
    override def getCurrencies(date: LocalDate): Stream[F, EitherNec[CBRError, Currency]] =
      Stream.eval(ldRef.set(date)).drain ++ Stream.emits(res).covary[F]
  }

  def emptyCurrencyService[F[_]: Sync]: F[CurrencyService[F]] =
    Ref.of(LocalDate.now).map(currencyService(_, List(WrongUrl("url").leftNec[Currency])))

  def metalService[F[_]](startRef: Ref[F, LocalDate], endRef: Ref[F, LocalDate], res: List[EitherNec[CBRError, Metal]]): MetalService[F] = new MetalService[F] {
    override def getMetals(start: LocalDate, end: LocalDate): Stream[F, EitherNec[CBRError, Metal]] =
      Stream.eval(startRef.set(start)).drain ++ Stream.eval(endRef.set(end)).drain ++ Stream.emits(res).covary[F]
  }
}
