package com.bot.cbr.cache

import java.util.concurrent.TimeUnit

import cats.data.EitherNec
import com.bot.cbr.domain.{CBRError, Currency}
import com.github.benmanes.caffeine.cache.{Caffeine, Cache => CCache}
import scalacache.caffeine.CaffeineCache
import scalacache.{Cache, CacheConfig, Entry}

object CurrencyCache {

  import CacheConfig._

  val underlyingCurrencyCache: CCache[String, Entry[Vector[EitherNec[CBRError, Currency]]]] = Caffeine.newBuilder()
    .expireAfterWrite(1, TimeUnit.HOURS)
    .maximumSize(1000L)
    .build[String, Entry[Vector[EitherNec[CBRError, Currency]]]]()

  implicit val currencyCache: Cache[Vector[EitherNec[CBRError, Currency]]] = CaffeineCache.apply(underlyingCurrencyCache)
}