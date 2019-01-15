package com.bot.cbr.cache

import java.util.concurrent.TimeUnit

import cats.data.EitherNec
import com.bot.cbr.domain.{CBRError, Metal}
import com.github.benmanes.caffeine.cache.{Caffeine, Cache => CCache}
import scalacache.{Cache, CacheConfig, Entry}
import scalacache.caffeine.CaffeineCache

object MetalCache {

  import CacheConfig._

  val underlyingMetalCache: CCache[String, Entry[Vector[EitherNec[CBRError, Metal]]]] = Caffeine.newBuilder()
    .expireAfterWrite(1, TimeUnit.HOURS)
    .maximumSize(1000L)
    .build[String, Entry[Vector[EitherNec[CBRError, Metal]]]]()

  implicit val metalCache: Cache[Vector[EitherNec[CBRError, Metal]]] = CaffeineCache.apply(underlyingMetalCache)
}
