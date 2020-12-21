package com.bot.cbr

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class UnitSpec
  extends AnyFlatSpec
    with Matchers
    with OptionValues
    with EitherValues
    with Inside
    with Inspectors
