package com.mosesn.intree

import org.scalatest.FunSpec
import scala.util.Random

class IntervalSpec extends FunSpec {
  val rand = new Random
  describe("An Interval") {
    it ("should be able to tell you if an item is in it") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first - 1, second + first + 1)
      val inbetween = rand.nextInt(second)
      assert(
        interval.contains(first + inbetween) === true,
        "%d is not in %s".format(first + inbetween, interval.toString))
    }

    it ("should be able to reject a smaller item") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first)
      val third = rand.nextInt(1000)
      assert(
        interval.contains(first - third) === false,
        "%d is not bigger than %s".format(first - third, interval.toString))
    }

    it ("should be able to reject a bigger item") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first)
      val third = rand.nextInt(1000)
      assert(
        interval.contains(first + second + third) === false,
        "%d is not less than %s".format(first + second + third, interval.toString))
    }

    it ("should be able to reject an item on the upper bound") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first)
      assert(
        interval.contains(first + second) === false,
        "%d is in %s".format(first + second, interval.toString))
    }

    it ("should be able to reject an item on the lower bound") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first)
      assert(
        interval.contains(first) === false,
        "%d is in %s".format(first, interval.toString))
    }

    it ("should be able to accept an item if it's closed") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first, new LeftClosed with RightClosed)
      val inbetween = rand.nextInt(second)
      assert(
        interval.contains(first + inbetween) === true,
        "%d is not in %s".format(first + inbetween, interval.toString))
    }
    it ("should be able to reject a smaller item if it's closed") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first, new LeftClosed with RightClosed)
      val third = rand.nextInt(1000)
      assert(
        interval.contains(first - third) === false,
        "%d is not in %s".format(first - third, interval.toString))
    }

    it ("should be able to reject a bigger item if it's closed") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first, new LeftClosed with RightClosed)
      val third = rand.nextInt(1000)
      assert(
        interval.contains(first + second + third) === false,
        "%d is not in %s".format(first + second + third, interval.toString))
    }

    it ("should be able to accept an item on the upper bound if it's closed") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first, new LeftClosed with RightClosed)
      assert(
        interval.contains(first) === true,
        "%d is not in %s".format(first, interval.toString))
    }

    it ("should be able to accept an item on the lower bound if it's closed") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first, new LeftClosed with RightClosed)
      assert(
        interval.contains(first + second) === true,
        "%d is not in %s".format(first + second, interval.toString))
    }

    it ("should be able to tell if an item is smaller than it") {
      val first = rand.nextInt(1000)
      val second = rand.nextInt(1000)
      val interval = Interval(first, second + first, new LeftClosed with RightClosed)
      assert(
        interval.gt(first - 1) === true,
        "%d is in %s".format(first - 1, interval.toString))
    }
  }
}
