package com.mosesn.intree

import org.scalatest.FunSpec
import scala.util.Random

class InTreeSpec extends FunSpec {
  describe("An InTree") {
    it("should know when it's empty") {
      assert(InTree[Int, Int]().isEmpty === true)
    }

    it("should know when it's not empty") {
      assert(InTree[Int, Int](Interval(0, 5) -> 3).isEmpty === false)
    }

    it("should turn into a seq properly") {
      val pair = Interval(0, 5) -> 3
      assert(InTree[Int, Int](pair).toSeq === Seq(pair))
    }

    it("should search properly") {
      val pair = Interval(0, 5) -> 3
      assert(InTree[Int, Int](pair).search(2) === Seq(3))
    }

    it("should search properly with a few elements") {
      val pair1 = Interval(0, 5) -> 3
      val pair2 = Interval(1, 8) -> 4
      val pair3 = Interval(-3, 2) -> 5
      assert(InTree[Int, Int](pair1, pair2, pair3).search(2).toSet === Set(3, 4))
    }

    it("should search properly with fuzzing") {
      val rand = new Random
      val pairs = (0 until 15) map { _ =>
        val first = rand.nextInt(1000)
        Interval(first, first + rand.nextInt(1000)) -> rand.nextInt
      }

      val inbetween = rand.nextInt(1000) * 2
      val goodPairs = (pairs filter (pair => pair._1.contains(inbetween)))
      val tree = InTree[Int, Int](pairs: _*)
      assert(tree.size === 15)
      assert(tree.search(inbetween).toSet === (goodPairs map (_._2)).toSet)
    }
  }
}
