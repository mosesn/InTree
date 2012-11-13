package com.mosesn.intree

trait Braces {
  def leftOperator[A : Ordering](left: A, right: A): Boolean

  def rightOperator[A : Ordering](left: A, right: A): Boolean
}

trait LeftOpen extends Braces {
  override def leftOperator[A : Ordering](left: A, right: A): Boolean =
    implicitly[Ordering[A]].lt(left, right)
}

trait RightOpen extends Braces {
  override def rightOperator[A : Ordering](left: A, right: A): Boolean =
    implicitly[Ordering[A]].lt(left, right)
}

trait LeftClosed extends Braces {
  override def leftOperator[A : Ordering](left: A, right: A): Boolean =
    implicitly[Ordering[A]].lteq(left, right)
}

trait RightClosed extends Braces {
  override def rightOperator[A : Ordering](left: A, right: A): Boolean =
    implicitly[Ordering[A]].lteq(left, right)
}

case class Interval[A : Ordering](
  first: A,
  second: A,
  braces: Braces = new LeftOpen with RightOpen) {
  require(
    implicitly[Ordering[A]].lteq(first, second),
    "The first argument to a Range but be less than the second under your ordering.")

  def contains(elt: A): Boolean = braces.leftOperator(first, elt) && braces.rightOperator(elt, second)

}
