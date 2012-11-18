package com.mosesn.intree

case class Node[A : Ordering, B](
  interval: Interval[A],
  value: B,
  left: Option[Node[A, B]],
  right: Option[Node[A, B]]) {

  left map { node =>
    require(node < this)
  }

  right map { node =>
    require(implicitly[Ordering[A]].lteq(this.key, node.key))
  }

  def contains(elt: A): Boolean = interval.contains(elt)

  def <(other: Node[A, B]): Boolean = implicitly[Ordering[A]].lt(key, other.key)

  def key: A = interval.first

  lazy val treeMax: A = {
    def max(elts: A*): A = elts reduce implicitly[Ordering[A]].max

    val leftMax = left map (_.treeMax)
    val rightMax = right map (_.treeMax)
    leftMax -> rightMax match {
      case (None, None) => interval.second
      case (None, Some(right)) => max(right, interval.second)
      case (Some(left), None) => max(left, interval.second)
      case (Some(left), Some(right)) => max(left, right, interval.second)
    }
  }
}

object Leaf {
  def apply[A : Ordering, B](
    interval: Interval[A],
    value: B): Node[A, B] = Node(interval, value, None, None)
}
