package com.mosesn.intree

case class Node[A : Ordering, B](
  interval: Interval[A],
  value: B,
  parent: Option[Node[A, B]],
  left: Option[Node[A, B]],
  right: Option[Node[A, B]]) {

  def contains(elt: A): Boolean = interval.contains(elt)

  def <(other: Node[A, B]): Boolean = implicitly[Ordering[A]].lt(key, other.key)

  def key: A = interval.first

  def treeMax: A = {
    val leftMax = left map (_.treeMax)
    val rightMax = right map (_.treeMax)
    leftMax -> rightMax match {
      case (None, None) => interval.second
      case (None, Some(right)) => right
      case (Some(left), None) => left
      case (Some(left), Some(right)) => implicitly[Ordering[A]].max(left, right)
    }
  }
}

object Leaf {
  def apply[A : Ordering, B](
    interval: Interval[A],
    value: B,
    parent: Option[Node[A, B]]): Node[A, B] = Node(interval, value, parent, None, None)
}
