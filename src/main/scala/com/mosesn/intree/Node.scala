package com.mosesn.intree

case class Node[A : Ordering, B](
  interval: Interval[A],
  myValue: B,
  parent: Option[Node[A, B]],
  left: Option[Node[A, B]],
  right: Option[Node[A, B]]) {

  def contains(elt: A): Boolean = interval.contains(elt)

  def <(other: Node[A, B]): Boolean = implicitly[Ordering[A]].lt(key, other.key)

  def key: A = interval.first

  def treeMax: A

  def value: B = myValue
}

object Leaf {
  def apply[A : Ordering, B](
    interval: Interval[A],
    value: B,
    parent: Option[Node[A, B]]): Node[A, B] = Node(interval, value, parent, None, None)
}
