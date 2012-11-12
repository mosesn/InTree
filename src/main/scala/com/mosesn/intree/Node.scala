package com.mosesn.intree

sealed trait Node[A, B] {
  def parent: Option[Node[A, B]]

  def left: Option[Node[A, B]]

  def right: Option[Node[A, B]]

  def leftMayContain(elt: A): Boolean

  def rightMayContain(elt: A): Boolean

  def contains(elt: A): Boolean

  private[Node] def interval: Interval[A]

  private[Node] def max: A

  def value: B
}

/*
case class Leaf[A, B](interval: Interval[A], value: B) extends Node[A, B]

case class Branch[A, B](interval: Interval[A], value: B) extends Node[A, B]
*/
