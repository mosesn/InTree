package com.mosesn.intree

import scala.annotation.tailrec
import scala.collection.mutable.Builder

trait InTree[A, B] extends Iterable[Pair[Interval[A], B]] { self =>
  type Elem = Pair[Interval[A], B]

  def search(elt: A): Seq[B]

  override def iterator: Iterator[Elem]

  override def newBuilder: Builder[Elem, InTree[A, B]] = new Builder[Elem, InTree[A, B]] {
    //var root: Node[A, B] = null

    //TODO MN: Implement
    override def +=(elem: Elem): this.type = {
      //def insert(subtree: Node)

      /*
      if (root == null)
        null //root = Leaf(elem._1, elem._2)
      else
        null
      */
      this
    }

    //TODO MN: Implement
    override def clear() {
    }

    //TODO MN: Implement
    override def result(): InTree[A, B] = self
  }
}

object InTree {
  def search[A, B](elt: A, root: Node[A, B]): Seq[B] = {
    def searchLeft: Seq[B] = if (root.leftMayContain(elt))
      root.left.toSeq flatMap (search(elt, _))
    else
      Seq.empty

    def searchRight: Seq[B] = if (root.rightMayContain(elt))
      root.right.toSeq flatMap (search(elt, _))
    else Seq.empty

    def getValueIfValid: Seq[B] = if (root.contains(elt))
      Seq(root.value)
    else
      Seq.empty

    getValueIfValid ++ searchLeft ++ searchRight
  }
}
