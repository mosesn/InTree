package com.mosesn.intree

import scala.annotation.tailrec
import scala.collection.mutable.Builder

trait InTree[A, B] extends Iterable[Pair[Interval[A], B]] {
  type Elem = Pair[Interval[A], B]

  implicit val ordering: Ordering[A]

  val root: Option[Node[A, B]]

  def search(elt: A): Seq[B] = (root map (InTree.search(elt, _))).getOrElse(Seq.empty)

  override def iterator: Iterator[Elem]

  override def newBuilder: Builder[Elem, InTree[A, B]] = new Builder[Elem, InTree[A, B]] { self =>
    var root: Option[Node[A, B]] = None

    override def +=(elt: Elem): this.type = {
      val newNode = Leaf(elt._1, elt._2, None)

      def insert(subtree: Node[A, B]): Node[A, B] = {
        if (subtree < newNode) {
          subtree.copy(left = Some(subtree match {
            case Node(_, _, _, None, _) => newNode
            case Node(_, _, _, Some(left), _) => insert(left)
          }))
        }
        else {
          subtree.copy(right = Some(subtree match {
            case Node(_, _, _, _, None) => newNode
            case Node(_, _, _, _, Some(right)) => insert(right)
          }))
        }
      }

      root match {
        case None => root = Some(newNode)
        case Some(node) => insert(node)
      }
      this
    }

    override def clear() {
      root = None
    }

    override def result(): InTree[A, B] = new InTree[A, B] {
      val root = self.root
    }
  }
}

object InTree {
  def search[A : Ordering, B](elt: A, root: Node[A, B]): Seq[B] = {
    def leftMayContain: Boolean = implicitly[Ordering[A]].lt(elt, root.key)

    def rightMayContain: Boolean = implicitly[Ordering[A]].gteq(elt, root.key) &&
      implicitly[Ordering[A]].lt(elt, root.treeMax)

    def searchLeft: Seq[B] = if (leftMayContain)
      root.left.toSeq flatMap (search(elt, _))
    else
      Seq.empty

    def searchRight: Seq[B] = if (rightMayContain)
      root.right.toSeq flatMap (search(elt, _))
    else Seq.empty

    def getValueIfValid: Seq[B] = if (root.contains(elt))
      Seq(root.value)
    else
      Seq.empty

    getValueIfValid ++ searchLeft ++ searchRight
  }
}
