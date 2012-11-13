package com.mosesn.intree

import scala.annotation.tailrec
import scala.collection.mutable.Builder

trait InTree[A, B] extends Iterable[Pair[Interval[A], B]] {
  type Elem = Pair[Interval[A], B]

  implicit val ordering: Ordering[A]

  val root: Option[Node[A, B]]

  def search(elt: A): Seq[B] = {
    def searchTree[A : Ordering, B](elt: A, roots: Seq[Node[A, B]]): Seq[B] = {
      def leftMayContain(rootNode: Node[A, B]): Boolean = implicitly[Ordering[A]].lteq(elt, rootNode.treeMax)

      def rightMayContain(rootNode: Node[A, B]): Boolean = implicitly[Ordering[A]].gteq(elt, rootNode.key) &&
        implicitly[Ordering[A]].lteq(elt, rootNode.treeMax)

      def leftNodesToSearch(rootNode: Node[A, B]): Seq[Node[A, B]] = if (leftMayContain(rootNode))
        rootNode.left.toSeq
      else
        Seq.empty

      def rightNodesToSearch(rootNode: Node[A, B]): Seq[Node[A, B]] = if (rightMayContain(rootNode))
        rootNode.right.toSeq
      else Seq.empty

      def getValueIfValid(rootNode: Node[A, B]): Seq[B] = if (rootNode.contains(elt))
        Seq(rootNode.value)
      else
        Seq.empty

      if (!roots.isEmpty) {
        (roots flatMap { head =>
          getValueIfValid(head)
        }) ++ searchTree(elt, roots flatMap (rootNode => leftNodesToSearch(rootNode) ++ rightNodesToSearch(rootNode)))
      }
      else {
        Seq()
      }
    }

    searchTree(elt, root.toSeq)
  }

  override def iterator: Iterator[Elem] = new Iterator[Elem] {
    var cur: Seq[Node[A, B]] = root.toSeq

    def hasNext: Boolean = {
      !cur.isEmpty
    }

    def next(): Elem = {
      val head = cur.head
      cur = (cur.tail ++ head.left.toSeq ++ head.right.toSeq)
      head.interval -> head.value
    }
  }

  override def newBuilder: Builder[Elem, InTree[A, B]] = InTree.builder[A, B]
}

object InTree {
  def apply[A : Ordering, B](pairs: Pair[Interval[A], B]*): InTree[A, B] =
    pairs.foldRight(builder[A, B])((pair, bldr) => bldr += pair).result()

  def builder[A : Ordering, B]: Builder[Pair[Interval[A], B], InTree[A, B]] =
    new Builder[Pair[Interval[A], B], InTree[A, B]] { self =>
      var root: Option[Node[A, B]] = None

      override def +=(elt: Pair[Interval[A], B]): this.type = {
        val newNode = Leaf(elt._1, elt._2, None)

        def insert(subtree: Node[A, B]): Node[A, B] = {
          if (newNode < subtree) {
            subtree.copy(left = Some(subtree match {
              case Node(_, _, _, None, _) => newNode.copy(parent = Some(subtree))
              case Node(_, _, _, Some(left), _) => insert(left)
            }))
          }
          else {
            subtree.copy(right = Some(subtree match {
              case Node(_, _, _, _, None) => newNode.copy(parent = Some(subtree))
              case Node(_, _, _, _, Some(right)) => insert(right)
            }))
          }
        }

        root = Some(root match {
          case None => newNode
          case Some(node) => insert(node)
        })

        this
      }

    override def clear() {
      root = None
    }

    override def result(): InTree[A, B] = new InTree[A, B] {
      override val root = self.root
      override val ordering = implicitly[Ordering[A]]
    }
  }
}
