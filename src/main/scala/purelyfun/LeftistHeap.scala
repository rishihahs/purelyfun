package purelyfun

/**
 * Implementation of leftist heaps from Chapter 3.1
 */
object LeftistHeap {

  sealed trait LeftistHeap[+A]

  case object Nil extends LeftistHeap[Nothing]

  case class LeftistTree[A](rank: Int, elem: A, left: LeftistHeap[A], right: LeftistHeap[A]) extends LeftistHeap[A]

  def apply[A](elems: A*)(implicit ord: Ordering[A]): LeftistHeap[A] = elems.foldLeft(Nil: LeftistHeap[A])((acc, e) => insert(e, acc))

  // Insert element into leftist heap
  def insert[A](elem: A, h: LeftistHeap[A])(implicit ord: Ordering[A]): LeftistHeap[A] = merge(h, LeftistTree(1, elem, Nil, Nil))

  // Merges two leftist heaps
  def merge[A](a: LeftistHeap[A], b: LeftistHeap[A])(implicit ord: Ordering[A]): LeftistHeap[A] = {
    // Rank of heap
    def rank[A](h: LeftistHeap[A]): Int = h match {
      case Nil => 0
      case LeftistTree(r, _, _, _) => r
    }

    // Make tree from element and two subtrees, swap if necessary
    def makeTree(x: A, l: LeftistHeap[A], r: LeftistHeap[A]) = if (rank(l) >= rank(r))
      LeftistTree(rank(r) + 1, x, l, r)
    else
      LeftistTree(rank(l) + 1, x, r, l)

    (a, b) match {
      case (x, Nil) => x
      case (Nil, x) => x
      case (LeftistTree(_, x, a1, b1), LeftistTree(_, y, _, _)) if ord.lteq(x, y) => makeTree(x, a1, merge(b1, b))
      case (LeftistTree(_, x, _, _), LeftistTree(_, y, a2, b2)) => makeTree(y, a2, merge(b2, a))
    }
  }

}
