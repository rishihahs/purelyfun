package purelyfun

/**
 * Implementation of sets from Chapter 2.2
 */

sealed trait Set[+A]

case object Nil extends Set[Nothing]

case class SetTree[A](left: Set[A], elem: A, right: Set[A]) extends Set[A]

object Set {

  def apply[A](elems: A*)(implicit ord: Ordering[A]): Set[A] = elems.foldLeft(Nil: Set[A])((set, e) => insert(e, set))

  // Checks if provided element is a member of the set
  def isMember[A, T](start: A, set: Set[A])(implicit ord: Ordering[A]): Boolean = set match {
    case Nil => false
    case SetTree(l, e, _) if ord.lt(start, e) => isMember(start, l)
    case SetTree(_, e, r) if ord.gt(start, e) => isMember(start, r)
    case SetTree(_, _, _) => true // since start == e at this point
  }

  // Inserts element into set
  def insert[A](elem: A, set: Set[A])(implicit ord: Ordering[A]): Set[A] = set match {
    case Nil => SetTree(Nil, elem, Nil)
    case SetTree(l, e, r) if ord.equiv(elem, e) => SetTree(l, e, r)
    case SetTree(l, e, r) if ord.lt(elem, e) => SetTree(insert(elem, l), e, r)
    case SetTree(l, e, r) if ord.gt(elem, e) => SetTree(l, e, insert(elem, r))
  }

}
