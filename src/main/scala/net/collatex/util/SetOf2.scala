package net.collatex.util

import scala.collection.StrictOptimizedIterableOps
import scala.collection.immutable.AbstractSet
import scala.collection.immutable.Set

// Copied and hacked from Set2 library code
class SetOf2[A](elem1: A, elem2: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
  override def size: Int = 2

  override def isEmpty = false

  override def knownSize: Int = size

  def contains(elem: A): Boolean = elem == elem1 || elem == elem2

  def incl(elem: A): Set[A] =
    if (contains(elem)) this
    else Set(elem1, elem2, elem)

  def excl(elem: A): Set[A] =
    if (elem == elem1) Set(elem2)
    else if (elem == elem2) Set(elem1)
    else this

  def iterator: Iterator[A] = Iterator[A](elem1, elem2)

  private def getElem(i: Int) = i match {
    case 0 => elem1
    case 1 => elem2
  }

  override def foreach[U](f: A => U): Unit = {
    f(elem1)
    f(elem2)
  }

  override def exists(p: A => Boolean): Boolean = {
    p(elem1) || p(elem2)
  }

  override def forall(p: A => Boolean): Boolean = {
    p(elem1) && p(elem2)
  }

  override def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
    var r1: A = null.asInstanceOf[A]
    var n = 0
    if (pred(elem1) != isFlipped) {
      r1 = elem1; n += 1
    }
    if (pred(elem2) != isFlipped) {
      if (n == 0) r1 = elem2; n += 1
    }

    n match {
      case 0 => Set.empty
      case 1 => Set(r1)
      case 2 => this
    }
  }

  override def find(p: A => Boolean): Option[A] = {
    if (p(elem1)) Some(elem1)
    else Some(elem2).filter(p)
  }

  override def head: A = elem1
  
  override def last: A = elem2

  override def tail: Set[A] = Set(elem2)
}
