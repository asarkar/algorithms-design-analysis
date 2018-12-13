package org.asarkar.data

import scala.math.Ordering.Implicits._

/*
 * Ordered and Ordering are semantically different. An Ordered[T] is an object that supports comparisons against
 * other Ordered[T]s, while an Ordering[T] is a single object that is a collection of functions able to compare
 * ordinary Ts.
 *
 * The view bound [T <% Ordered[T]] means that there needs to be an implicit function T => Ordered[T] somewhere
 * in scope.
 * However, def min[T <% Ordering[T]](a: T, b: T) won't work. This requires a T => Ordering[T], or a function from
 * values of a type to orderings on that type. For some types this might exist, but for most, like Int, it won't.
 *
 * The other kind of bound is the context bound, T: Ordering.  A context bound of this form requests an implicit value
 * (as opposed to an implicit function for Ordered) Ordering[T], that is, def min[T: Ordering](a: T, b: T) takes two
 * Ts as arguments, and also one implicit Ordering[T] that defines how they are ordered.
 *
 * More formally: A view bound is of the form Left <% Right for some types Left and Right of kind *,
 * leading to the creation of an implicit parameter of type Left => Right. A context bound is of the form Left: Right
 * for some type Left of some kind k and some type Right of kind k -> *, leading to the creation of an implicit
 * parameter of type Right[Left].
 *
 * traits cannot have type parameters with context bounds, since traits are not allowed to have value parameters,
 * so any sort of "implicit" constraint is not allowed.
 */
abstract class BST[+A: Ordering] {
  def value: A

  def isEmpty: Boolean

  def isLeaf: Boolean = !isEmpty && left.isEmpty && right.isEmpty

  // TODO: Can we do something like https://stackoverflow.com/a/4313266/839733
  def left: BST[A] = BST.empty[A]

  def right: BST[A] = BST.empty[A]

  def isRoot: Boolean = false

  def size: Int = left.size + right.size + 1

  def insert[B >: A](x: B)(implicit ord: Ordering[B]): BST[B] = {
    insertInternal(x, isEmpty)
  }

  def deleteMin(): (Option[A], BST[A]) = {
    if (isEmpty)
      (None, BST.empty[A])
    else if (left.isEmpty)
      (Some(value), delete(value))
    else
      left.deleteMin() match {
        case (None, l) => (None, BST(value, l, right, isRoot))
        case (x, l) => (x, BST(value, l, right, isRoot))
      }
  }

  def delete[B >: A](x: B)(implicit ord: Ordering[B]): BST[A] = {
    if (isEmpty)
      BST.empty[A]
    else {
      if (value == x) {
        Seq(left, right)
          .filter(_.nonEmpty) match {
          case Seq() => BST.empty[A]
          case child +: Seq() => child
          case Seq(_, _) if isRoot =>
            val inorderSucc = inorderSuccessor(value)
            // we need to reduce the sizes of all trees that lie in the path of the inorder successor
            val r = pathToParent(inorderSucc.value)
              .drop(1) // drop root
              // if right subtree of root is the inorder successor, the above path is empty
              .foldRight(inorderSucc) { (parent, child) =>
              val c = if (child.value == inorderSucc.value)
                BST.empty[A]
              else
                child

              if (parent.value > child.value)
                BST(parent.value, c, parent.right, parent.isRoot)
              else
                BST(parent.value, parent.left, c, parent.isRoot)
            }

            BST(inorderSuccessor(value).value, left, r, isRoot = true)
          case Seq(_, _) => inorderSuccessor(value)
        }
      } else if (x < value)
        BST(value, left.delete(x), right, isRoot)
      else
        BST(value, left, right.delete(x), isRoot)
    }
  }

  def inorderPredecessor[B >: A](x: B)(implicit ord: Ordering[B]): BST[A] = {
    find(x) match {
      case y if y.isEmpty => BST.empty[A]
      case y =>
        y.left match {
          // if left subtree is non empty, the maximum element in it is the inorder predecessor
          case l if l.nonEmpty => l.max
          /*
           * search from the root for this element, keeping track of the parent.
           * when we go down a right subtree, the inorder successor of a node is its parent.
           * when we go down a left subtree, the parent has not been visited; we need to go up the tree and
           * find the first ancestor such that the current subtree is part of its right subtree.
           * since we are not maintaining parent pointers, we search from the root for the element whose inorder pred
           * is desired, keeping track of the parent, and saving it whenever we go down a left subtree
           */
          case _ =>
            Iterator.iterate((this, BST.empty[A])) {
              case (e, p) if x < e.value => (e.left, p)
              case (e, _) => (e.right, e)
            }
              .dropWhile(_._1.value != x)
              .map(_._2)
              .take(1)
              .toSeq
              .headOption
              .getOrElse(BST.empty[A])
        }
    }
  }

  def inorderSuccessor[B >: A](x: B)(implicit ord: Ordering[B]): BST[A] = {
    find(x) match {
      case y if y.isEmpty => BST.empty[A]
      case y =>
        y.right match {
          // if right subtree is non empty, the minimum element in it is the inorder successor
          case r if r.nonEmpty => r.min
          /*
           * search from the root for this element, keeping track of the parent.
           * when we go down a left subtree, the inorder successor of a node is its parent.
           * when we go down a right subtree, the parent has already been visited; we need to go up the tree and
           * find the first ancestor such that the current subtree is part of its left subtree.
           * since we are not maintaining parent pointers, we search from the root for the element whose inorder succ
           * is desired, keeping track of the parent, and saving it whenever we go down a right subtree
           */
          case _ =>
            Iterator.iterate((this, BST.empty[A])) {
              case (e, _) if x < e.value => (e.left, e)
              case (e, p) => (e.right, p)
            }
              .dropWhile(_._1.value != x)
              .map(_._2)
              .take(1)
              .toSeq
              .headOption
              .getOrElse(BST.empty[A])
        }
    }
  }

  def min: BST[A] = {
    if (isEmpty)
      BST.empty[A]
    else if (left.isEmpty)
      this
    else
      left.min
  }

  def max: BST[A] = {
    if (isEmpty)
      BST.empty[A]
    else if (right.isEmpty)
      this
    else
      right.max
  }

  def find[B >: A](x: B)(implicit ord: Ordering[B]): BST[A] = {
    if (isEmpty)
      BST.empty[A]
    else {
      if (x < value)
        left.find(x)
      else if (x > value)
        right.find(x)
      else
        this
    }
  }

  def toSeq: Seq[A] = {
    if (isEmpty)
      Seq.empty[A]
    else
      (left.toSeq :+ value) ++ right.toSeq
  }

  override def toString: String = {
    val builder = StringBuilder.newBuilder

    if (nonEmpty) {
      builder.append(value)

      if (left.nonEmpty) {
        builder.insert(0, "<-").insert(0, left.value)
      }

      if (right.nonEmpty) {
        builder.append("->").append(right.value)
      }
    }

    s"(${builder.toString})"
  }

  def nonEmpty: Boolean = !isEmpty

  private def insertInternal[B >: A](x: B, root: Boolean = false)(implicit ord: Ordering[B]): BST[B] = {
    if (isEmpty)
      BST(x, BST.empty[B], BST.empty[B], isRoot = root)
    else {
      if (x < value)
        BST(value, left.insertInternal(x), right, isRoot)
      else
        BST(value, left, right.insertInternal(x), isRoot)
    }
  }

  // constructs a path from 'this' BST to the parent of the tree with the given value x
  private def pathToParent[B >: A](x: B)(implicit ord: Ordering[B]): Iterator[BST[A]] = {
    Iterator.iterate((this, right)) { case (_, subtree) =>
      if (subtree.value == x) (subtree, BST.empty[A])
      else if (x < subtree.value) (subtree, subtree.left)
      else (subtree, subtree.right)
    }
      .map(_._1)
      .takeWhile(_.value != x)
  }
}

private sealed case class Tree[+A: Ordering](
                                              override val value: A,
                                              override val left: BST[A],
                                              override val right: BST[A],
                                              override val isRoot: Boolean,
                                            ) extends BST[A] {
  override val isEmpty: Boolean = false
}

private sealed case class NilTree[+A: Ordering]() extends BST[A] {
  override val size: Int = 0
  override val isEmpty: Boolean = true

  override def value: A = throw new NoSuchElementException()
}

object BST {
  def apply[A: Ordering](
                          value: A,
                          left: BST[A],
                          right: BST[A],
                          isRoot: Boolean
                        ): BST[A] = Tree[A](value, left, right, isRoot)

  def apply[A: Ordering](xs: A*): BST[A] = {
    xs.foldLeft(BST.empty[A])((acc, e) => acc.insert(e))
  }

  // TODO: Memoize?
  def empty[A: Ordering]: BST[A] = NilTree[A]()
}
