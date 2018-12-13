package org.asarkar.data

import scala.math.Ordering.Implicits._

// https://github.com/scala/scala/blob/2.12.x/src/library/scala/collection/immutable/RedBlackTree.scala
abstract class RedBlackBST[+A: Ordering] extends BST[A] {
  def isRed: Boolean = true

  override def left: RedBlackBST[A] = RedBlackBST.empty[A]

  override def right: RedBlackBST[A] = RedBlackBST.empty[A]

  override def deleteMin(): (Option[A], RedBlackBST[A]) = ???

  override def delete[B >: A](x: B)(implicit ord: Ordering[B]): RedBlackBST[A] = ???

  override def insert[B >: A](x: B)(implicit ord: Ordering[B]): RedBlackBST[B] = {
    insertInternal(x, isEmpty)
  }

  private def insertInternal[B >: A](x: B, root: Boolean = false, red: Boolean = false)(implicit ord: Ordering[B]): RedBlackBST[B] = {
    if (isEmpty)
      RedBlackBST(x, RedBlackBST.empty[B], RedBlackBST.empty[B], root, red)
    else {
      val h = if (x < value)
        RedBlackBST(value, left.insertInternal(x, false, true), right, isRoot, isRed)
      else
        RedBlackBST(value, left, right.insertInternal(x, false, true), isRoot, isRed)

      flipColors(rotateRight(rotateLeft(h)))
    }
  }

  private def flipColors[B >: A](tree: RedBlackBST[B])(implicit ord: Ordering[B]): RedBlackBST[B] = {
    if (tree.left.isRed && tree.right.isRed) {
      RedBlackBST(
        tree.value,
        RedBlackBST(tree.left.value, tree.left.left, tree.left.right, tree.left.isRoot, isRed = false),
        RedBlackBST(tree.right.value, tree.right.left, tree.right.right, tree.right.isRoot, isRed = false),
        tree.isRoot,
        !tree.isRoot
      )
    } else tree
  }

  private def rotateRight[B >: A](tree: RedBlackBST[B])(implicit ord: Ordering[B]): RedBlackBST[B] = {
    if (tree.left.isRed && tree.left.left.isRed) {
      RedBlackBST(
        tree.left.value,
        tree.left.left,
        RedBlackBST(tree.value, tree.left.right, tree.right, tree.right.isRoot, isRed = true),
        tree.isRoot,
        !tree.isRoot && tree.isRed
      )
    } else tree
  }

  private def rotateLeft[B >: A](tree: RedBlackBST[B])(implicit ord: Ordering[B]): RedBlackBST[B] = {
    if (tree.right.isRed && !tree.left.isRed) {
      RedBlackBST(
        tree.right.value,
        RedBlackBST(tree.value, tree.left, tree.right.left, tree.left.isRoot, isRed = true),
        tree.right.right,
        tree.isRoot,
        !tree.isRoot && tree.isRed
      )
    } else tree
  }
}

private sealed case class RedBlackTree[+A: Ordering](
                                                      override val value: A,
                                                      override val left: RedBlackBST[A],
                                                      override val right: RedBlackBST[A],
                                                      override val isRoot: Boolean,
                                                      override val isRed: Boolean
                                                    ) extends RedBlackBST[A] {
  override val isEmpty: Boolean = false
}

private sealed case class NilRedBlackTree[+A: Ordering]() extends RedBlackBST[A] {
  override val isRed: Boolean = false

  override val size: Int = 0
  override val isEmpty: Boolean = true

  override def value: A = throw new NoSuchElementException()
}

object RedBlackBST {
  def apply[A: Ordering](
                          value: A,
                          left: RedBlackBST[A],
                          right: RedBlackBST[A],
                          isRoot: Boolean,
                          isRed: Boolean
                        ): RedBlackBST[A] = RedBlackTree[A](value, left, right, isRoot, isRed)

  def apply[A: Ordering](xs: A*): RedBlackBST[A] = {
    xs.foldLeft(RedBlackBST.empty[A])((acc, e) => acc.insert(e))
  }

  def empty[A: Ordering]: RedBlackBST[A] = NilRedBlackTree[A]()
}
