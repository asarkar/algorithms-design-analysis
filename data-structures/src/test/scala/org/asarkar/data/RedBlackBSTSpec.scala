package org.asarkar.data

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks


class RedBlackBSTSpec extends FlatSpec with TableDrivenPropertyChecks {
  // src/test/resources/red-black-bst.jpg
  private val xs = Seq('S', 'E', 'A', 'R', 'C', 'H', 'X', 'M', 'P', 'L')
  private val bst = RedBlackBST(xs: _*)

  "RedBlackBST" should "be empty when created" in {
    val nil = RedBlackBST.empty[Int]
    nil.isEmpty shouldBe true
    nil.left.isEmpty shouldBe true
    nil.right.isEmpty shouldBe true
    a[NoSuchElementException] should be thrownBy nil.value
    nil.min.isEmpty shouldBe true
    nil.isLeaf shouldBe false
    nil.find(1).isEmpty shouldBe true
    nil.toSeq shouldBe empty
    nil.isRoot shouldBe false
    nil.isRed shouldBe false
    nil.size shouldBe 0
  }

  it should "insert given elements" in {
    bst.toSeq shouldBe xs.sorted
    bst.size shouldBe xs.size
  }

  it should "not support deleting given element" in {
    a[NotImplementedError] should be thrownBy bst.delete('A')
  }

  it should "find inorder successor" in {
    val data =
    // format: off
      Table(("e", "succ", "empty"),
        ('A', 'C', false),
        ('M', 'P', false),
        ('R', 'S', false)
      )
    // format: on

    forAll(data) { (e, succ, empty) =>
      val inorder = bst.inorderSuccessor(e)
      inorder.isEmpty shouldBe empty
      inorder.value shouldBe succ
    }
  }

  it should "find the minimum element" in {
    val min = bst.min
    min.isEmpty shouldBe false
    min.value shouldBe 'A'
  }

  it should "not support deleting the minimum element" in {
    a[NotImplementedError] should be thrownBy bst.deleteMin()
  }

  it should "find an element" in {
    bst.find('L').toSeq should contain inOrderOnly('H', 'L')
    bst.find('Z').isEmpty shouldBe true
  }
}
