package org.asarkar.data

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks


class BSTSpec extends FlatSpec with TableDrivenPropertyChecks {
  // src/test/resources/bst.png
  private val xs = Seq(15, 10, 5, 1, 7, 8, 20, 17, 25)
  private val bst = BST(xs: _*)

  "BST" should "be empty when created" in {
    val nil = BST.empty[Int]
    nil.isEmpty shouldBe true
    nil.left.isEmpty shouldBe true
    nil.right.isEmpty shouldBe true
    a[NoSuchElementException] should be thrownBy nil.value
    nil.min.isEmpty shouldBe true
    nil.isLeaf shouldBe false
    val (min, tree) = nil.deleteMin()
    min shouldBe empty
    tree.isEmpty shouldBe true
    nil.delete(1).isEmpty shouldBe true
    nil.find(1).isEmpty shouldBe true
    nil.toSeq shouldBe empty
    nil.isRoot shouldBe false
    nil.size shouldBe 0
  }

  it should "insert given elements" in {
    bst.toSeq shouldBe xs.sorted
  }

  it should "delete given element" in {
    val bst1 = bst.delete(1)
    bst1.size shouldBe xs.size - 1
    bst1.toSeq should contain inOrderOnly(5, 7, 8, 10, 15, 17, 20, 25)

    val bst2 = bst1.delete(7)
    bst2.size shouldBe xs.size - 2
    bst2.toSeq should contain inOrderOnly(5, 8, 10, 15, 17, 20, 25)

    val bst3 = bst2.delete(15)
    bst3.size shouldBe xs.size - 3
    bst3.toSeq should contain inOrderOnly(5, 8, 10, 17, 20, 25)
  }

  it should "find inorder successor" in {
    val data =
    // format: off
      Table(("e", "succ", "empty"),
        (1, 5, false),
        (8, 10, false),
        (15, 17, false),
        (25, -1, true)
      )
    // format: on

    forAll(data) { (e, succ, empty) =>
      val inorder = bst.inorderSuccessor(e)
      inorder.isEmpty shouldBe empty
      if (!empty) {
        inorder.value shouldBe succ
      }
    }
  }

  it should "find inorder predecessor" in {
    val data =
    // format: off
      Table(("e", "pred", "empty"),
        (5, 1, false),
        (10, 8, false),
        (17, 15, false),
        (1, -1, true)
      )
    // format: on

    forAll(data) { (e, pred, empty) =>
      val inorder = bst.inorderPredecessor(e)
      inorder.isEmpty shouldBe empty
      if (!empty) {
        inorder.value shouldBe pred
      }
    }
  }

  it should "find the minimum element" in {
    val min = bst.min
    min.isEmpty shouldBe false
    min.value shouldBe 1
  }

  it should "find the maximum element" in {
    val max = bst.max
    max.isEmpty shouldBe false
    max.value shouldBe 25
  }

  it should "delete the minimum element" in {
    val (min1, tree1) = bst.deleteMin()
    tree1.size shouldBe xs.size - 1
    min1 should contain(1)
    tree1.toSeq should contain inOrderOnly(5, 7, 8, 10, 15, 17, 20, 25)

    val (min2, tree2) = tree1.deleteMin()
    tree2.size shouldBe xs.size - 2
    min2 should contain(5)
    tree2.toSeq should contain inOrderOnly(7, 8, 10, 15, 17, 20, 25)

    val (min3, tree3) = tree2.deleteMin()
    tree3.size shouldBe xs.size - 3
    min3 should contain(7)
    tree3.toSeq should contain inOrderOnly(8, 10, 15, 17, 20, 25)
  }

  it should "find an element" in {
    bst.find(7).toSeq should contain inOrderOnly(7, 8)
    bst.find(9).isEmpty shouldBe true
  }
}
