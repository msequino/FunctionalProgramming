package quickcheck

import org.scalacheck._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      a <- Arbitrary.arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMindBetweenTwo") = forAll { (a1: A, a2: A) =>
    findMin(insert(a2, insert(a1, empty))) == Math.min(a1, a2)
  }

  property("findInEmptyHeap") = forAll { (a: A) =>
    empty == deleteMin(insert(a, empty))
  }

  property("findRecursively") = forAll { (h: H) =>
    @tailrec
    def findAndDelete(heap: H, a: Seq[A]) : Seq[A] = {
      if(isEmpty(heap)) {
        a
      } else {
        val min = findMin(heap)
        findAndDelete(deleteMin(heap), a :+ min)
      }
    }

    val sortedSeq = findAndDelete(h, Nil)
    sortedSeq.sorted(ord) == sortedSeq
  }

  property("minimumInMelding") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("deleting empty heap") = forAll { (h: H) =>
    @tailrec
    def deleteRec(heap: H) : H = {
      if(isEmpty(heap)) {
        empty
      } else {
        deleteRec(deleteMin(heap))
      }
    }
    isEmpty(deleteRec(h))
  }

  property("detecting bug in bogus(3)") = forAll { (a: A) =>
    val h = insert(2, insert(4, insert(6, empty)))

    findMin(h) == 2
    val x1 = deleteMin(h)
    findMin(x1) == 4
    val x2 = deleteMin(x1)
    findMin(x2) == 6
  }
  property("detecting bug in bogus(4)") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H) : Boolean = {
      if(isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    }

    val meld1 = meld(h1, h2)
    val minH1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(minH1, h2))

    heapEqual(meld1, meld2)

  }
}
