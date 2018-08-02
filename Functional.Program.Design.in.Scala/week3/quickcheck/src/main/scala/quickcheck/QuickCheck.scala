package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min two elements") = forAll {(a1: Int, a2: Int) => {
    val h = insert(a2, insert(a1, empty))
    findMin(h) == math.min(a1, a2)
  }}

  property("empty insert delete empty") = forAll { a: Int => {
    val h = insert(a, empty)
    deleteMin(h) == empty
  }}

  property("meld any two heaps and findMin") = forAll{(h1: H, h2: H) => {
    if(!isEmpty(h1) && !isEmpty(h2)){
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      findMin(meld(h1, h2)) == math.min(min1, min2)
    } else if(!isEmpty(h1))
      findMin(meld(h1, h2)) == findMin(h1)
    else if(!isEmpty(h2))
      findMin(meld(h1, h2)) == findMin(h2)
    else true
  }}

  property("drop min to get ordered sequence") = forAll{ h: H => {
    def getMinSeq(h: H): Seq[A] = {
      if(isEmpty(h))
        Seq()
      else Seq(findMin(h)) ++ getMinSeq(deleteMin(h))
    }

    if(!isEmpty(h)){
      val minSeq = getMinSeq(h)
      minSeq == minSeq.sorted
    } else true
  }}

  property("meld two heaps") = forAll{(h1: H, h2: H) => {
    if(!isEmpty(h1) && !isEmpty(h2)){
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      findMin(meld(h1, h2)) == math.min(min1, min2)
    } else if(!isEmpty(h1))
      findMin(meld(h1, h2)) == findMin(h1)
    else if(!isEmpty(h2))
      findMin(meld(h1, h2)) == findMin(h2)
    else true
  }}

  property("meld two elements to two empty heaps; return max after findMin(deleteMin)") = forAll{(a1: A, a2: A) => {
    val h = meld(insert(a2, empty), insert(a1, empty))
    findMin(deleteMin(h)) == math.max(a1, a2)
  }}

  property("min after insert element gt min of a random heap should yield min") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a+1 else a
    val h1 = insert(b+1, insert(b, insert(b+2, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(b+2, insert(b+1, empty))
  }

}
