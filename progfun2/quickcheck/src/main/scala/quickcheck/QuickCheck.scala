package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      value <- arbitrary[A]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(value, heap)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a : A) =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (a1: A, a2: A) =>
    findMin(insert(a1, insert(a2, empty))) == a1.min(a2)
  }

  property("empty") = forAll { (h: H, a: A) =>
    if isEmpty(h) then {
      isEmpty(deleteMin(insert(a, h)))
    } else true
  }

  def loopCheckSorted(hh: H, lastMin: A): Boolean = {
    if isEmpty(hh) then true
    else {
      val m = findMin(hh)
      if m < lastMin then false
      else loopCheckSorted(deleteMin(hh), m)
    }
  }

  property("sorted") = forAll { (h: H) =>
    if isEmpty(h) then true
    else loopCheckSorted(deleteMin(h), findMin(h))
  }

  property("sorted meld") = forAll {(h1: H, h2: H) =>
    val h = meld(h1, h2)
    if isEmpty(h) then true
    else loopCheckSorted(deleteMin(h), findMin(h))
  }

  property("sorted insert empty") = forAll { (a1: A, a2: A, a3: A) =>
    val h = insert(a1, insert(a2, insert(a3, empty)))
    loopCheckSorted(h, a1.min(a2).min(a3))
  }

  property("delete min 3") = forAll { (a1: A, a2: A, a3: A) =>
    val h = insert(a1, insert(a2, insert(a3, empty)))
    findMin(deleteMin(deleteMin(h))) == a1.max(a2).max(a3)
  }
  property("delete min 2") = forAll { (a1: A, a2: A) =>
    val h = insert(a1, insert(a2, empty))
    findMin(deleteMin(h)) == a1.max(a2)
  }

  property("delete min 1") = forAll { (a: A, h: H) =>
    val h1 = insert(a, insert(a, h))
    val m2 = findMin(h1)
    findMin(h1) == findMin(deleteMin(insert(m2, h1)))
  }

  property("find min of meld") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => true
      case (true, false) => findMin(h) == findMin(h2)
      case (false, true) => findMin(h) == findMin(h1)
      case (false, false) => findMin(h) == findMin(h1).min(findMin(h2))
    }
  }

