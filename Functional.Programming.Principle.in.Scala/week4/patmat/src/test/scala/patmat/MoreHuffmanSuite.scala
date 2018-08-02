package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class MoreHuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t4 = makeCodeTree(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
      Leaf('d', 4))
    val t3 = Fork(
      Leaf('a',8),
      Fork(
        Fork(
          Leaf('b', 3),
          Fork(Leaf('c',1), Leaf('d',1),"cd".toList,2),
          "bcd".toList, 5),
        Fork(
          Fork(Leaf('e',1), Leaf('f',1),"ef".toList,2),
          Fork(Leaf('g',1), Leaf('h',1),"gh".toList,2),
          "efgh".toList,4),
        "bcdefgh".toList,9),
      "abcdefgh".toList,17)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("times chars") {
    assert(times("aaaaaaaabbbcdefgh".toList) === List(('e',1), ('f',1), ('a',8), ('b',3), ('g',1), ('c',1), ('h',1), ('d',1)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("makeOrderedLeafList") {
    val leaflist = List(('e',1), ('f',1), ('a',8), ('b',3), ('g',1), ('c',1), ('h',1), ('d',1))
    assert(makeOrderedLeafList(leaflist) === List(Leaf('e',1), Leaf('f',1), Leaf('g',1), Leaf('c',1), Leaf('h',1), Leaf('d',1), Leaf('b',3), Leaf('a',8)))
  }

  test("combine leafs") {
    val leafs = List(Leaf('e',1), Leaf('f',1), Leaf('g',1), Leaf('c',1), Leaf('h',1), Leaf('d',1), Leaf('b',3), Leaf('a',8))
    assert(combine(leafs) === List(Leaf('g',1), Leaf('c',1), Leaf('h',1), Leaf('d',1), Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2), Leaf('b',3), Leaf('a',8)))
  }

  test("combine all leafs") {
    val leafs = List(Leaf('e',1), Leaf('f',1), Leaf('g',1), Leaf('c',1), Leaf('h',1), Leaf('d',1), Leaf('b',3), Leaf('a',8))
    assert(until(singleton, combine)(leafs) === List(Fork(Leaf('a',8),Fork(Fork(Fork(Leaf('h',1),Leaf('d',1),List('h', 'd'),2),Fork(Leaf('g',1),Leaf('c',1),List('g', 'c'),2),List('h', 'd', 'g', 'c'),4),Fork(Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),Leaf('b',3),List('e', 'f', 'b'),5),List('h', 'd', 'g', 'c', 'e', 'f', 'b'),9),List('a', 'h', 'd', 'g', 'c', 'e', 'f', 'b'),17)))
  }

  test("create code tree from list of chars") {
    assert(createCodeTree("aaaaaaaabbbcdefgh".toList) === Fork(Leaf('a',8),Fork(Fork(Fork(Leaf('h',1),Leaf('d',1),List('h', 'd'),2),Fork(Leaf('g',1),Leaf('c',1),List('g', 'c'),2),List('h', 'd', 'g', 'c'),4),Fork(Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),Leaf('b',3),List('e', 'f', 'b'),5),List('h', 'd', 'g', 'c', 'e', 'f', 'b'),9),List('a', 'h', 'd', 'g', 'c', 'e', 'f', 'b'),17))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a some text should be identity") {
    new TestTrees {
      assert(decode(t3, encode(t3)("hehedageca".toList)) === "hehedageca".toList)
    }
  }

  test("decode very short codes should work") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === "ab".toList)
    }
  }

  test("encode very short codes should work") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("encode codes should work") {
    new TestTrees {
      assert(encode(t3)("hehedageca".toList) === List(1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0))
    }
  }

  test("quick encode very short codes should work") {
    new TestTrees {
      assert(quickEncode(t3)("hehedageca".toList) === List(1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0))
    }
  }

  test("decode secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }
}