package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
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

  test("times(\"abcdac\")") {
    print(times(string2Chars("abcdac")))
    assert(times(string2Chars("abcdac")) === List(('a', 2), ('b', 1), ('c', 2), ('d', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1), Leaf('t',2), List('e', 't'), 3), Leaf('x',4), List('e','t','x'), 7)))
  }

  test("create code tree") {
    val leaflist = "ettxxxx"
    assert(createCodeTree(string2Chars(leaflist)) === Fork(Fork(Leaf('e',1), Leaf('t',2), List('e', 't'), 3), Leaf('x',4), List('e','t','x'), 7))
  }


  test("encode a very short text should be identity") {
    new TestTrees {
      println(encode(t1)("abaabbb".toList))
      assert(encode(t1)("abaabbb".toList) === List(0,1,0,0,1,1,1))
    }
  }

  test("decode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, List(0,1)) === "ab".toList)
    }
  }

  test("decode and encode a text should be identity") {
    new TestTrees {
      val strings = "thisisthebesttesttotestwith".toList
      val tree = createCodeTree(strings)
      assert(decode(tree, encode(tree)(strings)) === strings)
    }
  }

  test("decode and encode fast a text should be identity") {
    new TestTrees {
      val strings = "The quick brown fox jumps over the lazy dog".toList
      val tree = createCodeTree(strings)
      println(tree)
      println(quickEncode(tree)(strings))
      println(quickEncode(tree)(strings).length)
//      println(quickEncode(tree)("a".toList))
//      println(quickEncode(tree)("b".toList))
//      println(quickEncode(tree)("c".toList))
//      println(quickEncode(tree)("d".toList))
//      println(quickEncode(tree)("e".toList))
//      println(quickEncode(tree)("f".toList))
//      println(quickEncode(tree)("g".toList))
//      println(quickEncode(tree)("h".toList))
      assert(decode(tree, quickEncode(tree)(strings)) === strings)
    }
  }

}
