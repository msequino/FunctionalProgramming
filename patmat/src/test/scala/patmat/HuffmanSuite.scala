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

  test("times *** ") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('b', 1),('a', 2)))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list (one element)") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e',1)))
  }
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  test("combine of some leaf list (right order)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 2))
    assert(combine(leaflist) === List(Leaf('x',2),Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }


  test("until of some leaf list (one element)") {
    val leaflist = List(Leaf('e', 1))
    assert(until(singleton, combine)(leaflist) === List(Leaf('e',1)))
  }
  test("until of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }
  test("until of some leaf list (right order)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 2))
    assert(until(singleton, combine)(leaflist) === List(Fork(Leaf('x',2),Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),List('x', 'e', 't'),5)))
  }


  /*test("Test: My simple test") {
    new TestTrees {
      println(createCodeTree("My simple test".toLowerCase.replaceAll("\\s", "").toList))
    }
  }*/

  test("decode secret") {
    new TestTrees {
      println(decodedSecret mkString "")
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode/encode using FrencCode") {
    new TestTrees {
      val test = "nuit"
      assert(decode(frenchCode, encode(frenchCode)(test.toList)) === test.toList)
    }
  }

  test("convert french code to codebit") {
    new TestTrees {
      println(convert(frenchCode))
    }
  }

  test("decode secret with codeTable") {
    new TestTrees {
      assert(quickEncode(frenchCode)(decode(frenchCode, secret)) === secret)
    }
  }


}
