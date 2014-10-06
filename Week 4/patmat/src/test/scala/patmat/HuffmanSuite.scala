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
    
    val efTree = Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2);
    val ghTree = Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'), 2);
    val cdTree = Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2);
    val cdefTree = Fork(cdTree, efTree, List('c', 'd', 'e', 'f'), 4);
    val bghTree = Fork(ghTree, Leaf('b', 3), List('g', 'h', 'b'), 5);
    val bcdefghTree = Fork(cdefTree, bghTree, List('c', 'd', 'e', 'f', 'g', 'h', 'b'), 9);
    val fullTree = Fork(Leaf('a', 8), bcdefghTree, List('a', 'c', 'd', 'e', 'f', 'g', 'h', 'b'), 17);
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  
  test("weight of the example tree") {
    new TestTrees {
      assert(weight(fullTree) === 17)
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(new Leaf('a', 3)) === List('a'))
    }
  }
  
  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("chars of the example tree") {
    new TestTrees {
      assert(chars(fullTree) === List('a', 'c', 'd', 'e', 'f', 'g', 'h', 'b'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times lists frequency of character in the text") {
    //Given:
    val charList = string2Chars("hello, world");
    
    //Then:
    val expectedTimesList = List(('w', 1), ('r', 1), ('o', 2), ('l', 3), ('h', 1), ('e', 1), ('d', 1), (',', 1), (' ', 1))
    assert(times(charList) === expectedTimesList)
  }
  
  test("makeOrderedLeafList is ordered by leaf weight") {
    //Given:
    val charList = string2Chars("hello, world");
    val weightList = times(charList);
    
    //Then:
    val expectedLeafList = List(
        new Leaf(' ', 1), new Leaf(',', 1), new Leaf('d', 1),
        new Leaf('e', 1), new Leaf('h', 1), new Leaf('r', 1),
        new Leaf('w', 1), new Leaf('o', 2), new Leaf('l', 3))
    assert(makeOrderedLeafList(weightList) === expectedLeafList)
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton detects single trees in a list") {
    assert(singleton(List(new Leaf('a', 1))));
  }
  
  test("singleton rejects multiple trees in a list") {
    //Given:
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    
    //Then:
    assert(!singleton(leafList));
  }
  
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("createCodeTree results in a single tree") {
    val leafList = string2Chars("ettxxxx");
    val expectedTree = Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)
    assert(createCodeTree(leafList) === expectedTree);
  }

  test("createCodeTree results in the example tree") {
      new TestTrees {
        val leafList = string2Chars("aaaaaaaabbbcdefgh");
        assert(createCodeTree(leafList) === fullTree);
      }
  }
  
  test("decode D from tree") {
    new TestTrees {
      assert(decode(fullTree, List(1,0,0,1)) === List('d')) 
    }
  }
  
  test("verify secret code") {
    assert(decodedSecret === string2Chars("huffmanestcool"));
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
