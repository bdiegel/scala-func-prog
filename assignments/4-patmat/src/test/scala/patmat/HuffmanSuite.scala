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
    
    val l1 = List( 'a','a','c','g','g','g', 't','t')
    val l1Times = List( ('a',2), ('c',1), ('g',3), ('t',2) )
    //val tree1 = Fork(Fork(Leaf('c',1),Leaf('a',2),List('c', 'a'),3),Fork(Leaf('t',2),Leaf('g',3),List('t', 'g'),5),List('c', 'a', 't', 'g'),8)
    val tree1 = Fork(Fork(Leaf('c',1),Leaf('t',2),List('c', 't'),3),Fork(Leaf('a',2),Leaf('g',3),List('a', 'g'),5),List('c', 't', 'a', 'g'),8)
    val s = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
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

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("mytest: util-singleton test") {
  	  val result = Huffman.until(Huffman.singleton, Huffman.combine)(List( Huffman.Leaf('a',1) ))
	  assert( result.length == 1)
	  
	  val result2 = Huffman.until(Huffman.singleton, Huffman.combine)(List( Huffman.Leaf('a',1), Huffman.Leaf('b',1), Huffman.Leaf('c',1) ))
	  assert( result2.length == 1)
  }
  
  test("mytest: secret test") {
    new TestTrees {
      assert(Huffman.decodedSecret == s)
    }
  }

  test("mytest: times test") {
    new TestTrees {
      assert(Huffman.times(l1) == l1Times)
    }
  }
  
  test("mytest: createCodeTree test") {
	 new TestTrees {
		 assert( tree1 == Huffman.createCodeTree( l1 ))
	 }
  }

  test("mytest: encode 'cat'") {
	  new TestTrees {	  
    	  val encodeTree1 = Huffman.encode( tree1 ) _    	  
    	  assert( encodeTree1 (List('c','a','t') ) == List(0, 0, 1, 0, 0, 1) )
    }
  }
  
  test( "mytest: codetable test" ) {
	  new TestTrees {
	    assert( Huffman.convert( tree1 ) ==  List(('c',List(0, 0)), ('t',List(0, 1)), ('a',List(1, 0)), ('g',List(1, 1))) )
	  }
  }
  
  test( "mytest: quickencode test" ) {
	  new TestTrees {
	    assert( Huffman.quickEncode( tree1 )( List('c','a','t') ) == List(0, 0, 1, 0, 0, 1) )
	  }
  }

}
