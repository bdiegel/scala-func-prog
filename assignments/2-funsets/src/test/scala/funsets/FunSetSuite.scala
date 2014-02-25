package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }
  
  test("Testing singletonSet") {
 
    new TestSets {
      assert(!contains(s1, 2), "singletonSet(1) does not contains 2")
      assert(contains(s2, 2), "singletonSet(2) does contains 2")
    }
  }
  
  test("Testing union") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("Testing intersect") {
    new TestSets {
      val u12 = union(s1, s2)
      val u13 = union(s1, s3)
      val i1 = intersect( u12, u13)
      val i3 = intersect( u13, s3)
      assert(contains(i1, 1), "intersect 1")
      assert(contains(i3, 3), "intersect 2")
      assert(!contains(i1, 2), "intersect 3")
      assert(!contains(i3, 1), "intersect 4")
    }
  }
  
  test("Testing diff") {
    new TestSets {
      val u12 = union(s1, s2)
      val u13 = union(s1, s3)
      assert( contains( diff(u12, s1), 2 ), "diff 1")
      assert( contains( diff(u12, s2), 1 ), "diff 2")
      assert( !contains( diff(u12, s1), 1 ), "diff 3")
      assert( !contains( diff(u13, s1), 1 ), "diff 4")
      assert( contains( diff(u13, s1), 3 ), "diff 5")
      assert( !contains( diff(u13, s1), 2 ), "diff 6")
    }
  }
  
  test("Testing filter") {
    new TestSets {
      val u12 = union(s1, s2)
      val u13 = union(s1, s3)
      val u123 = union(u12, s3)
      
      
      assert( contains( filter(u12, s1), 1 ), "filter 1")
      assert( contains( filter(u12, (x => x != 2)), 1 ), "filter 2")
      assert( contains( filter(u123, (x => x < 3)), 2 ), "filter 3")
      assert( contains( filter(u123, (x => x < 3)), 1 ), "filter 4")
      assert( !contains( filter(u123, (x => x < 3)), 3 ), "filter 5")

    }
  }
  
  
    
  test("Testing forall") {
    new TestSets {
      val u12 = union(s1, s2)
      val u13 = union(s1, s3)
      val u123 = union(u12, s3)
      val u1234 = union(u123, s4)
      
      val pEven = ( (x: Int) => x % 2 == 0 )
      val pOdd = ( (x: Int) => x % 2 != 0 )
      val p123 = ( (x: Int) => x >= 1 && x <= 3 )
      
      assert( forall(u123, p123), "forall 1")
      assert( !forall(u123, pEven), "forall 2")
      assert( forall(pEven, pEven), "forall 3")      
      assert( !forall(u123, (x => x == 90)), "forall 4")
      assert( !forall(pEven, pOdd), "forall 5") 
      assert( forall(union(s2, s4), pEven), "forall 6")
      assert( !forall(union(s2, s3), pOdd), "forall 7")
      
      assert( forall(union(singletonSet(-20), singletonSet(-100)), pEven), "forall 8")
      assert( forall(union(singletonSet(-20), singletonSet(0)), pEven), "forall 9")
    }
  }
  
  test("Testing exists") {
    new TestSets {
      val u12 = union(s1, s2)
      val u13 = union(s1, s3)
      val u123 = union(u12, s3)
      val u1234 = union(u123, s4)
      
      val pEven = ( (x: Int) => x % 2 == 0 )
      val pOdd = ( (x: Int) => x % 2 != 0 )
      val pNegative = ( (x: Int) => x < 0 )
      val p123 = ( (x: Int) => x >= 1 && x <= 3 )
      
      assert( exists(u123, pEven), "exists 1")
      assert( !exists(s4, p123), "exists 2")      
      assert( exists(union(u1234, singletonSet(-100)), pNegative), "exists 3")
      assert( !exists(union(u1234, singletonSet(100)), pNegative), "exists 3")
    }
  }
  test("Testing map") {
    new TestSets {
      val u12 = union(s1, s2)
      val u23 = union(s2, s3)
      val u1234 = union( union( union(s1, s2), s3), s4 )

      assert( contains( map(u12, x => x + 1), 3 ), "map 1")
      assert( !contains( map(u12, x => x + 1), 1 ), "map 2")
      assert( contains( map(u12, x => x * 2), 4 ), "map 3")
      assert( !contains( map(u12, x => x * 2), 0 ), "map 4")
      assert( contains( map(u23, x => x * x), 4 ), "map 5")
      assert( contains( map(u23, x => x * x), 9 ), "map 6")
      assert( !contains( map(u23, x => x * x), 2 ), "map 7")
      
      assert( contains( map(u1234, x => x * x), 9 ), "map 8")
      assert( contains( map(u1234, x => x * x), 4 ), "map 9")
      assert( !contains( map(u1234, x => x * x), 3 ), "map 10")
      assert( !contains( map(u1234, x => x * x), 5 ), "map 11")
    }
  }
  

//  ignore("union contains all elements") {
//    new TestSets {
//      val s = union(s1, s2)
//      assert(contains(s, 1), "Union 1")
//      assert(contains(s, 2), "Union 2")
//      assert(!contains(s, 3), "Union 3")
//    }
//  }
}
