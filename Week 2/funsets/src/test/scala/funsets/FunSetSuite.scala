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
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
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

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("unions can be combined") {
    val set35 = union(singletonSet(3), singletonSet(5))
    val set57 = union(singletonSet(5), singletonSet(7))
    val unionSet = union(set35, set57)
    assert(contains(unionSet, 3))
    assert(contains(unionSet, 5))
    assert(contains(unionSet, 7))
    assert(!contains(unionSet, 6))
    assert(!contains(unionSet, 0))
  }
  
  test("intersection of distinct sets does not include either element") {
    val set1 = singletonSet(1)
    val set3 = singletonSet(3)
    val intersectSet = intersect(set1, set3)
    assert(!contains(intersectSet, 1))
    assert(!contains(intersectSet, 2))
    assert(!contains(intersectSet, 3))
  }
  
  test("intersection of overlapping sets only returns the values included in both sets") {
    val set35 = union(singletonSet(3), singletonSet(5))
    val set57 = union(singletonSet(5), singletonSet(7))
    val intersectSet = intersect(set35, set57)
    assert(!contains(intersectSet, 3))
    assert(contains(intersectSet, 5))
    assert(!contains(intersectSet, 7))
  }
    
  test("diff of identical sets has no members") {
    val set = singletonSet(1)
    val identicalSet = singletonSet(1)
    val diffSet = diff(set, identicalSet)
    assert(!contains(diffSet, 1))
  }
    
  test("diff of distinct sets only returns first set values") {
    val set1 = singletonSet(1)
    val set3 = singletonSet(3)
    val diffSet = diff(set1, set3)
    assert(contains(diffSet, 1))
    assert(!contains(diffSet, 3))
  }
  
  test("diff of overlapping sets only returns the values only present in the first set") {
    val set35 = union(singletonSet(3), singletonSet(5))
    val set57 = union(singletonSet(5), singletonSet(7))
    val diffSet = diff(set35, set57)
    assert(contains(diffSet, 3))
    assert(!contains(diffSet, 5))
    assert(!contains(diffSet, 7))
  }
  
  val set35 = union(singletonSet(3), singletonSet(5))
  val set57 = union(singletonSet(5), singletonSet(7))
  val unionSet = union(set35, set57)
  def emptySet : Set = x => false

  test("filtering false returns no results") {
    val filterSet = filter(unionSet, x => false)
    assert(!contains(filterSet, 3))
    assert(!contains(filterSet, 5))
    assert(!contains(filterSet, 7))
  }
  
  test("filtering only returns results matching the filter") {
    val filterSet = filter(unionSet, x => (x > 5))
    assert(!contains(filterSet, 3))
    assert(!contains(filterSet, 5))
    assert(contains(filterSet, 7))
  }
  
  test("filtering does not include values not originally in the set") {
    val filterSet = filter(unionSet, x => true)
    assert(contains(filterSet, 3))
    assert(contains(filterSet, 5))
    assert(contains(filterSet, 7))
    assert(!contains(filterSet, 9))
  }
  
  test("forAll searches the whole set") {
    assert(forall(unionSet, x => (x > 0)))
    assert(!forall(unionSet, x => (x > 4)))
  }
  
  test("forAll rejects and accepts explicitly") {
    assert(!forall(unionSet, x => false))
    assert(forall(unionSet, x => true))
  }
      
  test("forAll has to match the whole set, not just a part") {
    assert(!forall(unionSet, x => (x == 5)))
  }

  test("forAll accepts if set is empty") {
    assert(forall(emptySet, x => true))
  }
  
  test("exists is satisfied if all elements pass") {
    assert(exists(unionSet, x => true))
  }
  
  test("exists is not satisfied if all elements fail") {
    assert(!exists(unionSet, x => x < 0))
  }

  test("exists is satisfied if only some elements pass") {
    assert(exists(unionSet, x => (x > 5)))
  }
  
  test("nothing exists in the empty set") {
    assert(!exists(emptySet, x => true))
  }
  
  test("map with identify function is original input") {
    val mappedSet = map(unionSet, x => x)
    assert(contains(mappedSet, 3))
    assert(contains(mappedSet, 5))
    assert(contains(mappedSet, 7))
    assert(!contains(mappedSet, 9))
  }
  
  test("map with squaring function squares each value") {
    val mappedSet = map(unionSet, x => x * x)
    assert(contains(mappedSet, 9))
    assert(contains(mappedSet, 25))
    assert(contains(mappedSet, 49))
    assert(!contains(mappedSet, 3))
    assert(!contains(mappedSet, 5))
  }
  
  test("map of empty set is empty set") {
    val mappedSet = map(emptySet, x => x)
    assert(!exists(mappedSet, x => true))
  }
  
  test("map doubling numbers forall set") {
    val mappedSet = map(unionSet, x => x * 2)
    forall(mappedSet, x => x > 0)
    assert(true)
  }
 
}
