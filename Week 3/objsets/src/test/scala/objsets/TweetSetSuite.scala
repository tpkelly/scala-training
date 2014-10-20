package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }
  
  trait TomSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("m", "m body", 5))
    val set3 = set2.incl(new Tweet("l", "l body", 40))
    val set4 = set3.incl(new Tweet("p", "p body", 20))
    val set5 = set4.incl(new Tweet("n", "n body", 30))
    val set6 = set5.incl(new Tweet("z", "z body", 21))
    val set7 = set6.incl(new Tweet("d", "d body", 19))
    val set8 = set7.incl(new Tweet("a", "a body", 20))
    val set9 = set8.incl(new Tweet("e", "e body", 44))
    val set10 = set9.incl(new Tweet("y", "y body", 2))

  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }
  
  test("filter: unbalanced tree") {
    new TomSets {
      val filtered = set10.filter(tw => tw.retweets >= 20)
      assert(size(filtered) == 6)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("union: with self is self") {
    new TomSets {
      assert(asSet(set10.union(set10)) === asSet(set10))
    }
  }
  
  test("most retweeted on empty set throws NoSuchElementException") {
    intercept[NoSuchElementException] {
      val empty = new Empty
      empty.mostRetweeted
    }
  }
  
  test("most retweeted gives single tweet") {
    new TomSets {
      assert(set10.mostRetweeted.user === "e")
    }
  }
  
  test("descending: empty set is Nil") {
    val empty = new Empty
    assert(empty.descendingByRetweet == Nil)
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
