package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet =
     filterAcc(t => true, that)

  def mostRetweeted: Tweet
  protected[objsets] def hasMostRetweeted: Boolean
  
  def descendingByRetweet: TweetList
  
  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit
  
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc


  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  def mostRetweeted: Tweet = throw new NoSuchElementException
  
  def hasMostRetweeted: Boolean = false
  
  def descendingByRetweet: TweetList = Nil
  
  override def toString = "."
    
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  override def toString = "{" + left + elem.user + right + "}"
  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    //Combine filterAcc on left and right sides
    def filterBothSides: TweetSet => TweetSet =
      newAcc => left.filterAcc(p, right.filterAcc(p, newAcc))
    
    if (p(elem)) filterBothSides(acc incl elem)
    else filterBothSides(acc)
  }


  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  
  def hasMostRetweeted : Boolean = true
  
  def mostRetweeted: Tweet = {
    val leftMost = if (left.hasMostRetweeted) left.mostRetweeted else elem
    val rightMost = if (right.hasMostRetweeted) right.mostRetweeted else elem
    
    if (elem.retweets >= leftMost.retweets && elem.retweets >= rightMost.retweets) elem
    else if (rightMost.retweets >= leftMost.retweets) rightMost
    else leftMost
  }
  
  def descendingByRetweet = new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)
  
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tw =>
    google.exists(filterStr => tw.text.contains(filterStr)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tw =>
  	apple.exists(filterStr => tw.text.contains(filterStr)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
