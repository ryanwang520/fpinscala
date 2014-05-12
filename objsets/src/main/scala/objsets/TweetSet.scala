package objsets

import TweetReader._


class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}


abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def mostretweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

  def union(that: TweetSet): TweetSet
}

class Empty extends TweetSet {
  def descendingByRetweet: TweetList = Nil

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  lazy val singleton: Tweet = new Tweet("", "", -1)

  def mostRetweeted: Tweet = throw new NoSuchElementException("No tweet exists")

  def mostretweeted: Tweet = singleton

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def union(that: TweetSet): TweetSet = that

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  def descendingByRetweet: TweetList = new Cons(mostRetweeted, this.remove(mostRetweeted).descendingByRetweet)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = elem match {
    case e if p(e) => this.remove(elem).filterAcc(p, acc) incl e
    case _ => this.remove(elem).filterAcc(p, acc)
  }

  def mostretweeted: Tweet = {
    val l = left.mostretweeted
    val r = right.mostretweeted
    def max(l: Tweet, m: Tweet, r: Tweet) = List(l, m, r).maxBy(x => x.retweets)
    max(l, elem, r)
  }

  def mostRetweeted: Tweet = mostretweeted

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def union(that: TweetSet): TweetSet = this.left union (this.right union that.incl(elem))


  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
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
  lazy val googleTweets: TweetSet = allTweets.filter(t => google.exists(x => t.text.contains(x)))
  lazy val appleTweets: TweetSet = allTweets.filter(t => apple.exists(x => t.text.contains(x)))
  lazy val trending: TweetList = (appleTweets union googleTweets).descendingByRetweet
}

object Main extends App {
  println("begin")
  GoogleVsApple.trending foreach println
}
