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
      assert(size(set5.filter(tw => tw.user >= "a")) === 4)
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
      assert(size(set5.filter(tw => tw.user >= "c")) === 2)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
      assert(size(set5.filter(tw => tw.retweets >= 7)) === 4)
      assert(size(set5.filter(tw => tw.retweets >= 9)) === 3)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
      assert(size(set1.union(set2).union(set4d)) === 3)
      assert(size(set1.union(set1)) === 0)
      assert(size(set1.union(set4c)) === 3)
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

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
    // println(trends.tail.head.user)
     // println(trends.tail.tail.head.user)
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.tail.head.user == "b")
      assert(trends.tail.tail.head.user == "d")

    val t = set1.descendingByRetweet
      assert(t.isEmpty)

      val t1 = set2.descendingByRetweet
      assert(!t1.isEmpty)
      assert(t1.head.user == "a")

      val t2 = set4c.descendingByRetweet
      assert(t2.tail.tail.head.retweets == 7)
    }


  }
}
