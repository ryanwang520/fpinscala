import org.scalacheck.Prop.False
object Test {

  trait IntSet {
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    def union(s: IntSet): IntSet
  }
  case class NonEmpty(value:Int,left:IntSet,right:IntSet) extends IntSet{
     def contains(x: Int): Boolean = x match{
      case x if (x < value) => left contains x
      case x if (x > value) => right contains x
      case _ => true
    }


     def incl(x: Int): IntSet = x match{
      case x if (x < value) => NonEmpty(value,left incl x,right)
      case x if (x > value) => NonEmpty(value,left,right incl x)
      case _ => this
    }
    override def toString = "{" + left + value + right + "}"


  def union(s: IntSet): IntSet = (left union  right union s) incl value


  }

  object Empty extends IntSet {
    def contains(x:Int):Boolean = false
    def incl(x:Int) = NonEmpty(x,Empty,Empty)
    override  def toString = "."
    def union(s:IntSet):IntSet = s
  }

  val t1:NonEmpty = NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  import java.lang
  val x:lang.String = "sdf"

}