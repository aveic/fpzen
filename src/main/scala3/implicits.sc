import scala.annotation.implicitNotFound

object X {
  implicit val otherPrefix: (String,String) = "[" -> "]"
}

implicit val prefix: (String, String) = "{" -> "}"
def wrap(s:String)(implicit prefix:(String,String)) = prefix._1 + s + prefix._2

wrap("value1")

{
  implicit val prefix = X.otherPrefix // shadowing
  wrap("value2")
}


// implicit val, ambiguity, shadow, (wrap + prefix)
// implicitly summoner
// conversion, def + extra class, for syntax for type converion
// implicit class
// context bound (simple and suggared), type class pattern, IOrdered, @implicitNotFound("Couldn't find implicit for Ordered[${A}]")
// implicit resolution precedence

implicitly[(String,String)] // summoner


@implicitNotFound("No member of type class Group found for type ${A}")
trait IOrdered[A] {
  def cmp(a: A, b: A):Boolean
}

def sort[A : IOrdered](l: List[A]):List[A] = {
  val order = implicitly[IOrdered[A]]
  l.sortWith(order.cmp)
}

 val intOrderer = new IOrdered[Int] {
  def cmp(a:Int, b:Int):Boolean = a < b
}

sort(List(6,7,1,3,4))
