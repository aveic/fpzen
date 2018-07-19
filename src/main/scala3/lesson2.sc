object Test {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => f(a, _)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = f(_)(_)
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def mul(a:Int, b:Int) = a * b
  val mulC = curry(mul)

  mulC(5)(10)


  // every func is a closure actually! (that's why curry works)
  // partially applied functions
  // u can define anything anywhere
  // class in def/ def in class w/e
  // no operators -> just functions
  // infix operator, postfix operator
  // () = {}
  // class/object/apply/type params/override?
  // type
  // trait and dynamic classes (in case of Function1/2)
  // DSL homework:

  class Pair[A,B](val first:A, val second:B) {
    override def toString: String = s"($first,$second)" // string context, we can write our own
    def applied[C](f: (A,B) => C):C = f(first, second)
    def swap:Pair[B,A] = Pair(second, first)
  }

  object Pair {
    def apply[A,B](first:A, second:B):Pair[A,B] = new Pair(first, second)
  }

  type Point = Pair[Int, Int]     // type aliases
  type Rect  = Pair[Point, Point] // type aliases, can be parametrized as well


  val topLeft = Pair(10, 20)
  val bottomRight = Pair(30, 40)

  val rectangle = Pair(topLeft, bottomRight)

  implicit class PairOps[A](first:A) {
    def x[B](second:B):Pair[A,B] = Pair(first, second)
  }

  val rect:Rect = (0 x 0) x (3 x 4)

  rect.first
  rect.second

  rect.swap


  def dist(p1:Point, p2:Point):Double = Math.sqrt(
    Math.pow(p1.first - p2.first, 2) + Math.pow(p1.second - p2.second, 2)
  )

  (rect applied dist) == 5.0

  implicit class Func2Ops[A,B,C](f: (A,B) => C) {
    def paired: Pair[A,B] => C = p => f(p.first, p.second)
    def applyPair(p:Pair[A,B]):C = f(p.first, p.second)
  }

  (dist _) applyPair rect



  // home work:
  def trim(s:String) = s.trim
  def upperCase(s:String) = s.toUpperCase
  def surround(dec:String) = (s:String) => dec + s + dec

  val result = (
    Wrap function trim
    chainWith     upperCase
    andChainWith  surround("+++")
    runWith       "   HeLlo    "
  )

  result == "+++HELLO++"
}