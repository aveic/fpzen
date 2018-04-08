class Rational(val n:Int, val d:Int) extends Product with Serializable

object Rational {
  def apply(n:Int, d:Int) = new Rational(n, d)
  def unapply(arg: Rational): Option[(Int, Int)] = if (arg == null) None else Some((arg.n, arg.d))
}

val r = Rational(1,2)

val s = r match {
  case Rational(1, 2) => "1/2"
  case Rational(n, d) => s"$n/$d"
}



