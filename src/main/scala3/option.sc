object yolo {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = flatMap(x => if(f(x)) Some(x) else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  val s = Some(2)
  val n = None:Option[Int]

  s.map(_*2) == Some(4)
  s.flatMap(x => Some(x*2)) == Some(4)
  s.getOrElse(3) == 2
  s.orElse(Some(3)) == Some(2)
  s.filter(_ > 2) == None
  s.filter(_ > 1) == Some(2)

  n.map(_*2) == None
  n.flatMap(x => Some(x*2)) == None
  n.getOrElse(3) == 3
  n.orElse(Some(3)) == Some(3)
  n.filter(_ > 2) == None

  def mean(xs: Seq[Double]): Option[Double] =
    if
    (xs.isEmpty) None
    else
      Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]):Option[Double] = {
    mean(xs).flatMap( m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil):Option[List[A]]) { (el, acc) => map2(el, acc)(_ :: _) }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(Nil):Option[List[B]]) {
    (el, acc) => map2(f(el), acc)((el2, acc2) => el2 :: acc2)
  }

  sequence(List(Some(1), Some(2), None))
}

