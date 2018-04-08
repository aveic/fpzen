package object myStream {
  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def drop(n:Int):Stream[A] = this match {
      case Cons(_, t) if n >= 1 => t().drop(n - 1)
      case _ => this
    }

    def take(n:Int):Stream[A] = this match {
      case Cons(h, t) if n >= 1 => Stream.cons2(h(), t().take(n - 1))
      case _ => Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B):B = this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

    def exists(p: A => Boolean):Boolean = foldRight(false){ (el,acc) => p(el) || acc }

    def forAll(p: A => Boolean):Boolean = foldRight(true){ (el,acc) => p(el) && acc }

    def takeWhile(p: A => Boolean):Stream[A] = foldRight(Empty:Stream[A])( (el, acc) => if(p(el)) Stream.cons2(el, acc) else Empty )

    def headOption_2: Option[A] = foldRight(None:Option[A])( (el, _) => Some(el) )

    def map[B](f: A => B): Stream[B] = foldRight(Empty:Stream[B])( (el, acc) => Stream.cons2(f(el), acc))

    def filter(p: A => Boolean): Stream[A] = foldRight(Empty:Stream[A])( (el, acc) => if (p(el)) Stream.cons2(el, acc) else acc)

    def append[B >: A](b: => Stream[B]):Stream[B] = foldRight(b)( (el, acc) => Stream.cons2(el, acc) )

    def flatMap[B](f: A => Stream[B]):Stream[B] = foldRight(Empty:Stream[B])((el, acc) => f(el) append acc)

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def mapViaUnfold[B](f: A => B):Stream[B] = Stream.unfold(this){
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

    def takeViaUnfold(n:Int):Stream[A] = Stream.unfold((n, this)){
      case (n, _) if n <= 0 => None
      case (_, Empty) => None
      case (n, Cons(h, t)) => Some(h(), (n-1, t()))
    }

    def takeWhileViaUnfold(p: A => Boolean) = Stream.unfold(this){
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      case _ => None
    }

    def zipWithViaUnfold[B,C](b: Stream[B])(f: (A,B) => C):Stream[C] = Stream.unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some( ( f(h1(), h2()), (t1(), t2())) )
      case _ => None
    }

    def zipAllViaUnfold[B,C](b:Stream[B]):Stream[(Option[A], Option[B])] = Stream.unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some( (Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some( (Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some( (None, Some(h2())), (Empty, t2()))
      case _ => None
    }

    def startsWith[B >: A](b:Stream[B]):Boolean = zipAllViaUnfold(b) forAll ( t => {
      val (ao, bo) = t
      if (bo.isEmpty) true
      else
        (for {
          a <- ao
          b <- bo
        } yield a == b
          ).getOrElse(false)
    })

    def tails: Stream[Stream[A]] = Stream.cons2(this,Empty) append Stream.unfold(this)({
      case Cons(_, t) => Some((t(), t()))
      case _ => None
    })

    def hasSubsequence[B >: A](s:Stream[B]):Boolean = tails exists (_ startsWith s)
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons2[A](h: => A, t: => Stream[A]) = {
      lazy val hd = h
      lazy val tl = t
      Cons(() => hd, () => tl)
    }

    def empty[A] = Empty: Stream[A]

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty[A] else cons2(as.head, apply[A](as.tail: _*))

    def unfold[A,S](z:S)(f: S => Option[(A,S)]):Stream[A] = f(z) match {
      case Some((v,s)) => cons2(v, unfold(s)(f))
      case None => Empty
    }

    def constant[A](c:A): Stream[A] = unfold(c)(s => Some(s,s))

    def from(n:Int):Stream[Int] = unfold(n)(s => Some(s, s+1))

    def fib:Stream[Int] = unfold((0, 1))( s => Some(s._1, (s._2, s._1 + s._2)))
  }



}
