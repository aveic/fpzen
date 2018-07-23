object FP {

  trait Functor[F[_]] {
    // u define
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] { self =>
    // u define
    def apply[A, B](fa: F[A])(fab: F[A => B]): F[B]
    def unit[A](a: => A): F[A]

    // override
    override def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(unit(f))

    // helpers
    def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = apply(fa)(map(fb)(b => (a:A) => f(a,b)))


    def sequence[A](fas:List[F[A]]):F[List[A]] = traverse(fas)(identity)
    def traverse[A,B](fas:List[F[A]])(f: A => B):F[List[B]] = fas.foldRight(unit(List.empty[B])){ (el, acc) => map2(map(el)(f))(acc)(_ +: _) }
    def replicateM[A](n:Int, fa:F[A]):F[List[A]] = sequence(List.fill(n)(fa))



    // composing
    def product[G[_]](G: Applicative[G]):Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fga: (F[A], G[A]))(fgab: (F[(A) => B], G[(A) => B])) = {
        val (fa, ga) = fga
        val (fab, gab) = fgab
        (self.apply(fa)(fab), G.apply(ga)(gab))
      }
    }

    def compose[G[_]](G: Applicative[G]):Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A):F[G[A]] = self.unit(G.unit(a))
      override def apply[A, B](fga: F[G[A]])(fgab: F[G[(A) => B]]):F[G[B]] = self.map2(fga)(fgab)( (ga, gab) => G.apply(ga)(gab))
    }
  }


  trait Monad[F[_]] extends Applicative[F] {
    // u define
    def unit[A](a: => A):F[A]
    def flatMap[A,B](fa:F[A])(f: A => F[B]):F[B]

    // override Applicative
    override def apply[A, B](fa: F[A])(fab: F[A => B]): F[B] = flatMap(fab)(f => flatMap(fa)(a => unit(f(a))) )

    // derivates
    def join[A](ffa: F[F[A]]):F[A] = flatMap(ffa)(fa => fa)
    // compose as well
  }


  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A) = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) = fa.flatMap(f)
  }

  implicit def eitherMonad[E]: Monad[({type f[x] = Either[E,x]})#f] = new Monad[({type f[x] = Either[E,x]})#f] {
    override def unit[A](a: => A):Either[E,A] = Right(a)
    override def flatMap[A, B](fa: Either[E,A])(f: (A) => Either[E,B]) = fa.flatMap(f)
  }

  optionMonad.replicateM(2, None)

}