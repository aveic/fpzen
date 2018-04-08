package fpver.day3

final case class OptionT[F[_], A](value: F[Option[A]]) {
  def map[B](f: (A) => B)(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.map(f)))
  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))
}

object OptionT {

  /**
    * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
    */
  private[day3] final class PurePartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(Some(value)))
  }

  /** Creates a `OptionT[A]` from an `A`
    *
    * {{{
    * scala> import cats.implicits._
    * scala> OptionT.pure[List](2)
    * res0: OptionT[List, Int] = OptionT(List(Some(2)))
    * }}}
    *
    */
  def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]

  /** An alias for pure
    *
    * {{{
    * scala> import cats.implicits._
    * scala> OptionT.some[List](2)
    * res0: OptionT[List, Int] = OptionT(List(Some(2)))
    * }}}
    *
    */
  def some[F[_]]: PurePartiallyApplied[F] = pure

  def none[F[_], A](implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(None))

  /**
    * Transforms an `Option` into an `OptionT`, lifted into the specified `Applicative`.
    *
    * {{{
    * scala> import cats.implicits._
    * scala> val o: Option[Int] = Some(2)
    * scala> OptionT.fromOption[List](o)
    * res0: OptionT[List, Int] = OptionT(List(Some(2)))
    * }}}
    */
  def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  /**
    * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
    */
  private[day3] final class FromOptionPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: Option[A])(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(value))
  }

  /**
    * Lifts the `F[A]` Functor into an `OptionT[F, A]`.
    */
  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(fa)(Some(_)))


  // instances for Option
  implicit def optionTFunctor[E[_] : Functor] = new Functor[({type f[x] = OptionT[E, x]})#f] {
    override def map[A, B](fa: OptionT[E,A])(f: (A) => B): OptionT[E,B] = fa map f
  }

  implicit def optionTApplicative[E[_] : Monad] = new Applicative[({type f[x] = OptionT[E, x]})#f] {
    override def pure[A](v: A): OptionT[E, A] = OptionT.pure[E](v)

    override def ap[A, B](ff: OptionT[E, (A) => B])(fa: OptionT[E, A]): OptionT[E, B] = {

      val x = for {
        f <- ff
        a <- fa
      } yield f(a)

      x
    }
  }

  implicit def optionTMonad[E[_] : Monad] = new Monad[({type f[x] = OptionT[E, x]})#f] {
    override def flatMap[A, B](fa: OptionT[E, A])(f: (A) => OptionT[E, B]): OptionT[E, B] = fa.flatMap(a => f(a))
    override def map[A, B](fa: OptionT[E, A])(f: (A) => B): OptionT[E, B] = optionTFunctor.map(fa)(f)
    override def pure[A](v: A): OptionT[E, A] = optionTApplicative.pure(v)
    override def ap[A, B](ff: OptionT[E, (A) => B])(fa: OptionT[E, A]): OptionT[E, B] = optionTApplicative.ap(ff)(fa)
  }
}
