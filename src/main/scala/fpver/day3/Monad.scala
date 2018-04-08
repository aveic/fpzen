package fpver.day3

trait Monad[F[_]] extends Functor[F] with Applicative[F] {
  def flatMap[A,B](fa:F[A])(f: A => F[B]):F[B]
}
