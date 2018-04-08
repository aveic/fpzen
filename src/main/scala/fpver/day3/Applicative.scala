package fpver.day3

trait Applicative[F[_]] {
  def ap[A,B](ff: F[(A) => B])(fa: F[A]):F[B]
  def pure[A](v:A):F[A]
}
