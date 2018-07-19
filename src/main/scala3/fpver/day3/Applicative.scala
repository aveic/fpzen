package fpver.day3

trait Applicative[F[_]] extends Functor[F] {
  def ap[A,B](ff: F[(A) => B])(fa: F[A]):F[B]
  def pure[A](v:A):F[A]
  def product[A,B](fa:F[A], fb:F[B]):F[(A,B)] = ap(map(fa)(a => (b: B) => (a,b)))(fb)
  def ap2[A,B,Z](ff: F[(A,B) => Z])(fa:F[A], fb:F[B]):F[Z] = map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = map(product(fa, fb))(f.tupled)
}
