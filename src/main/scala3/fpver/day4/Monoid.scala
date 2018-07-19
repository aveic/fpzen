package fpver.day4

trait Monoid[A] {
  def empty: A
  def combine(a:A, b:A):A
}
