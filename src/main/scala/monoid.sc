trait Monoid[A] {
  def empty: A
  def combine(a:A, b:A):A
}

def fold[A : Monoid](list:List[A]):A = {
  val m = implicitly[Monoid[A]]
  list.foldLeft(m.empty)(m.combine)
}

implicit val intAddMonoid:Monoid[Int] = new Monoid[Int] {
  override def empty: Int = 0
  override def combine(a: Int, b: Int): Int = a + b
}

fold(List(1,2,3))

implicit def mapMonoid[K, V : Monoid]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
  override def empty: Map[K, V] = Map()
  override def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
    val vm = implicitly[Monoid[V]]
    a ++ b.map{ case (k,v) => k -> vm.combine(v, a.getOrElse(k, vm.empty)) }
  }
}

fold(List(
  Map("a" -> 2, "b" -> 5),
  Map("a" -> 1, "c" -> 7)
))

implicit class MonoidOps[A : Monoid](a:A) {
  def |+|(b:A):A = implicitly[Monoid[A]].combine(a,b)
}

1 |+| 2

Map("a" -> 2) |+| Map("b" -> 5)

implicit def EndoFunctionMonoid[A]: Monoid[(A) => A] = new Monoid[(A) => A] {
  override def empty: (A) => A = identity
  override def combine(a: (A) => A, b: (A) => A): (A) => A = v => a(b(v))
}

def times(times:Int): Int => Int = _ * times

def double = times(2)
def tripple = times(3)

def sixtiple = double |+| tripple

sixtiple(10)

case class IsSorted[A : Ordering](min: Option[A], isSorted: Boolean, max: Option[A])

implicit def isSortedMonoid[A : Ordering]:Monoid[IsSorted[A]] = new Monoid[IsSorted[A]] {
  val ordering = implicitly[Ordering[A]]
  def cmp(a:Option[A], b:Option[A], f: (A,A) => A):Option[A] = if (a.isDefined && b.isDefined) Some(f(a.get, b.get)) else a orElse b
  override def empty: IsSorted[A] = IsSorted(None, true, None)
  override def combine(a: IsSorted[A], b: IsSorted[A]): IsSorted[A] = {
    if (a.isSorted && b.isSorted) {
      IsSorted(cmp(a.min, b.min, ordering.min), true, cmp(a.max, b.max, ordering.max))
    }
    else IsSorted(None, false, None)
  }
}

def isSorted[A : Ordering](seq:IndexedSeq[A]):Boolean = {
  def go(as:IndexedSeq[A]):IsSorted[A] = {
    val ord = implicitly[Ordering[A]]
    val (_, isSorted) = seq.foldRight((seq.last, true)){ (el, acc) => {
      el -> (acc._2 && ord.gteq(acc._1, el))
    } }

    if (isSorted) IsSorted(Some(seq.min), true, Some(seq.max)) else IsSorted(None, false, None)
  }

  if (!seq.isEmpty) fold(seq.sliding(5).map(go).toList).isSorted else false
}

isSorted(Vector(1,2,3,4,5,6,7,10,11,12,13))