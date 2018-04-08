trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A,S)) {
  def unit[B >: A](a: B):State[S,B] = State((a, _))
  def flatMap[B](f: A => State[S,B]):State[S,B] = State(s => {
    val (v, s2) = run(s)
    f(v).run(s2)
  })

  def map[B](f: A => B):State[S, B] = flatMap(a => State(s => (f(a), s)))
}

object State {
  def map2[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] = sa.flatMap(a => sb.map(b => f(a,b)))
  def sequence[S,A](fs: List[State[S,A]]):State[S, List[A]] = State(s => {
    fs.foldRight((Nil:List[A], s)){ (el, acc) => {
      val (l, s1) = acc
      val (v, s2) = el.run(s1)
      (v :: l, s2)
    }}
  })
}

object Rand {
  def int:State[RNG,Int] = State(_.nextInt)
}


case class Gen[A](sample: State[RNG,A])


object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State((a, _)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(Rand.int.map(i => i % (stopExclusive - start + 1) + start))
  def boolean: Gen[Boolean] = Gen(Rand.int.map(_ % 2 == 0))
  def listOfN[A](n:Int, gen: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(gen.sample)))

  def word(maxLength: Int): Gen[String] = {
    val s = State.sequence(List.fill(maxLength)(Rand.int.map(Character.forDigit(_, 10).toString)))
    val l = s.map(_.reduce(_ + _))
    Gen(l)
  }
}

case class Prop[A](gen: Gen[A], p: A => Boolean) {
  def check: Boolean = gen.sample.forall(p)

  def &&(b: Prop[A]): Prop[A] = Prop(gen, a => p(a) && b.p(a))

  def ||(b: Prop[A]): Prop[A] = Prop(gen, a => p(a) || b.p(a))
}

object Prop {
  def forAll[A](gen: Gen[List[A]])(p: List[A] => Boolean):Prop[List[A]] = new Prop(gen, p)
  def any[A](gen: Gen[List[A]])(p: List[A] => Boolean):Prop[List[A]] = new Prop(gen, p)


}

val strList = Gen.listOf(Gen.str(10))

val prop = Prop.forAll(strList){ (ns:List[String]) => ns.reverse.reverse != ns }

prop.check
