package object rng {

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

  case class State[S, +A](run: S => (A, S)) {
    def unit[B >: A](a: B): State[S, B] = State((a, _))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (v, s2) = run(s)
      f(v).run(s2)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))
  }

  object State {
    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = sa.flatMap(a => sb.map(b => f(a, b)))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
      fs.foldRight((Nil: List[A], s)) { (el, acc) => {
        val (l, s1) = acc
        val (v, s2) = el.run(s1)
        (v :: l, s2)
      }
      }
    })
  }


  type Rand[A] = State[RNG, A]
  val int: Rand[Int] = State(_.nextInt)
}