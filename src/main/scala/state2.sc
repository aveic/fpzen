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


type Rand[A] = State[RNG,A]
val int:Rand[Int] = State(_.nextInt)
def ints(count:Int):Rand[List[Int]] = State.sequence(List.fill(count)(int))




def get[S]: State[S,S] = State(s => (s, s))
def set[S](s: S):State[S,Unit] = State(_ => ((), s))

def modify[S](f: S => S): State[S, Unit] = for {
  s <- get
  _ <- set(f(s))
} yield ()

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def input(i:Input):Machine = if (candies <= 0) this else (i, this) match {
    case (Coin, Machine(true,  s, c)) => Machine(false, s, c+1)
    case (Turn, Machine(false, s, c)) => Machine(true, s - 1, c)
    case _ => this
  }
}

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  var actions = inputs.map( i => modify[Machine]( _.input(i)) )
  for {
    actions <- State.sequence(actions)
    m <- get
  } yield (m.coins, m.candies)
}

def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
  var actions = inputs.map((i: Input) => modify((m: Machine) => m.input(i)))
  State.sequence(actions).flatMap(_ => get).map(m => (m.coins, m.candies)) // or for comprehension
}

simulateMachine(List(Coin,Turn, Coin,Turn, Coin,Turn, Coin,Turn)).run(Machine(false,5,10))

