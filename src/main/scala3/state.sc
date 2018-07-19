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

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i2, rng2) = rng.nextInt
  val r = if (i2 == Int.MinValue) 0 else if (i2 < 0) -i2 else i2
  (r, rng2)
}

def double(rng: RNG): (Double, RNG) = {
  val (i2, rng2) = rng.nextInt
  ((i2.abs - 1).toDouble / Int.MaxValue, rng2)
}


def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (i, rng2) = rng.nextInt
  val (d, rng3) = double(rng2)

  ((i,d), rng3)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = List.fill(count)(1).foldRight((Nil:List[Int], rng))((_, acc) => {
  val (l, r) = acc
  val (i, r2) = r.nextInt
  (i :: l, r2)
})

type Rand[+A] = RNG => (A, RNG)

val int:Rand[Int] = _.nextInt

def doubleViaMap: Rand[Double] = map(int){i => (i.abs - 1).toDouble / Int.MaxValue}


def unit[A](a: A): Rand[A] = rng => (a, rng)

def map[A,B](s: Rand[A])(f: A => B):Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, r2) = ra(rng)
  val (b, r3) = rb(r2)
  (f(a,b), r3)
}



def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

def both[A,B](ra: Rand[A], rb:Rand[B]):Rand[(A,B)] = map2(ra, rb){ (_, _) }

def randIntDouble = both(doubleViaMap, int)

def sequence[A](fs: List[Rand[A]]):Rand[List[A]] = rng => fs.foldRight((Nil:List[A], rng))( (el, acc) => {
  val (l, r) = acc
  val (v, r2) = el(r)
  (v :: l, r2)
})


def randInts(count:Int):Rand[List[Int]] = sequence(List.fill(count)(int))

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]):Rand[B] = rng => {
  val (fv, r2) = f(rng)
  g(fv)(r2)
}

def map_2[A,B](s:Rand[A])(f: A => B):Rand[B] = flatMap(s)(a => (f(a), _))


randInts(10)(SimpleRNG(42))
nonNegativeEven(SimpleRNG(23))

map_2(randInts(5))(l => l.map(_.abs % 10))(SimpleRNG(42))
