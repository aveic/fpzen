object MyProps {

  import rng._
  import myStream._

  case class Gen[A](sample: State[RNG,A]) {
    def flatMap[B](f: A => Gen[B]):Gen[B] = Gen(sample.flatMap(a => f(a).sample))
    def unsized:SGen[A] = SGen(_ => this)
  }

  case class SGen[A](forSize: Int => Gen[A])

  object Gen {
    def unit[A](a: => A):Gen[A] = Gen(State((a, _)))
    def choose(start:Int, stopExclusive:Int):Gen[Int] = Gen(int.map(_ % (stopExclusive - start + 1) + start))
    def boolean:Gen[Boolean] = Gen(int.map(_ % 2 == 0))
    def listOfGenN[A](ng: Gen[Int], g: Gen[A]): Gen[List[A]] = ng.flatMap(n => Gen(State.sequence(List.fill(n)(g.sample))))
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
    def listOf[A](g: Gen[A]):SGen[List[A]] = SGen(n => listOfN(n, g))
    def listOf1[A](g: Gen[A]):SGen[List[A]] = SGen(n => listOfN(n max 1, g))
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)):Gen[A] = {
      val minGen = if (g1._2 < g2._2) g1 else g2
      val maxGen = if (g1._2 >= g2._2) g2 else g2

      val minW = (minGen._2 * 100).toInt
      val maxW = (maxGen._2 * 100).toInt

      val tw = minW + maxW

      choose(1, tw).flatMap(w => if (minW >= w) minGen._1 else maxGen._1)
    }
  }

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure:FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop): Prop = Prop((mx, n,rng) => Prop.resultAnd(this.run(mx,n,rng), p.run(mx,n,rng)))
    def ||(p: Prop): Prop = Prop((mx, n,rng) => Prop.resultOr(this.run(mx,n,rng), p.run(mx,n,rng)))
  }

  object Prop {
    def randomStream[A](g: Gen[A])(rng:RNG):Stream[A] = Stream.unfold(rng)(r => Some(g.sample.run(r)))

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(n => g.forSize(n))(f)
    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max,n,rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props:Stream[Prop] = Stream.from(0).takeViaUnfold((n min max) + 1).mapViaUnfold(i => Prop.forAll(g(i))(f))
        val prop: Prop = props.mapViaUnfold(p => Prop{(max, _, rng) => p.run(max, casesPerSize, rng)}).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }

    def forAll[A](gen: Gen[A])(p: A => Boolean): Prop = Prop {
      (_,n,rng) => randomStream(gen)(rng).zipWithViaUnfold(Stream.from(0))((_,_)).takeViaUnfold(n).mapViaUnfold {
        case (a, i) => try {
          if (p(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
    }

    def buildMsg[A](s:A, e:Exception):String = s"test case $s\n generated an exception ${e.getMessage}\n}"

    def resultAnd(r1:Result, r2: => Result):Result = if (r1.isFalsified) r1 else r2
    def resultOr(r1:Result, r2: => Result):Result = if (!r1.isFalsified) r1 else r2

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Passed else Falsified("()", 0)
    }
  }

  // test case 1
  val intList = Gen.listOfN(100, Gen.choose(0,100))
  val prop =
      Prop.forAll(intList)(ns => ns.reverse.reverse == ns) &&
      Prop.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  // test case 2
  val smallInt = Gen.choose(-10,10)
  val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())):Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property")
    }
  }

  run(prop)
  run(maxProp)

}
