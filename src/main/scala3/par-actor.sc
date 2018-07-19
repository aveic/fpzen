import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.collection.mutable.{Map => MMap}

type Par[A] = ExecutionContext => Future[A]

def unit[A](a: A): Par[A] = _ => Future.successful(a)
def fork[A](a: => Par[A]): Par[A] = ec => {
  implicit val iec:ExecutionContext = ec
  Future(a(ec)).flatten
}

def delay[A](fa: => Par[A]): Par[A] = ec => fa(ec)

def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
def map2[A,B,C](a: Par[A], b:Par[B])(f: (A,B) => C):Par[C] = ec => {

  println("MAP2")

  val fa = a(ec)
  val fb = b(ec)

  implicit val iec:ExecutionContext = ec

  for {
    vb <- fb
    va <- fa
  } yield f(va, vb)
}

def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))


def run[A](a: Par[A])(implicit ec:ExecutionContext):A = {
  Await.result(a(ec), Duration.Inf)
}

// We define `sequenceBalanced` using `IndexedSeq`, which provides an
// efficient function for splitting the sequence in half.
def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
  if (as.isEmpty) unit(Vector())
  else if (as.length == 1) map(as.head)(a => Vector(a))
  else {
    val (l,r) = as.splitAt(as.length/2)
    map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
  }
}

def sequence[A](as: List[Par[A]]): Par[List[A]] =
  map(sequenceBalanced(as.toIndexedSeq))(_.toList)

def parMap[A,B](ps: List[A])(f: A => B):Par[List[B]] = fork {
  val fbs: List[Par[B]] = ps.map(asyncF(f))
  sequence(fbs)
}


def choice[A](cond: Par[Boolean])(t: Par[A], f:Par[A]):Par[A] = ec => {
  if (run(cond)(ec)) t(ec) else f(ec)
}


def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = ec => choices(run(n)(ec))(ec)

def choice2[A](cond: Par[Boolean])(t:Par[A], f:Par[A]):Par[A] = choiceN(map(cond)(b => if(b) 0 else 1))(List(t, f))

def flatMap[A,B](a: Par[A])(f: A => Par[B]):Par[B] = ec => f(run(a)(ec))(ec)
def join[A](a: Par[Par[A]]):Par[A] = ec => run(a)(ec)(ec)


var rmap:MMap[Int,R] = MMap()

case class R(d:Double, to:Int, st:Long)

val s1 = System.currentTimeMillis()

def wrappedSqrt(d:Double):Double = {
  val to:Int = (2000 * Math.random()).toInt
  Thread.sleep(to.toInt)
  val r = R(d.toInt, to, System.currentTimeMillis())
  rmap.update(d.toInt, r)
  math.sqrt(d)
}

val p = parMap(List.range(1, 10))(wrappedSqrt(_))

run(p)(scala.concurrent.ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5) ))

val s2 = System.currentTimeMillis()

val s = rmap.toSeq.sortWith((l, r) => l._2.st < r._2.st)

s.foreach(println(_))

(s2 - s1, s.map(_._2.to).sum)