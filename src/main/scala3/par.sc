import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit}

sealed trait Future[A] { def get: A }

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A):Par[A] = es => new Future[A] {
    def apply(cb: A => Unit) { cb(a) }
  }

  def eval(es:ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {def call = r})

  def fork[A](a: => Par[A]):Par[A] = es => new Future[A] {
    def apply(cb: A => Unit):Unit = {
      eval(es)(a(es)(cb))
    }
  }

  def run[A](es:ExecutorService)(p: Par[A]): Future[A] = ???

  def map2[A,B,C](pa: Par[A], pb:Par[B])(f: (A,B) => C):Par[C] = ???

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = (es:ExecutorService) => {
    val fa = run(es)(a).get
    run(es)(f(fa))
  }

  def join[A](ppa: Par[Par[A]]):Par[A] = es => {
    val pa = run(es)(ppa).get
    pa(es)   
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => fork(unit(f(a)))

  def map[A,B](pa: Par[A])(f: A => B):Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]):Par[List[A]] = {
    ps.foldRight(unit(Nil:List[A]))( (el, acc) => map2(el, acc)(_ :: _))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fs:Par[List[List[A]]] = parMap(as)((a: A) => if (f(a)) List(a) else Nil: List[A])
    map(fs)(_.flatten)
  }
}