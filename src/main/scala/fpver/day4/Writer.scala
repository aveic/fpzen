package fpver.day4

import scala.util.{Failure, Success, Try}

class Writer[W : Monoid, V](val written: W, val value: V) {
  def map[BV](f: V => BV):Writer[W, BV] = Writer(written, f(value))
  def mapWritten[BW : Monoid](f: W => BW):Writer[BW, V] = Writer(f(written), value)
  def tell(w: W):Writer[W,V] = Writer(implicitly[Monoid[W]].combine(written, w), value)
  def flatMap[B](f: V => Writer[W, B]):Writer[W, B] = {
    val b = f(value)
    Writer(implicitly[Monoid[W]].combine(written, b.written), b.value)
  }
}

object Writer {
  def apply[W : Monoid,V](written:W, value: V):Writer[W,V] = new Writer(written, value)
  def log[W : Monoid](w:W):Writer[W,Unit] = new Writer(w, ())
  def fromOption[W : Monoid,V](vo:Option[V], onNone:W):Writer[W,Option[V]] = if (vo.isDefined) Writer(implicitly[Monoid[W]].empty, vo) else Writer(onNone, vo)
  def fromEither[W : Monoid,V](ve:Either[W,V]):Writer[W,Option[V]] = ve match {
    case Left(e) => Writer(e, None)
    case Right(v) => Writer(implicitly[Monoid[W]].empty, Some(v))
  }

  def fromTry[W : Monoid,V](t:Try[V], f: Throwable => W)():Writer[W, Option[V]] = t match {
    case Success(v) => Writer(implicitly[Monoid[W]].empty, Some(v))
    case Failure(e) => Writer(f(e), None)
  }

  implicit val writerFunctor:Functor[LogsWriter] = new Functor[LogsWriter] {
    override def map[A, B](fa: LogsWriter[A])(f: (A) => B): LogsWriter[B] = fa.map(f)
  }

  implicit val writerApplictive:Applicative[LogsWriter] = new Applicative[LogsWriter] {
    override def pure[A](v: A): LogsWriter[A] = Writer(implicitly[Monoid[Logs]].empty,v)
    override def ap[A, B](ff: LogsWriter[(A) => B])(fa: LogsWriter[A]): LogsWriter[B] = for {
      f <- ff
      a <- fa
    } yield f(a)

    override def map[A, B](fa: LogsWriter[A])(f: (A) => B): LogsWriter[B] = writerFunctor.map(fa)(f)
  }

  implicit val writerMonad:Monad[LogsWriter] = new Monad[LogsWriter] {
    override def flatMap[A, B](fa: LogsWriter[A])(f: (A) => LogsWriter[B]): LogsWriter[B] = fa.flatMap(a => f(a))
    override def map[A, B](fa: LogsWriter[A])(f: (A) => B): LogsWriter[B] = writerFunctor.map(fa)(f)
    override def pure[A](v: A): LogsWriter[A] = writerApplictive.pure(v)
    override def ap[A, B](ff: LogsWriter[(A) => B])(fa: LogsWriter[A]): LogsWriter[B] = writerApplictive.ap(ff)(fa)
  }
}

