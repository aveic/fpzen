import java.util.concurrent.Executors

import scala.concurrent.{Await, Future}
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration.Duration

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

implicit val ec = scala.concurrent.ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))

val l = List.range(1,7)
val p = Future.sequence(l.map(x => Future(wrappedSqrt(x))))

val l2 = Await.result(p, Duration.Inf)

val s2 = System.currentTimeMillis()

val s = rmap.toSeq.sortWith((l, r) => l._2.st < r._2.st)

s.foreach(println(_))

(s2 - s1, s.map(_._2.to).sum)