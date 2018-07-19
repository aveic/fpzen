val x = scala.collection.immutable.Map

class MyMap[K,+V](val _pairs:List[(K,V)]) extends (K=>V) with Iterable[(K,V)] {

  val pairs:List[(K,V)] = normalizePairs(_pairs)

  protected def normalizePairs[VV >: V](pairs: List[(K, VV)]):List[(K,VV)] = {
    pairs.foldRight((Nil:List[(K,VV)], Set():Set[K]))( (pair, acc) => {
      val (k, _) = pair
      val (pairsList, keysSet) = acc
      if (keysSet.contains(k)) (pairsList, keysSet) else (pairsList :+ pair, keysSet + k)
    })._1
  }

  override def toString = "MyMap(" + pairs.map(p => p._1.toString + "=>" + p._2.toString).reduce(_ + "," + _) + ")"
  def mapValues[B](f: V => B):MyMap[K,B] = new MyMap(pairs.map(p => (p._1, f(p._2))))
  def apply(k:K):V = find(k).get
  def getOrElse[VV >: V](k:K, default: VV):VV = find(k).getOrElse(default)
  def find(k:K):Option[V] = pairs.find(_._1 == k).map(_._2)

  def keys:Set[K] = pairs.map(_._1).toSet
  def values:List[V] = pairs.map(_._2)

  def updated[VV >: V](k:K, v:VV):MyMap[K,VV] = new MyMap(pairs :+ (k,v))

  def +[VV >: V](p: (K,VV)) = new MyMap(pairs :+ p)
  def ++[VV >: V](b: MyMap[K,VV]):MyMap[K,VV] = new MyMap(pairs ++ b.pairs)

  override def iterator: Iterator[(K,V)] = new MyMapIterator(pairs.toVector)

  private class MyMapIterator(val vec:Vector[(K,V)]) extends Iterator[(K,V)] {
    protected var k:Int = 0
    override def hasNext: Boolean = k < vec.length

    override def next(): (K,V) = {
      val next = vec(k)
      k = k + 1
      next
    }
  }
}

object MyMap {
  def apply[K,V](pairs:(K,V)*) = new MyMap(pairs.toList)
}

val mm = MyMap("a" -> 10, "b" -> 20, "a" -> 30)
val mm2 = MyMap("c" -> 10, "a" -> 20, "d" -> 30)

mm ++ mm2

for { (k:String, v:Int) <- mm } yield k + v.toString

List("a", "b") map mm

mm.mapValues(_ + 10)
