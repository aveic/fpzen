class MyMap[K,V] private(val buckets:Array[Array[(K,V)]]) extends (K => V) {
  val size = buckets.length
  def apply(key:K):V = get(key).get
  def get(key:K):Option[V] = {
    val i = Math.abs(key.hashCode() % size)
    val bucket = buckets(i)

    if (bucket == null) None else {
      val filtered = bucket filter {_._1 == key}
      if (!filtered.isEmpty) Some(filtered.head._2) else None
    }
  }

  def getOrElse(key:K, default: => V):V = get(key).getOrElse(default)

  def pairs:Array[(K,V)] = {
    val pairs = for { i <- buckets.indices } yield if (buckets(i) == null) buckets(i) else Array[(K,V)]()
    pairs.flatten.toArray
  }

  def values:Array[V] = ???
  def keys:Array[K] = ???
  def mapValues[VV](f: V => VV):MyMap[K, VV] = ???
  def flatMap[VV](p:(K,V) => Map[K,VV]):Map[K,VV] = ???
  def :+(p:(K,V)):Map[K,V] = ???
  override def toString:String = {
    val sublists = for {
      i <- buckets.indices
    } yield if (null == buckets(i)) "" else buckets(i) map {case (k, v) => s"$k->$v"} mkString ","


    val vals = sublists.filter(_ != "").mkString(",")

    s"MyMap($vals)"
  }

}

object MyMap {
  def apply[K,V](pairs: (K,V)*):MyMap[K,V] = {
    val length = 5
    val buckets = new Array[Array[(K,V)]](length)

    for { (k, v) <- pairs } yield {
      val i = Math.abs(k.hashCode() % length)

      if (null == buckets(i)) buckets(i) = Array(k -> v) else buckets(i) = merge(buckets(i), k -> v)
    }


    debug(buckets)
    new MyMap[K,V](buckets)
  }

  protected def merge[K,V](bucket:Array[(K,V)], v:(K,V)):Array[(K,V)] = {
    var merged = false
    for { i <- bucket.indices } {
      if (bucket(i)._1 == v._1) {merged = true; bucket(i) = v} else ()
    }

    if (merged) bucket else bucket :+ v
  }

  def debug[K,V](buckets:Array[Array[(K,V)]]):Unit = {
    for {
      i <- buckets.indices

    } yield {
      val line = if (null == buckets(i)) "EMPTY" else buckets(i).mkString(" -> ")
      println(s"$i = $line")
    }
  }
}


val map = MyMap(
  "a" -> 1,
  "x" -> 13,
  "b" -> 2,
  "g" -> 33,
  "z" -> 50,
  "gqgq" -> 12312,
  "a" -> 5
)

map("a")
map.get("a")
map.getOrElse("niggah", 5)
map.pairs


// ok we need real lists!