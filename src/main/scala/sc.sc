implicit class MyStringContext(val sc:StringContext){
  def url(args: Any*): String = {
    val strings = sc.parts.iterator
    val expressions = args.iterator
    var buf = new StringBuffer(strings.next)
    while(strings.hasNext) {
      buf append java.net.URLEncoder.encode(expressions.next.toString, "utf-8")
      buf append strings.next
    }
    buf.toString
  }
}

val p = "Non Url ENCODED"

url"https://pimpay.ru/actoin?load=$p"