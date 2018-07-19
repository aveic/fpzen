package fpver.day2

object TableDumper {
  def dumpTable(table: Seq[Map[String,Any]], caption:String):String = {
    val title = s"### $caption: ###\n"
    val rows = if (table.isEmpty) "Empty" else dumpRows(table)

    title + rows
  }

  protected def dumpRows(table: Seq[Map[String,Any]]):String = {
    val colsMeta = getColsMeta(table)

    val headers = dumpHeaders(colsMeta)
    val rows    = table map { dumpRow(_, colsMeta) } mkString "\n"
    val total   = s"Rows count: ${table.size}"

    headers + "\n" + rows + "\n" + sep(colsMeta) + "\n" + total
  }

  protected def getColsMeta(table: Seq[Map[String,Any]]):Map[String,Int] = {
    val cellsWidths  = table map {r => r.mapValues(_.toString.length + 2) }
    val headerWidths = table.head.keys.map(c => (c,c.length + 2)).toMap

    val allWidths = cellsWidths ++ Seq(headerWidths)

    table.head.keys map { col => (col, allWidths.map(r => r(col)).max) } toMap
  }

  protected def getColsWidth(r: Map[String,Any]):Map[String,Int] = r.mapValues(_.toString.length)

  protected def dumpHeaders(colsMeta: Map[String, Int]):String = {
    val s = sep(colsMeta)
    s + "\n" + dumpRow(colsMeta.map(p => (p._1, p._1)), colsMeta) + "\n" + s
  }

  protected def dumpRow(row:Map[String,Any], colsMeta:Map[String,Int]):String = {
    "|" + row.map(p => " " + p._2.toString.padTo(colsMeta(p._1), " ").mkString("")).mkString("|") + "|"
  }

  protected def sep(meta: Map[String,Int]):String = {
    "+" + (meta.values map {w => "-" * (w + 1)} mkString "+") + "+"
  }
}
