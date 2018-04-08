package fpver.day3

class InlineSource(val table:Seq[Map[String,String]]) extends Source {
  def getRows: Table = table map (m => Row(m))
}