package fpver.day4

class InlineSource(val table:Seq[Map[String,String]]) extends Source {
  def getRows: Table = table map (m => Row(m))
}