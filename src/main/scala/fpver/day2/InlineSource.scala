package fpver.day2

class InlineSource(val table:Table) extends Source {
  def getRows: Table = table
}