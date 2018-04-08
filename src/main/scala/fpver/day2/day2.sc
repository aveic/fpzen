import fpver.day2._

val source = new InlineSource(Seq(
  Map("A" -> "ORD-17813",   "B" -> "250.00", "C" ->  "740.00"),
  Map("A" -> "ORD-6724215", "B" -> "150.00", "C" -> "2500.00"),
  Map("A" -> "ORD-21235",   "B" -> "300.00", "C" -> "5700.00")
))

// the program itself
def processRow(row: Map[String,String], repo:OrderRepository):Unit = {
  // parsing phase
  val orderId  = parseCellAsString(row, "A")
  val payment  = parseCellAsMoney(row, "B")
  val delivery = parseCellAsMoney(row, "C")

  // lookup phase
  val orderRef = repo.findOrder(orderId)

  // IO phase
  repo.writeTx(orderRef, PaymentFromRecipient, payment)
  repo.writeTx(orderRef, DeliveryCost, delivery)
}

def parseCellAsString(row:Row, col:String):String = row(col)
def parseCellAsDouble(row:Row, col:String):Double = parseCellAsString(row, col).toDouble
def parseCellAsMoney(row:Row,  col:String):Money  = Money(parseCellAsDouble(row, col))


// the program runner
val repo = new OrderRepository(source)
source.getRows.foreach(processRow(_, repo))

// view external db state
val txsDump = repo.dumpTransactions

println(txsDump)





