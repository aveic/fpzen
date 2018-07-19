/* imagine u have some sort of a table:
+---+---+---+---+
| A | B | C | D |
+---+---+---+---+
with rows:
[ ORD-124213, 250.00, 1240.00, -     ]
[ ORD-124215,   0.00, 2100.00, -     ]
[ ORD-124217,   0.00, 3140.00, 12.00 ]
*/

// u want to write transactions of delivery, payment, insurance
// to corresponding orders
// in a generic manner
// u'll start with a simple imperative side-effectul code like that:

type Row = Map[String,String]
type Table = Seq[Row]

trait Source {
  def getRows: Table
}

class InlineSource(val table:Table) extends Source {
  def getRows = table
}

val source = new InlineSource(Seq(
  Map("A" -> "ORD-17813",   "B" -> "250.00", "C" ->  "740.00"),
  Map("A" -> "ORD-6724215", "B" -> "150.00", "C" -> "2500.00"),
  Map("A" -> "ORD-21235",   "B" -> "300.00", "C" -> "5700.00")
))


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
def parseCellAsMoney(row:Row,  col:String):Money = Money(parseCellAsDouble(row, col))

class OrderRepository {
  def findOrder(id:String):OrderRef = OrderRef((Math.random() * 100000).toInt)
  def writeTx(order:OrderRef, txType: TransactionType, amount: Money): Unit = {
    println(s"Writing transaction of type $txType for order $order with amount = $amount")
  }
}

case class OrderRef(id:Long)

sealed trait TransactionType
case object  DeliveryCost         extends TransactionType
case object  PaymentFromRecipient extends TransactionType
case object  CashService          extends TransactionType

case class Money(amount: BigDecimal)


// program itself
val repo = new OrderRepository
source.getRows.foreach(processRow(_, repo)) // no result, we have no way to explore it





