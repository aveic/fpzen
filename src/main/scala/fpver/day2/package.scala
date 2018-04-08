package fpver

package object day2 {
  type Row = Map[String,String]
  type Table = Seq[Row]

  case class OrderRef(id:Long)

  sealed trait TransactionType
  case object  DeliveryCost         extends TransactionType
  case object  PaymentFromRecipient extends TransactionType
  case object  CashService          extends TransactionType

  case class Money(amount: BigDecimal) extends AnyVal
}
