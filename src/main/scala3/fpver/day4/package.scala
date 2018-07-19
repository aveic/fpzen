package fpver

import scala.util.Try

package object day4 {

  type LogsWriter[A] = day4.Writer[Logs, A]

  case class Row(cells:Map[String,String]) {
    def column(name:String):day4.OptionT[LogsWriter, String]  = cells.get(name) loggedWith Logs.simple(s"Column $name doesn't exist")
    def asDouble(col:String):day4.OptionT[LogsWriter, Double] = for {
      v <- column(col)
      d <- OptionT(Try(v.toDouble).write[Logs](Logs.fromThrowable(_)))
    } yield d

    def asMoney(col:String):day4.OptionT[LogsWriter, Money]  = asDouble(col) map { Money(_) }
  }

  type Table = Seq[Row]

  case class OrderRef(id:Long)

  sealed trait TransactionType
  case object  DeliveryCost         extends TransactionType
  case object  PaymentFromRecipient extends TransactionType
  case object  CashService          extends TransactionType

  case class Money(amount: BigDecimal) extends AnyVal


  case class Transaction(orderRef: OrderRef, txType: TransactionType, amount: Money)



  type Logs = Vector[LogMessage]

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    override def empty: Vector[A] = Vector.empty[A]
    override def combine(a: Vector[A], b: Vector[A]): Vector[A] = a ++ b
  }

  implicit class LogsOps(m:LogMessage) {
    def toLogs:Logs = Vector(m)
  }

  implicit class WriterOps[A](v:A) {
    def asWriter:Writer[Logs, A] = Writer(Vector(), v)
  }

  implicit class WriterTryOps[A](ta:Try[A]) {
    def write[W : Monoid](f: Throwable => W):Writer[W,Option[A]] = Writer.fromTry(ta, f)
  }

  implicit class OptionOps[A](oa:Option[A]) {
    def loggedWith(logs: Logs):OptionT[LogsWriter, A] = OptionT(Writer.fromOption(oa, logs))
  }

  implicit class ValueOps[A](a:A) {
    def loggedWith(logs:Logs):OptionT[LogsWriter, A] = OptionT(Writer(logs, Some(a)))
  }
}
