package fpver.day3

trait LogMessage

case class OrderNotFound(externalId:String) extends LogMessage
case class EmptyColumn(name:String) extends LogMessage
case class SimpleMessage(msg:String) extends LogMessage
case class ExceptionMessage(e:Throwable) extends LogMessage

object Logs {
  def fromThrowable(e:Throwable):Logs = Vector(ExceptionMessage(e))
  def simple(s:String):Logs = Vector(SimpleMessage(s))
}


