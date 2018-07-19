import cats.free.Free
import cats.free.Free.liftF
import cats.arrow.FunctionK
import cats.{Eval, Id, Monad, ~>}
import cats.instances.all._

import scala.concurrent.{Await, Future}
import cats.Monad._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration





case class Column[A](name:String, value: A) {
  def mappedValue[B](f: A => B):B = f(value)
}

trait Context

case class Order(id:String)

sealed trait TableOpA[A]
case class Col(key: String, default: String) extends TableOpA[Column[String]]
case class MappedCol[R](key:String, default:String, mapper: String => R) extends TableOpA[Column[R]]
case class ResolveOrder(id: String) extends TableOpA[Option[Order]]
case class WriteTx(o: Order, txType:String, v:BigDecimal) extends TableOpA[Tx]
case class Log(m:String) extends TableOpA[Unit]
case object RowN extends TableOpA[Int]
case class SetCtx[C](c: C) extends TableOpA[Unit]
case class GetCtx[C]() extends TableOpA[C]


case class Tx(order:Order, txType:String, amount:BigDecimal)

type TableOp[A] = Free[TableOpA, A]

// bind a column
def column(key: String, default: String = ""): TableOp[Column[String]] = liftF[TableOpA, Column[String]](Col(key, default))
def mappedColumn[R](key: String, f: String => R, default: String = ""): TableOp[Column[R]] = liftF[TableOpA, Column[R]](MappedCol(key, default, f))
def resolveOrder(id: String): TableOp[Option[Order]] = liftF[TableOpA, Option[Order]](ResolveOrder(id))
def writeTx(o: Order, txType:String, v:BigDecimal): TableOp[Tx] = liftF[TableOpA, Tx](WriteTx(o, txType, v))
def log(m: String): TableOp[Unit] = liftF[TableOpA, Unit](Log(m))
def rowN:TableOp[Int] = liftF[TableOpA, Int](RowN)
def getCtx[C]:TableOp[C] = liftF[TableOpA, C](GetCtx[C]())
def setCtx[C](c: C):TableOp[Unit] = liftF[TableOpA, Unit](SetCtx(c))

def when[T](f: => Boolean)(op:TableOp[T]): TableOp[Option[T]] =
  for {
    p <- Free.pure(f):TableOp[Boolean]
    r <- if (p) op.map(Some(_)) else Free.pure(None):TableOp[Option[T]]
  } yield r

object TX {
  val PaymentFromRecipient = "paymentFromRecipient"
  val DeliveryCost = "deliveryCost"
}

case class MyContext(isOdd:Boolean)

def rowProgram =
  for {
    ctx   <- getCtx[MyContext]
    _     <- setCtx[MyContext](MyContext(!ctx.isOdd))
    rowN  <- rowN
    colA  <- column("A")
    colB  <- mappedColumn("B", _.toDouble)
    colC  <- mappedColumn("C", _.toDouble)
    x     <- log(s"$rowN Is odd: ${ctx.isOdd} pls log")
    order <- resolveOrder(colA.value)
    txs   <- if (order.isDefined) for {
              tx1 <- writeTx(order.get, TX.DeliveryCost, colB.value)
              tx2 <- writeTx(order.get, TX.PaymentFromRecipient, colC.value)
          } yield Seq(tx1, tx2)
          else for {
              _ <- log("wtf")
          } yield Seq.empty[Tx]
  } yield txs



val logs:StringBuffer = new StringBuffer()

case class CtxManager[C](initialCtx:C) {
  var ctx = initialCtx
  def get = ctx
  def set(c: C):Unit = {
    ctx = c
    ()
  }
}

def rowCompiler[C](
                    initialCtx:C,
                    row: Map[String,String],
                    rowN: Int,
                    ctxMan: CtxManager[C]
                  ): TableOpA ~> Future  =
new (TableOpA ~> Future) {
  def apply[A](fa: TableOpA[A]):Future[A] = {
    fa match {
      case ResolveOrder(id) => Future.successful(Some(Order(id)))
      case WriteTx(o, t, v) => Future.successful(Tx(o, t, v))
      case Log(s) => Future.successful({logs.append(s); ()})
      case Col(k, d) => Future.successful(Column[String](k, row.getOrElse(k, d)))
      case MappedCol(k, d, f) => Future.successful(Column(k, f(row.getOrElse(k, d))).asInstanceOf[A])
      case RowN => Future.successful(rowN)
      case GetCtx() => Future.successful(ctxMan.get.asInstanceOf[A])
      case SetCtx(c) => Future.successful({ctxMan.modify(fn); ()})
      case _ => Future.successful(null.asInstanceOf[A])
    }
  }
}

val table = Seq(
  Map("A" -> "ORD1", "B" -> "12.3", "C" -> "120.12"),
  Map("A" -> "ORD2", "B" -> "100",  "C" -> "120.00"),
  Map("A" -> "ORD3", "B" -> "100",  "C" -> "120.00"),
  Map("A" -> "ORD4", "B" -> "100",  "C" -> "120.00")
)



def rowsCompiler[C,R](program:Free[TableOpA, R], initialCtx:C, rows:Seq[Map[String,String]]) = {
  val ctxMan = CtxManager(initialCtx)
  Future.sequence(rows.zipWithIndex.map( (k, row) => program.foldMap(rowCompiler(initialCtx, row, k, ctxMan))))
}



val f = rowsCompiler(rowProgram, MyContext(false), table)

Await.ready(f, Duration.Inf).value.get

logs

