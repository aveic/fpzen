trait Expr[+T] {
  def AS(alias:Symbol) = AliasedExpr(alias, this)
}

case class AliasedExpr[+T](alias:Symbol, e:Expr[T]) extends Expr[T]

trait Ident
trait Table extends Ident

trait Col[+T] extends Expr[T] {

}

object tbl_order extends Table {
  val id: Col[String] = new Col[String] {}
  def customer_id: Col[String] = new Col[String] {}
  def status:Col[String] = new Col[String] {}
}

object tbl_customer extends Table {
  def id:Col[String] = new Col[String] {}
  def company_group_id:Col[String] = new Col[String] {}
}

object tbl_company_group extends Table {
  def id:Col[String] = new Col[String] {}
  def name:Col[String] = new Col[String] {}
}

implicit class ColOps[A](c:Col[A]) {
  def >(e:Expr[Any]):Expr[Boolean]   = new Expr[Boolean] {}
  def <(e:Expr[Any]):Expr[Boolean]   = new Expr[Boolean] {}
  def >=(e:Expr[Any]):Expr[Boolean]   = new Expr[Boolean] {}
  def <=(e:Expr[Any]):Expr[Boolean]   = new Expr[Boolean] {}

  def +(e:Expr[Any]):Expr[Any]       = new Expr[Any] {}
  def -(e:Expr[Any]):Expr[Any]       = new Expr[Any] {}
  def *(e:Expr[Any]):Expr[Any]       = new Expr[Any] {}
  def /(e:Expr[Any]):Expr[Any]       = new Expr[Any] {}

  def ===(e:Expr[Any]):Expr[Boolean]  = new Expr[Boolean] {}
  def IN(v:Any*):Expr[Boolean]        = new Expr[Boolean] {}
  def IN(se:SelectExpr):Expr[Boolean] = new Expr[Boolean] {}
  def NOT_IN(v:Any*):Expr[Boolean]    = new Expr[Boolean] {}
}

implicit def anyValToExpr[A <: AnyVal](v:A):Expr[A] = new Expr[A]{}

trait SelectExpr extends Expr[Any] {}

object SELECT extends SelectExpr {
  def apply(cols:Expr[Any]*) = this
  def apply(all: *.type) = this
  def FROM(tbl:Table) = this
  def WHERE(e:Expr[Boolean]) = this
  def INNER_JOIN(t:Table) = this
  def LEFT_JOIN(t:Table) = this
  def ON(e:Expr[Any]) = this
  def GROUP_BY(e:Expr[Any]*) = this
  def ORDER_BY(e:SortExpr*) = this
  def WINDOW(s:Symbol) = new ExpectWindowClause {}
  override def AS(ident:Symbol) = AliasedExpr(ident, this)
}

trait ExpectWindowClause {
  def AS(wc:WindowClause): SELECT.type = SELECT
}

case class SortExpr(e:Expr[Any], direction:String)

implicit class SortOps[A](e:Expr[A]) {
  def ASC:  SortExpr  = SortExpr(e, "ASC")
  def DESC: SortExpr  = SortExpr(e, "DESC")
}

def select(cols:Expr[Any]*) = SELECT.apply(cols:_*)

object * {

}

trait Join

def JOIN(t:Table) = new Join {}

val x = 5

case class With() {
  def SELECT(cols:Expr[Any]*) = select(cols:_*)
}

case class WithStart(s:Symbol) {
  def AS(s: SELECT.type) = new With()
}

def WITH(s:Symbol) = WithStart(s)

implicit class BooleanExprOps(b:Expr[Boolean]) {
  def AND(e:Expr[Boolean]):Expr[Boolean] = new Expr[Boolean] {}
  def OR(e:Expr[Boolean]):Expr[Boolean] = new Expr[Boolean] {}
}

case class BetweenSyntaxStart(e:Any, a:Any) {
  def AND(b:Any):Expr[Boolean] = new Expr[Boolean] {}
}

object NOT {
  def apply(e:Expr[Boolean]):Expr[Boolean] = new Expr[Boolean] {}
}

object EXISTS {
  def apply(e:SelectExpr):Expr[Boolean] = new Expr[Boolean] {}
}

implicit class BetweenOps(e:Any) {
  def BETWEEN(a:Any):BetweenSyntaxStart = BetweenSyntaxStart(e, a)
}

implicit class SymbolOps(rel:Symbol) {
  def !(id:Symbol):Expr[Any] = new Expr[Any] {}
}

case class When(c: CASE.type, cond:Expr[Boolean]) {
  def THEN(e:Expr[Any]): CASE.type = c
}

object CASE {
  def apply(e:Expr[Any]) = this
  def WHEN(cond:Expr[Boolean]) = When(this, cond)
  def ELSE(e:Expr[Any]) = this
  def END: Expr[Boolean] = new Expr[Boolean] {}
}

object TRUE  extends Expr[Boolean]
object FALSE extends Expr[Boolean]

trait WindowClause

trait WindowClauseHasPartition extends WindowClause {
  def ORDER_BY(se:SortExpr*) = this
}


object PARTITION_BY {
  def apply(col:Expr[Any]*) = new WindowClauseHasPartition {}

}

object ORDER_BY {
  def apply(col:Expr[Any]*) = new WindowClause {}

}

trait AggrExpr[A] extends Expr[A] {
  def OVER(w:WindowClause = null) = this
  def OVER(s:Symbol) = this
}

object MIN {
  def apply[A](e:Expr[A]):AggrExpr[A] = new AggrExpr[A] {}
}

object MAX {
  def apply[A](e:Expr[A]):AggrExpr[A] = new AggrExpr[A] {}
}

object AVG {
  def apply[A](e:Expr[A]):AggrExpr[A] = new AggrExpr[A] {}
}


// aliases
val (o,ct,cg) = (tbl_order,tbl_customer,tbl_company_group)

val query = (
  SELECT (
      o.id
    , o.status
  )
    FROM  tbl_order
    INNER_JOIN tbl_customer      ON (o.customer_id === ct.id)
    LEFT_JOIN  tbl_company_group ON (ct.company_group_id === cg.id)
    WHERE NOT ( ct.id > bind[Int]('customerId)
    AND (o.status IN ("intransit", "delivered"))
    AND (o.id BETWEEN bind[Int]('start) AND bind[Int]('end))
    AND (CASE
    WHEN TRUE  THEN 4
    WHEN FALSE THEN 7
    ELSE 5
    END
    )
    AND ( ct.id IN (SELECT (o.id) FROM tbl_customer) )
  )
    GROUP_BY o.id
    ORDER_BY (o.id ASC, cg.id + 2 DESC)
  )


def fn[A](fn:String)(args:Expr[Any]*):Expr[A] = new Expr[A] {}
def plain[A](expr:String):Expr[A] = new Expr[A] {}
def bind[A](s:Symbol):Expr[A] = new Expr[A] {}
def bind[A](s:Symbol, e:Expr[A]):Expr[A] = new Expr[A] {}

query