package fpver.day3

import scalikejdbc._

class OrderRepository(val fixtureSource:Source) {
  // initialize JDBC driver & connection pool
  Class.forName("org.postgresql.Driver")
  ConnectionPool.singleton("jdbc:postgresql://localhost:5432/domain", "postgres", "123")

  // ad-hoc session provider on the REPL
  implicit val session = AutoSession

  def init:Unit = {
    // create table order
    sql"""
create table if not exists tbl_order (
  id bigserial not null primary key,
  external_id TEXT UNIQUE
)
""".execute.apply()


    // create transactions table
    sql"""
create table if not exists tbl_transaction (
  id bigserial not null primary key,
  order_id bigint NOT NULL REFERENCES tbl_order(id) ON UPDATE CASCADE ON DELETE RESTRICT,
  type TEXT NOT NULL,
  amount NUMERIC(20,2) NOT NULL
)
""".execute.apply()

    // truncate shit
    sql"""
TRUNCATE tbl_transaction;
TRUNCATE tbl_order CASCADE;
""".execute.apply()

    fixtureSource.getRows.foreach(r => {
      val orderId = r.cells("A")
      sql"insert into tbl_order (external_id) values ($orderId)".update.apply()
    })
  }

  def findOrder(externalId:String):OptionT[LogsWriter, OrderRef] = {
    println(s"Fetch order by externalId = $externalId")
    val pkOption = sql"select id from tbl_order where external_id = $externalId".map(r => r.any(1).toString.toInt).first().apply()
    val refOption = pkOption map { OrderRef(_) }
    refOption loggedWith Logs.simple(s"Couldn't find order by external identity $externalId")
  }

  def writeTx(order:OrderRef, txType: TransactionType, amount: Money): Transaction = {
    println(s"Writing transaction of type $txType for order $order with amount = $amount")
    sql"insert into tbl_transaction (order_id, type, amount) values (${order.id}, ${txType.toString}, ${amount.amount})".update.apply()
    Transaction(order, txType, amount)
  }

  def dumpOrders:String = {
    val orders: List[Map[String, Any]] = sql"select * from tbl_order".map(_.toMap).list.apply()
    TableDumper.dumpTable(orders, "Orders")
  }

  def dumpTransactions:String = {
    val txs: List[Map[String, Any]] = sql"SELECT o.external_id AS order, tx.type AS tx_type, tx.amount FROM tbl_transaction AS tx INNER JOIN tbl_order AS o ON o.id = tx.order_id".map(_.toMap).list.apply()
    TableDumper.dumpTable(txs, "Transactions")
  }

  init
}