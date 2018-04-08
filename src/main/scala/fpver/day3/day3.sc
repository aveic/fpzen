import fpver.day3._

val source = new InlineSource(Seq(
  Map("A" -> "ORD-17813",   "B" -> "250.00", "C" ->  "740.00"),
  Map("A" -> "ORD-6724215", "B" -> "150.00", "C" -> "2500.00"),
  Map("A" -> "ORD-21235",   "B" -> "300.00", "C" -> "5700.00")
))

// the program itself
def processRow(row: Row, repo:OrderRepository):OptionT[LogsWriter, Seq[Transaction]] = {
  val txs = for {
    orderId  <- row.column("A")
    payment  <- row.asMoney("B")
    delivery <- row.asMoney("C")
    orderRef <- repo.findOrder(if (orderId == "ORD-17813") "not_found" else orderId)
  } yield Seq(
    repo.writeTx(orderRef, PaymentFromRecipient, payment),
    repo.writeTx(orderRef, DeliveryCost, delivery)
  )

  txs
}


// the program runner
val repo = new OrderRepository(source)
val txs = source.getRows map { processRow(_, repo).value }

// yet another pattern
def sequence[A, F[_] : Monad](seq:Seq[F[A]]):F[Seq[A]] = {
  val monad = implicitly[Monad[F]]
  seq.foldRight(monad.pure(Seq.empty[A])) { (elF, accF) =>
    monad.flatMap(elF)( el => monad.flatMap(accF)(acc => monad.pure(acc :+ el)))
  }
}

val result = sequence(txs) map (_.flatten)

println(result.value)
println(result.written)










