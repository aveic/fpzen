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
    repo.writeTx(orderRef, PaymentFromRecipient, payment), // no point providing this info about orderRef
    repo.writeTx(orderRef, DeliveryCost, delivery)         // no point providing ref again
  )

  txs
}


// the program runner
val repo = new OrderRepository(source)
val txs = source.getRows map { processRow(_, repo).value }

// sequence pattern for applicatives
def sequence[A, F[_] : Applicative](seq:Seq[F[A]]):F[Seq[A]] = {
  val ap = implicitly[Applicative[F]]
  val zero = ap.pure(Seq.empty[A])
  seq.foldRight(zero) { (elF, accF) => ap.map2(elF, accF)(_ +: _) }
}

val result = sequence(txs) map (_.flatten)

println(result.value)
println(result.written)










