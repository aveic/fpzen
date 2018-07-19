import fpver.day4._

val source = new InlineSource(Seq(
  Map("A" -> "ORD-17813",   "B" -> "250.00", "C" ->  "740.00"),
  Map("A" -> "ORD-6724215", "B" -> "150.00", "C" -> "2500.00"),
  Map("A" -> "ORD-21235",   "B" -> "300.00", "C" -> "5700.00")
))

// we could provide it with row syntax, but it's better to have a generic combinator
// since it's often what we need
implicit class Ops[A](a:OptionT[LogsWriter, A]) {
  // TODO write it better?
  def defaulted(default:A):OptionT[LogsWriter, A] = if (a.value.value.isDefined) a else OptionT(Writer(a.value.written, Some(default)))
  def orElse(b:OptionT[LogsWriter, A]) = if (a.value.value.isDefined) a else {
    val monoid = implicitly[Monoid[Logs]]
    OptionT(Writer(monoid.combine(a.value.written, b.value.written), b.value.value))
  }
}

// the program itself
// introduce skipping
// and rowNumber

// arbitrary details: repo, orderRef, for each new "domain" we introduce dependency
//
def processRow(row: Row, repo:OrderRepository):OptionT[LogsWriter, Seq[Transaction]] = {
  val txs = for {
    orderId  <- row.column("A")  // same for row
    payment  <- row.asMoney("B") // and repo
    delivery <- row.asMoney("C") defaulted Money(34.5)
    // TODO: orElse is part of OptionT?
    orderRef <- repo.findOrder(if (orderId == "ORD-17813") "not_found" else orderId) orElse repo.createOrder("AC-" + orderId)
    tx1 <- repo writeTx (orderRef, PaymentFromRecipient, payment)
    tx2 <- repo writeTx (orderRef, DeliveryCost, delivery)
  } yield tx1 ++ tx2

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

repo.dumpTransactions










