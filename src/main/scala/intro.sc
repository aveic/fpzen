case class CreditCard(pan: Int)
case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge):Charge = {
    if (other.cc == cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards")
  }
}
case class Coffee() {
  val price = 10.00
}

class Cafe {
  def buyCoffee(cc:CreditCard):(Coffee,Charge) = {
    val cup = new Coffee

    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc:CreditCard, n:Int):(List[Coffee], Charge) = {

    val purchases = List.fill(n)(buyCoffee(cc))

    val (coffees, charges) = purchases.unzip

    (coffees, charges.reduce(_ combine _))
  }
}

class PaymentGateway {
  def process(charges: List[Charge]): Unit = {println("processing shit")}
}


def coalesce(charges: List[Charge]):List[Charge] = {
  charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

val cafe = new Cafe

val cc1 = CreditCard(12345678)
val cc2 = CreditCard(77777777)

val (_, charges1) = cafe.buyCoffees(cc1, 2)
val (_, charges2) = cafe.buyCoffees(cc2, 10)
val (_, charges3) = cafe.buyCoffees(cc2, 5)

val charges = coalesce(List(charges1, charges2, charges3))

(new PaymentGateway).process(charges)








