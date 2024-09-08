case class CreditCard() {
  // def charge(price: Double) = () // credit cards should not be able to charge. they should only contain data
}

case class Coffee(price: Double = 1.99)

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Throwable("Can't combine charges to different cards")
}

class Cafe {
  /*def buyCoffee(cc: CreditCard): Coffee = {
    val cup = Coffee()
    cc.charge(cup.price) // side effect, performs a network request
    cup
  }*/

  // eliminate the side effect by returning the charge as a value. gather all
  // the chargers and perform the network requests once. this will be done elsewhere.
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    (cup, Charge(cc, cup.price))
  }
  // We saw in the case of buyCoffee how we could separate the creation of the Charge from
  // the interpretation or processing of that Charge.

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  // Since Charge is first-class, we can write the following function to coalesce any same-card charges in a List[Charge]:
  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}


