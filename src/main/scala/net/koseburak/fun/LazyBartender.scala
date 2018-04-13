package net.koseburak.fun

object LazyBartender {
  import scala.annotation.tailrec

  def find(cus2Drinks: Map[String, Set[String]]): Set[String] = {
    lazy val drinks2Cus = cus2Drinks.toList
      .flatMap { case (customer, drinks) =>
          drinks.map(_ -> customer)
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)

    @tailrec
    def inner(
        drinks2Cus: Map[String, Set[String]],
        drinks: Set[String],
        customers: Set[String]
    ): Set[String] = {
      if (customers.size < cus2Drinks.size) {
        val (maxDrink, maxCustomers) = drinks2Cus.maxBy(_._2.size)
        val (newDrinks, newsCustomers) =
          maxCustomers.foldLeft((drinks, customers)) {
            case ((accDrinks, accCustomers), customer) =>
              if (customers.contains(customer))
                (accDrinks, accCustomers)
              else
                (accDrinks + maxDrink, accCustomers + customer)
          }
        inner(drinks2Cus - maxDrink, newDrinks, newsCustomers)
      } else drinks
    }

    inner(drinks2Cus, Set.empty, Set.empty)
  }

  val input = Map(
    "cust1" -> Set("n3", "n7", "n5", "n2", "n9"),
    "cust2" -> Set("n5"),
    "cust3" -> Set("n2", "n3"),
    "cust4" -> Set("n4"),
    "cust5" -> Set("n3", "n4", "n5", "n7")
  )

  find(input)
}
