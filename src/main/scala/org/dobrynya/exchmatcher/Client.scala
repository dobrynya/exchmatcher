package org.dobrynya.exchmatcher

import scala.util.Try


/**
  * Represents a client of the exchange.
  * @param name name of the client
  * @param balances current client's balances of securities and currency
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 25.11.16 0:03.
  */
case class Client(name: String, balances: Map[Securities.Value, Int]) {
  import Securities._

  /**
    * Buys securities.
    * @param securities securities type
    * @param amount securities amount
    * @param price securities price
    * @return updated buyer's account
    */
  def buy(securities: Securities.Value, amount: Int, price: Int): Client = {
    val dollars = balances(DOLLAR) - amount * price
    val securitiesAmount = balances(securities) + amount
    copy(balances = this.balances + (DOLLAR -> dollars) + (securities -> securitiesAmount))
  }

  /**
    * Sells securities.
    * @param securities securities type
    * @param amount securities amount
    * @param price securities price
    * @return updated seller's account
    */
  def sell(securities: Securities.Value, amount: Int, price: Int): Client = {
    val securitiesRemaining = balances(securities) - amount
    val dollars = balances(DOLLAR) + amount * price
    copy(balances = this.balances + (securities -> securitiesRemaining) + (DOLLAR -> dollars))
  }
}

/**
  * Provides handy methods.
  */
object Client {
  import Securities._

  private val clientString = """(.+)\t(\d{1,})\t(\d{1,})\t(\d{1,})\t(\d{1,})\t(\d{1,})""".r

  def from(client: String): Option[Client] = client match {
    case clientString(name, dollars, a, b, c, d) =>
      Try(Client(name, Map(DOLLAR -> dollars.toInt, A -> a.toInt, B -> b.toInt, C -> c.toInt, D -> d.toInt)))
        .toOption
    case _ => None
  }
}


/**
  * Corresponds possible security types.
  */
object Securities extends Enumeration {
  val DOLLAR, A, B, C, D = Value
}