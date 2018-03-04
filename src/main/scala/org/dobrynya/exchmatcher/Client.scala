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
    * @param security securities type
    * @param amount securities amount
    * @param price securities price
    * @return updated buyer's account
    */
  def buy(security: Securities.Value, amount: Int, price: Int): Client = {
    val dollars = balances.getOrElse(DOLLAR, 0) - amount * price
    val securitiesAmount = balances.getOrElse(security, 0) + amount
    copy(balances = this.balances ++ Map(DOLLAR -> dollars, security -> securitiesAmount))
  }

  /**
    * Sells securities.
    * @param security securities type
    * @param amount securities amount
    * @param price securities price
    * @return updated seller's account
    */
  def sell(security: Securities.Value, amount: Int, price: Int): Client = {
    val securitiesRemaining = balances.getOrElse(security, 0) - amount
    val dollars = balances.getOrElse(DOLLAR, 0) + amount * price
    copy(balances = this.balances ++ Map(security -> securitiesRemaining, DOLLAR -> dollars))
  }
}

/**
  * Provides handy methods.
  */
object Client {
  import Securities._

  private val clientString = """(.+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)""".r

  def from(client: String): Option[Client] = client match {
    case clientString(name, dollars, a, b, c, d) =>
      Try(Client(name, Map(DOLLAR -> dollars.toInt, A -> a.toInt, B -> b.toInt, C -> c.toInt, D -> d.toInt)))
        .toOption
    case _ => None
  }

  private val securityOrder = List(DOLLAR, A, B, C, D)

  def serialize(client: Client): String =
    client.name + "\t" + securityOrder.map(client.balances.withDefault(_ => 0)).mkString("\t")
}


/**
  * Corresponds possible security types.
  */
object Securities extends Enumeration {
  val DOLLAR, A, B, C, D = Value
}