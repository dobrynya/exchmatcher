package org.dobrynya.exchmatcher

import scala.util.{Failure, Try}

/**
  * Represents an order for operation with securities.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 26.11.16 23:16.
  */
sealed trait Order {
  def client: String
  def security: Securities.Value
  def price: Int
  def amount: Int

  def index: (Securities.Value, Int, Int) = (security, amount, price)
}

object Order {
  private val orderPattern = """(.+?)\t(b|s)\t(A|B|C|D)\t(\d{1,})\t(\d{1,})""".r

  def from(order: String): Option[Order] = order match {
    case orderPattern(client, orderType, security, price, amount) =>
        Try(
          if (orderType == "b")
            Bid(client, Securities.withName(security), price.toInt, amount.toInt)
          else
            Ask(client, Securities.withName(security), price.toInt, amount.toInt)
        ).toOption
    case t => None
  }
}

/**
  * Represens a bid for a security.
  * @param client client placed this bid
  * @param security security to buy
  * @param price security price
  * @param amount security amount
  */
case class Bid(client: String, security: Securities.Value, price: Int, amount: Int) extends Order

/**
  * Represents an ask for a security.
  * @param client client placed this ask
  * @param security security to sell
  * @param price security price
  * @param amount security amount
  */
case class Ask(client: String, security: Securities.Value, price: Int, amount: Int) extends Order

