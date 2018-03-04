package org.dobrynya.exchmatcher

import Exchange._

/**
  * Provides operations specific to Exchange.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 26.11.16 1:27.
  */
case class Exchange(clients: Map[String, Client], bids: IndexedOrderQueue, asks: IndexedOrderQueue) {

  def this(clients: Set[Client] = Set.empty) = this(clients.map(c => c.name -> c).toMap, Map.empty, Map.empty)

  /**
    * Searches for the client portfolio scpecified by client name.
    * @param client client name
    * @return some client portfolio or none
    */
  def portfolio(client: String): Option[Client] = clients.get(client)

  /**
    * Adds or updates client's portfolio.
    * @param client client portfolio
    * @return new exchange state
    */
  def updatePortfolio(client: Client): Exchange = copy(clients = this.clients + (client.name -> client))

  /**
    * Processes an order to produce an new exchange state or an error.
    * @param order a bid or an ask to process by this exchange
    * @return new exchange state or an error
    */
  def process(order: Order): Either[ProcessingError, Exchange] = {
    if (clients contains order.client) {
      Right(
        order match {
          case bid: Bid =>
            findMatching(bid, asks).map {
              case (ask, remainingAsks) =>
                Exchange(clients ++ makeDeal(bid, ask), bids, remainingAsks)
            } getOrElse copy(bids = addToQueue(bid, bids))
          case ask: Ask =>
            findMatching(ask, bids) map {
              case (bid, remainingBids) =>
                Exchange(clients ++ makeDeal(bid, ask), remainingBids, asks)
            } getOrElse copy(asks = addToQueue(ask, asks))
        })
    } else Left(UnknownClient)
  }

  private[exchmatcher] def addToQueue(order: Order, queue: IndexedOrderQueue) = {
    println("%s cannot be matched, so enqueued" format order)
    queue.updated(order.index, queue.getOrElse(order.index, Nil) :+ order)
  }

  private[exchmatcher] def makeDeal(bid: Order, ask: Order) = {
    val buyer = clients(bid.client).buy(bid.security, bid.amount, bid.price)
    val seller = clients(ask.client).sell(bid.security, bid.amount, bid.price)
    Map(bid.client -> buyer, ask.client -> seller)
  }

  /**
    * Searches for a matching order.
    * @param order order to find a matching order for
    * @param indexedQueue current buy/offer queue
    * @return some matching order and the rest of the current queue or none
    */
  private[exchmatcher] def findMatching(order: Order, indexedQueue: IndexedOrderQueue): Option[(Order, IndexedOrderQueue)] = {
    indexedQueue.get(order.index).flatMap { queue =>
      val (sameClient, suffix) = queue.span(_.client == order.client)
      suffix.headOption.map { matching =>
        val actualQueue = sameClient ::: suffix.tail
        println("Found %s for request %s".format(matching, order))
        (matching, if (actualQueue.isEmpty) indexedQueue - order.index else indexedQueue + (order.index -> actualQueue))
      }
    }
  }
}

object Exchange {
  type IndexedOrderQueue = Map[(Securities.Value, Int, Int), List[Order]]
}

trait ProcessingError
case object UnknownClient extends ProcessingError