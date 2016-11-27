package org.dobrynya.exchmatcher

/**
  * Provides operations specific to Exchange.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 26.11.16 1:27.
  */
case class Exchange(clients: Map[String, Client], bids: List[Order], asks: List[Order]) {
  def this(clients: Set[Client] = Set.empty) = this(clients.map(c => c.name -> c).toMap, List.empty, List.empty)

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
            findMatching(order, asks).map {
              case (ask, remainingAsks) =>
                Exchange(clients ++ makeDeal(bid, ask), bids, remainingAsks)
            } getOrElse copy(bids = this.bids :+ bid)
          case ask: Ask =>
            findMatching(order, bids) map {
              case (bid, remainingBids) =>
                Exchange(clients ++ makeDeal(bid, ask), remainingBids, asks)
            } getOrElse copy(asks = this.asks :+ ask)
        })
    } else Left(UnknownClient)
  }

  private[exchmatcher] def makeDeal(bid: Order, ask: Order) = {
    val buyer = clients(bid.client).buy(bid.security, bid.amount, bid.price)
    val seller = clients(ask.client).sell(ask.security, ask.amount, ask.price)
    Map(bid.client -> buyer, ask.client -> seller)
  }

  private[exchmatcher] def findMatching(order: Order, orders: List[Order]): Option[(Order, List[Order])] = {
    def notMatching(o1: Order)(o2: Order) =
      !(o1.security == o2.security && o1.amount == o2.amount && o1.price == o2.price && o1.client != o2.client)

    val (prefix, suffix) = orders.span(notMatching(order))
    suffix.headOption.map(matching => (matching, prefix ::: suffix.tail))
  }
}

trait ProcessingError
case object UnknownClient extends ProcessingError