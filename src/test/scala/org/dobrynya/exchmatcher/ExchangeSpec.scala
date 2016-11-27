package org.dobrynya.exchmatcher

import java.io.PrintWriter
import org.scalatest._
import Securities._
import scala.io.Source

/**
  * Specification on Exchange.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 26.11.16 22:20.
  */
class ExchangeSpec extends FlatSpec with Matchers {
  behavior of "Exchange"

  it should "not find a non-existent client portfolio" in {
    val exch = new Exchange()

    exch.portfolio("non-existent") should matchPattern {
      case None =>
    }
  }

  it should "find an existing portfolio" in {
    val client = Client("C1", Map(DOLLAR -> 0))
    val exch = new Exchange(Set(client))
    exch.portfolio("C1") should matchPattern {
      case Some(`client`) =>
    }
  }

  it should "add a new client portfoilio" in {
    val client = Client("C2", Map(DOLLAR -> 1000))
    val exch = new Exchange()
    val updatedExchange = exch.updatePortfolio(client)
    updatedExchange.portfolio("C2") should matchPattern {
      case Some(`client`) =>
    }
  }

  it should "replace a client portfolio with a new one" in {
    val updatedPortfolio = Client("C3", Map(DOLLAR -> 100, A -> 150))
    val exch = new Exchange(Set(Client("C3", Map(DOLLAR -> 1000))))
    exch.updatePortfolio(updatedPortfolio).portfolio("C3") should matchPattern {
      case Some(`updatedPortfolio`) =>
    }
  }

  it should "not process orders if a client is unknown" in {
    val exch = new Exchange()
    exch.process(Bid("C1", A, 25, 100)) should matchPattern {
      case Left(`UnknownClient`) =>
    }
  }

  it should "place a bid in case of absent matching ask" in {
    val client = Client("C1", Map(DOLLAR -> 1000))
    val bid = Bid("C1", A, 10, 10)
    new Exchange(Set(client)).process(bid) should matchPattern {
      case Right(exch: Exchange) if exch.bids == List(bid) =>
    }
  }

  it should "place an ask in case of absent matching bid" in {
    val client = Client("C1", Map(DOLLAR -> 1000))
    val ask = Ask("C1", A, 10, 10)
    new Exchange(Set(client)).process(ask) should matchPattern {
      case Right(exch: Exchange) if exch.asks == List(ask) =>
    }
  }

  it should "match a bid by specified security, price and amount" in {
    val ask = Ask("C2", A, 10, 10)
    new Exchange(Set(Client("C1", Map(DOLLAR -> 100)), Client("C2", Map(A -> 10)))).process(ask) match {
      case Right(exch1) if exch1.asks.contains(ask) =>
        exch1.process(Bid("C1", A, 10, 10)) match {
          case Right(exch2) if exch2.bids.isEmpty && exch2.asks.isEmpty =>
            exch2.portfolio("C1") should matchPattern {
              case Some(Client("C1", balances)) if balances == Map(DOLLAR -> 0, A -> 10) =>
            }
            exch2.portfolio("C2") should matchPattern {
              case Some(Client("C2", balances)) if balances == Map(DOLLAR -> 100, A -> 0) =>
            }
          case x =>
            fail(s"Unexpected result: $x!")
        }
      case x =>
        fail(s"Unexpected result: $x!")
    }
  }

  it should "match an ask by specified security, price and amount" in {
    val bid = Bid("C1", A, 10, 10)
    new Exchange(Set(Client("C1", Map(DOLLAR -> 100)), Client("C2", Map(A -> 10)))).process(bid) match {
      case Right(exch) if exch.bids.contains(bid) =>
        exch.process(Ask("C2", A, 10, 10)) match {
          case Right(exch1) if exch1.bids.isEmpty && exch1.asks.isEmpty =>
            exch1.portfolio("C1") should matchPattern {
              case Some(Client("C1", balances)) if balances == Map(DOLLAR -> 0, A -> 10) =>
            }
            exch1.portfolio("C2") should matchPattern {
              case Some(Client("C2", balances)) if balances == Map(DOLLAR -> 100, A -> 0) =>
            }
          case x =>
            fail(s"Unexpected result: $x!")
        }
      case x =>
        fail(s"Unexpected result: $x!")
    }
  }

  def aggregatePortfolios(clients: Iterable[Client]) =
    clients.map(_.balances)
      .foldLeft(Map(A -> 0, B -> 0, C -> 0, D -> 0)) {(acc, portfolio) =>
        acc.map(entry => entry._1 -> (entry._2 + portfolio.getOrElse(entry._1, 0)))
      }

  def runWithCheckings(clients: Set[Client], orders: Iterable[Order]): Exchange = {
    val exch = new Exchange(clients)
    val beforeTrading = aggregatePortfolios(clients)

    val afterTrading = orders.foldLeft(exch) { (exch, order) =>
      exch process order match {
        case Left(`UnknownClient`) =>
          println("Order %s could not be processed due to unknown client!")
          exch
        case Right(exchange) => exchange
      }
    }

    aggregatePortfolios(afterTrading.clients.values) should equal(beforeTrading)

    afterTrading
  }

  it should "correctly process synthesized orders so securities amounts should be the same before and after the trading day" in {
    val afterTrading = runWithCheckings(
      Set(Client("1", Map(DOLLAR -> 1000, A -> 1000)), Client("2", Map(DOLLAR -> 5000, B -> 1000))),
      List(
        Bid("1", B, 1, 20),
        Ask("2", B, 1, 20),
        Bid("2", A, 1, 20),
        Ask("1", A, 1, 20),
        Ask("2", B, 1, 380),
        Ask("1", A, 1, 380),
        Ask("2", B, 1, 500),
        Ask("1", A, 1, 500),
        Ask("1", A, 1, 100),
        Ask("2", B, 1, 100),
        Bid("1", B, 1, 380),
        Bid("2", A, 1, 380),
        Bid("1", B, 1, 500),
        Bid("2", A, 1, 500),
        Bid("1", B, 1, 100),
        Bid("2", A, 1, 100)
      ))

    afterTrading.portfolio("1") should matchPattern {
      case Some(Client("1", balances)) if balances == Map(DOLLAR -> 1000, A -> 0, B -> 1000) =>
    }
    afterTrading.portfolio("2") should matchPattern {
      case Some(Client("2", balances)) if balances == Map(DOLLAR -> 5000, A -> 1000, B -> 0) =>
    }

    afterTrading.bids shouldBe 'empty
    afterTrading.asks shouldBe 'empty
  }

  it should "correctly process orders from file and dump client portfolios to file after the trading day" in {
    val parsedClients =
      Source.fromResource("clients.txt").getLines.map(Client.deserialize).collect {
        case Some(client) => client
      }.toSet

    val parsedOrders = Source.fromResource("orders.txt").getLines.map(Order.from).collect {
      case Some(order) => order
    }.toList

    val afterTrading = runWithCheckings(parsedClients, parsedOrders)

    val w = new PrintWriter("result.txt")
    afterTrading.clients.values.toList.sortBy(_.name).map(Client.serialize).foreach(w.println)
    w.close()
  }
}