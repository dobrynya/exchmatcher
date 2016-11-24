package org.dobrynya.exchmatcher

import org.scalatest._

/**
  * Specification in Client.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 25.11.16 0:28.
  */
class ClientSpec extends FlatSpec with Matchers {
  import org.dobrynya.exchmatcher.Securities._

  "Client" should "be successfully parsed from line" in {

    Client.from("C1\t1000\t130\t240\t760\t320") should matchPattern {
      case Some(Client("C1", balances))
        if balances == Map(DOLLAR -> 1000, A -> 130, B -> 240, C -> 760, D -> 320) =>
    }

    Client.from("C2\t4350\t370\t120\t950\t560") should matchPattern {
      case Some(Client("C2", balances))
        if balances == Map(DOLLAR -> 4350, A -> 370, B -> 120, C -> 950, D -> 560) =>
    }

    Client.from("C3\t2760\t0\t0\t0\t0") should matchPattern {
      case Some(Client("C3", balances))
        if balances == Map(DOLLAR -> 2760, A -> 0, B -> 0, C -> 0, D -> 0) =>
    }
  }

  "Client" should "not be parsed from incorrectly formatted string" in {
    List("C2\t4350\t370\t120\t950\t", "C3\t2760\t0\t0", "1000\t130\t240\t760\t320")
      .map(Client.from).forall(_.isEmpty) should equal(true)
  }

  "Client" should "buy securities for specified price" in {
    Client("C1", Map(DOLLAR -> 100, A -> 0)).buy(A, 10, 10) should matchPattern {
      case Client("C1", balances) if balances == Map(DOLLAR -> 0, A -> 10) =>
    }

  }

  "Client" should "be overdrafted and buy securities in case of insufficient funds" in {
    Client("C1", Map(DOLLAR -> 0, A -> 0)).buy(A, 10, 10) should matchPattern {
      case Client("C1", balances) if balances == Map(DOLLAR -> -100, A -> 10) =>
    }
  }

  "Client" should "sell securities for specified price" in {
    Client("C2", Map(DOLLAR -> 0, A -> 100)).sell(A, 100, 1) should matchPattern {
      case Client("C2", balances) if balances == Map(DOLLAR -> 100, A -> 0) =>
    }
  }

  "Client" should "be overdrafted and sell securities in case of insufficient funds" in {
    Client("C1", Map(DOLLAR -> 0, A -> 0)).sell(A, 10, 10) should matchPattern {
      case Client("C1", balances) if balances == Map(DOLLAR -> 100, A -> -10) =>
    }
  }
}
