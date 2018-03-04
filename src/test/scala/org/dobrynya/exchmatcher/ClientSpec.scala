package org.dobrynya.exchmatcher

import org.scalatest._

/**
  * Specification in Client.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 25.11.16 0:28.
  */
class ClientSpec extends FlatSpec with Matchers {
  behavior of "Client"

  import org.dobrynya.exchmatcher.Securities._

  it should "be successfully parsed from string" in {
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

  it should "not be parsed from incorrectly formatted string" in {
    List("C2\t4350\t370\t120\t950\t", "C3\t2760\t0\t0", "1000\t130\t240\t760\t320")
      .map(Client.from).forall(_.isEmpty) shouldBe true
  }

  it should "buy securities for specified price" in {
    Client("C1", Map(DOLLAR -> 100, A -> 0)).buy(A, 10, 10) should matchPattern {
      case Client("C1", balances) if balances == Map(DOLLAR -> 0, A -> 10) =>
    }

  }

  it should "be overdrafted and buy securities in case of insufficient funds" in {
    Client("C1", Map.empty).buy(A, 10, 10) should matchPattern {
      case Client("C1", balances) if balances == Map(DOLLAR -> -100, A -> 10) =>
    }
  }

  it should "sell securities for specified price" in {
    Client("C2", Map(DOLLAR -> 0, A -> 100)).sell(A, 100, 1) should matchPattern {
      case Client("C2", balances) if balances == Map(DOLLAR -> 100, A -> 0) =>
    }
  }

  it should "be overdrafted and sell securities in case of insufficient funds" in {
    Client("C1", Map(DOLLAR -> 0, A -> 0)).sell(A, 10, 10) should matchPattern {
      case Client("C1", balances) if balances == Map(DOLLAR -> 100, A -> -10) =>
    }
  }

  it should "correctly serialize clients" in {
    List(Client("1", Map(DOLLAR -> 1, B -> 1)), Client("2", Map(A -> 20)), Client("3", Map.empty),
      Client("4", Map(DOLLAR -> 30, A -> 1, B -> 5, C -> 7, D -> 8))) map Client.serialize should matchPattern {
      case List(
      "1\t1\t0\t1\t0\t0",
      "2\t0\t20\t0\t0\t0",
      "3\t0\t0\t0\t0\t0",
      "4\t30\t1\t5\t7\t8"
      ) =>
    }
  }
}
