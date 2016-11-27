package org.dobrynya.exchmatcher

import org.scalatest._
import Securities._


/**
  * Specification on Order.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 26.11.16 22:47.
  */
class OrderSpec extends FlatSpec with Matchers {
  behavior of "Order"

  it should "not parse invalid string" in {
    Order.from("wrong string") should matchPattern {
      case None =>
    }
  }

  it should "successfully parse orders from strings" in {
    List("C8\tb\tC\t15\t4", "C2\ts\tC\t14\t5", "C2\ts\tC\t13\t2").map(Order.from).toSet shouldBe Set(
      Some(Bid("C8", C, 15, 4)),
      Some(Ask("C2", C, 14, 5)),
      Some(Ask("C2", C, 13, 2))
    )
  }
}
