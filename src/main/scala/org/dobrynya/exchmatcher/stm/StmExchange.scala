package org.dobrynya.exchmatcher.stm

import zio._
import zio.console.Console
import zio.stm._

case class Client(id: String, securities: TMap[String, BigDecimal]) {
  def buy(security: String, amount: Int, price: BigDecimal) =
    STM.ifM(securities.getOrElse("USD", BigDecimal(0)).map(_ >= price * amount))(
      securities.merge("USD", price * amount)(_ - _) *> securities.merge(security, amount)(_ + _),
      STM.fail(s"Client $id has no sufficient funds: requested ${price * amount}!")
    )

  def sell(security: String, amount: Int, price: BigDecimal): STM[String, Unit] =
    STM.ifM(securities.getOrElse(security, BigDecimal(0)).map(_ >= amount))(
      securities.merge("USD", price * amount)(_ + _) *>
        securities.merge(security, amount)(_ - _) *>
        securities.removeIf((_, amount) => amount.intValue == 0),
      STM.fail(s"Client $id has no sufficient instrument amount: requested  $amount of $security!")
    )

  def printInfo =
    securities.toMap.commit >>= (s => console.putStrLn(s"Client $id (${s.mkString(", ")})"))
}

trait Order {
  def clientId: String

  def security: String

  def amount: Int

  def price: BigDecimal

  def matches(offer: Order): Boolean

  def decrease(delta: Int): Order

  def dealPrice(order: Order): BigDecimal
}

case class Ask(clientId: String, security: String, amount: Int, price: BigDecimal) extends Order {
  override def matches(offer: Order) = offer match {
    case Bid(clientId, security, _, price)
      if security == this.security && clientId != this.clientId && price >= this.price => true
    case _ => false
  }

  override def decrease(delta: Int): Order = copy(amount = amount - delta)

  override def dealPrice(order: Order): BigDecimal = price
}

case class Bid(clientId: String, security: String, amount: Int, price: BigDecimal) extends Order {
  override def matches(offer: Order) = offer match {
    case Ask(clientId, security, _, price)
      if security == this.security && clientId != this.clientId && price <= this.price => true
    case _ => false
  }

  override def decrease(delta: Int): Order = copy(amount = amount - delta)

  override def dealPrice(order: Order): BigDecimal = order.price
}

case class StmExchange(clients: TMap[String, Client], asks: TMap[String, List[Order]], bids: TMap[String, List[Order]]) {

  def createClient(id: String, securities: Map[String, BigDecimal] = Map.empty) =
    (for {
      tSecurities <- TMap.fromIterable(securities)
      client = Client(id, tSecurities)
      _ <- clients.put(id, client)
    } yield client).commit

  private def extract[T]: PartialFunction[Option[T], T] = {
    case Some(elem) => elem
  }

  def client(id: String) =
    STM.ifM(clients.contains(id))(clients.get(id).collect(extract), STM.fail(s"There is no client $id!"))

  private[stm] def placeOrder(order: Order, offers: TMap[String, List[Order]], store: TMap[String, List[Order]],
                              makeDeal: (String, String, String, Int, BigDecimal) => STM[String, Unit]) = {

    def placeOrder(order: Order, offers: List[Order], inappropriateOffers: List[Order]): STM[String, List[Order]] =
      offers match {
        case Nil =>
          store.merge(order.security, List(order))(_ ::: _).as(inappropriateOffers)
        case offer :: tail if !order.matches(offer) =>
          placeOrder(order, tail, inappropriateOffers :+ offer)
        case offer :: tail =>
          (for {
            _ <- makeDeal(offer.clientId, order.clientId, order.security,
              Math.min(offer.amount, order.amount), offer.dealPrice(order))
            remainder <-
              if (offer.amount < order.amount) placeOrder(order.decrease(offer.amount), tail, inappropriateOffers)
              else STM.succeed(
                inappropriateOffers :::
                  (if (offer.amount > order.amount) List(offer.decrease(offer.amount - order.amount)) else Nil) ::: tail
              )
          } yield remainder).catchAll(_ => placeOrder(order, tail, inappropriateOffers :+ offer))
      }

    for {
      allOffers <- offers.getOrElse(order.security, Nil)
      remainder <- placeOrder(order, allOffers, Nil)
      _ <- offers.put(order.security, remainder)
    } yield ()
  }

  def placeAsk(ask: Order) = placeOrder(ask, bids, asks, sell)

  def placeBid(bid: Order) = placeOrder(bid, asks, bids, buy)

  private[stm] def buy(sellerId: String, buyerId: String, security: String, amount: Int, price: BigDecimal) =
    sell(buyerId, sellerId, security, amount, price)

  private[stm] def sell(buyerId: String, sellerId: String, security: String, amount: Int, price: BigDecimal) =
    for {
      (buyer, seller) <- client(buyerId).zip(client(sellerId))
      _ <- buyer.buy(security, amount, price) *> seller.sell(security, amount, price)
    } yield ()

  def printInfo =
    clients.values.commit.flatMap(ZIO.foreach(_)(_.printInfo)).ignore *>
      console.putStrLn("Asks: ") *> (asks.values.map(_.flatten).commit >>=
      (asks => ZIO.foreach(asks)(a => console.putStrLn(s"$a")))) *>
      console.putStrLn("Bids: ") *> (bids.values.map(_.flatten).commit >>=
      (bids => ZIO.foreach(bids)(b => console.putStrLn(s"$b")))) *>
      console.putStrLn("=======================================================")
}

object StmExchange {
  def create =
    (for {
      clients <- TMap.empty[String, Client]
      bids <- TMap.empty[String, List[Order]]
      asks <- TMap.empty[String, List[Order]]
    } yield StmExchange(clients, asks, bids)).commit
}

object Main extends App {
  implicit def logError[R, E, A](zio: ZIO[R, E, A]) = new {
    def debug: ZIO[R with Console, E, Unit] =
      zio.catchAll(e => console.putStrLn(s"$e")).ignore
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      exch <- StmExchange.create
      _ <- exch.createClient("client1", Map("USD" -> BigDecimal(3500), "APPLE" -> BigDecimal("100")))
      _ <- exch.createClient("client2", Map("A" -> BigDecimal(30)))
      _ <- exch.createClient("client3", Map("A" -> BigDecimal(30)))
      _ <- exch.placeAsk(Ask("client2", "A", 30, BigDecimal(1))).commit.debug *> exch.printInfo
      _ <- exch.placeBid(Bid("client1", "A", 30, BigDecimal(1.8))).commit.debug *> exch.printInfo
      _ <- exch.placeBid(Bid("client1", "A", 20, BigDecimal(1.7))).commit.debug *> exch.printInfo
      _ <- exch.placeAsk(Ask("client3", "A", 30, BigDecimal(1))).commit.debug *> exch.printInfo
    } yield 0).fold(_ => 0, _ => 0)
}
