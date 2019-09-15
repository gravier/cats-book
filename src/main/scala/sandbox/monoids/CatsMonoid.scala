package sandbox.monoids

import cats.syntax.monoid._
import cats.instances.double._

object CatsMonoid {

  implicit def orderMonoid = new cats.Monoid[Order] {
    def empty: Order = Order(0, 0)

    def combine(x: Order, y: Order): Order =
      Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }

  def add[A](items: List[A])(implicit monoid: cats.Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)
}
case class Order(totalCost: Double, quantity: Double)
