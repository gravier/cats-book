package sandbox.monoids

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

object OrBoolean extends Monoid[Boolean] {
  def empty: Boolean = false

  def combine(x: Boolean, y: Boolean): Boolean = x || y
}

object AndBoolean extends Monoid[Boolean] {
  def empty: Boolean = true

  def combine(x: Boolean, y: Boolean): Boolean = x && y
}

object ConcatSet extends Monoid[Set[Int]] {
  def empty: Set[Int] = Set()

  def combine(x: Set[Int], y: Set[Int]): Set[Int] = x ++ y
}
