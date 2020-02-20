package sandbox.crdt

//final case class GCounter(counters: Map[String, Int]) {
//  def increment(machine: String, amount: Int) =
//    GCounter(counters ++ Map( machine -> (counters.getOrElse(machine, 0) + amount)))
//
//  def merge(that: GCounter): GCounter =
//    GCounter(that.counters ++ this.counters.map {
//      case (k, v) =>
//        k -> (v max that.counters.getOrElse(k, 0))
//    })
//
//  def total: Int =
//    counters.values.sum
//}
//
//val a = GCounter(Map("A" -> 10))
//val b = GCounter(Map("B" -> 2))
//val ab = a.merge(b)
//val a2 = a.increment("A", 1)
//ab.merge(a2).total

import cats.Monoid
import cats.kernel.CommutativeMonoid
trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intInstance: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int =
        a1 max a2

      val empty: Int =
        0
    }

  implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] =
        a1 union a2

      val empty: Set[A] =
        Set.empty[A]
    }
}

import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._ // for combineAll

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(
      implicit m: Monoid[A]): GCounter[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    println(s"value: $value")
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
    GCounter(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    this.counters.values.toList.combineAll
}
