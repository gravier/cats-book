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
import scala.language.implicitConversions
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
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
    GCounter(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    this.counters.values.toList.combineAll
}

trait GCounter2[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(
      implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(
      implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  def apply[F[_, _], K, V](implicit store: KeyValueStore[F]) =
    store

  implicit def mapInstance[K, V] = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
    def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }
}

//trait GCounter3[KeyValueStore, K, V] {
//  def increment(f: KeyValueStore[K, V])(k: K, v: V)(
//    implicit m: CommutativeMonoid[V]): KeyValueStore[K, V]
//
//  def merge(f1: KeyValueStore[K, V], f2: F[K, V])(
//    implicit b: BoundedSemiLattice[V]): KeyValueStore[K, V]
//
//  def total(f: KeyValueStore[K, V])(implicit m: CommutativeMonoid[V]): V
//}

object GCounter2 {
  def apply[F[_, _], K, V](implicit counter: GCounter2[F, K, V]) =
    counter

  implicit def mapInstance[K, V] = new GCounter2[Map, K, V] {
    def increment(f: Map[K, V])(k: K, v: V)(
        implicit m: CommutativeMonoid[V]): Map[K, V] = {
      val value = v |+| f.getOrElse(k, m.empty)
      f + (k -> value)
    }

    def merge(f1: Map[K, V], f2: Map[K, V])(
        implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2

    def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.toList.combineAll
  }

  implicit def kvStoreInstance[S[_, _], K, V](implicit store: KeyValueStore[S]) =
    new GCounter2[S, K, V] {
      def increment(f: S[K, V])(k: K, v: V)(
          implicit m: CommutativeMonoid[V]): S[K, V] = {
        val value = v |+| store.getOrElse(f)(k, m.empty)
        store.put[K, V](f)(k, value)
      }

      def total(f: S[K, V])(implicit m: CommutativeMonoid[V]): V =
        store.values(f).combineAll

      def merge(f1: S[K, V], f2: S[K, V])(implicit b: BoundedSemiLattice[V]): S[K, V] = f1
    }
}
