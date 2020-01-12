package sandbox.check

import cats.Semigroup
import cats.instances.list._
import cats.syntax.either._
// for Semigroup
import cats.syntax.semigroup._ // for |+|
//
//trait Check[E, A] {
//  def apply(value: A): Either[E, A]
//
//  def and(that: Check[E, A])(implicit s: Semigroup[E]): Check[E, A]
//}

final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] =
    func(a)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] = {
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e1), Right(_)) => Left(e1)
        case (Right(_), Left(e2)) => Left(e2)
        case (Right(_), Right(_)) => a.asRight
      }
    }
  }
}

sealed trait Check[E, A] {
  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)) match {
          case (Left(e1), Left(e2))   => (e1 |+| e2).asLeft
          case (Left(e), Right(a))    => e.asLeft
          case (Right(a), Left(e))    => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }
    }
}

final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

final case class And[E, A](left: Check[E, A], right: Check[E, A])
    extends Check[E, A]
