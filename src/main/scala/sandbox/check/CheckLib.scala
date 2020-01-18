package sandbox.check

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.instances.list._
import cats.syntax.validated._
import cats.syntax.apply._
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
  def or(that: Check[E, A]): Check[E, A] =
    Or(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        (left(a), right(a)) match {
          case (Valid(a), Invalid(e))     => a.valid
          case (Valid(a1), Valid(a2))     => a1.valid
          case (Invalid(e), Valid(a))     => a.valid
          case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
        }
    }
}

final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

final case class And[E, A](left: Check[E, A], right: Check[E, A])
    extends Check[E, A]

final case class Or[E, A](left: Check[E, A], right: Check[E, A])
    extends Check[E, A]
