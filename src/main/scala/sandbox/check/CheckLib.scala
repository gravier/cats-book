package sandbox.check

import cats.Semigroup
import cats.data.{AndThen, Validated}
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

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] =
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

final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
    extends Predicate[E, A]

final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
    extends Predicate[E, A]

sealed trait Check[E, A, B] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen2(this, that)

}

object Check {
  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    Pure2(pred)
}

final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
    extends Check[E, A, C] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(in).map(func)
}

final case class FlatMap[E, A, B, C](check: Check[E, A, B],
                                     func: B => Check[E, A, C])
    extends Check[E, A, C] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(in).toEither.flatMap(b => func(b)(in).toEither).toValidated
}

final case class AndThen2[E, A, B, C](check1: Check[E, A, B],
                                      check2: Check[E, B, C])
    extends Check[E, A, C] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check1(in).withEither(e => e.flatMap(check2(_).toEither))
}

final case class Pure2[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
}
