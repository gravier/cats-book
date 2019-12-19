import cats.Applicative
import cats.syntax.apply._ // for mapN
import cats.syntax.applicative._ // for pure
import scala.language.higherKinds

def listTraverse[F[_]: Applicative, A, B]
(list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B]
(list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

import cats.instances.vector._ // for Applicative
listSequence(List(Vector(1, 2), Vector(3, 4)))
listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

import cats.instances.option._ // for Applicative
def process(inputs: List[Int]) =
  listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

process(List(2, 4, 6))
process(List(1, 2, 3))

import cats.data.Validated
import cats.instances.list._ // for Monoid

type ErrorsOr[A] = Validated[List[String], A]

def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if(n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }

process2(List(2, 4, 6))
process2(List(1, 2, 3))


