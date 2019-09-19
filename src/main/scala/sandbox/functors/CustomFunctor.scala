package sandbox.functors

import cats._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object CustomFunctor {

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case (fa: Branch[A]) =>
          Branch(map(fa.left)(f), map(fa.right)(f))
        case (fa: Leaf[A]) =>
          Leaf(f(fa.value))
      }
  }
}
