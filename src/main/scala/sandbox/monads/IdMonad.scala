package sandbox.monads
import cats.Id

object IdFuncs {
  def pure[A](value: A): Id[A] =
    value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] =
    func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
    func(initial)

  pure(123)

  map(123)(_ + 1)
}
