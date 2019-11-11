package sandbox.monads
import cats.data.State
import cats.data.State._
import cats.syntax.applicative._

object PostCalculator {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    for {
      ans <- get[Int]
//    if (sym.forall(_.isDigit)) State.modify[List[Int]](_ :+ sym.toInt)
    } yield ans
}
