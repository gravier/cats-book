package sandbox.monads
import cats.data.State
import cats.data.State._
import cats.syntax.applicative._
import sandbox.monads.PostCalculator.CalcState

object PostCalculator {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case num if (num.forall(_.isDigit)) =>
        State[List[Int], Int] { stack =>
          val newStack = stack :+ num.toInt
          val res = num.toInt
          (newStack, res)
        }
      case "+" =>
        State[List[Int], Int] { stack =>
          val newStack = stack match {
            case (a :: b :: tail) => (a + b) +: tail
            case _ =>
              throw new Exception(s"cannot perform op: + on state: $stack")
          }
          val res = newStack.head
          (newStack, res)
        }
      case "*" =>
        State[List[Int], Int] { stack =>
          val newStack = stack match {
            case (a :: b :: tail) => (a * b) +: tail
            case _ =>
              throw new Exception(s"cannot perform op: * on state: $stack")
          }
          val res = newStack.head
          (newStack, res)
        }
      case _ => throw new Exception(s"cannot parse value")
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0)) { (st, sym) =>
      st.flatMap(_ => evalOne(sym))
    }
}
