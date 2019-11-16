import cats.data.State
import sandbox.monads.PostCalculator.CalcState

def parser(sym: String) =   sym match {
  case num if (num.forall(_.isDigit)) => println(num)
  case "+"  => println("add")
  case _ => println("not known")
}




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

evalOne("1").run(Nil).value