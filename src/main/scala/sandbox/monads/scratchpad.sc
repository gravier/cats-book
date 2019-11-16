import cats.data.State
import sandbox.monads.PostCalculator.{CalcState}

def parser(sym: String) =   sym match {
  case num if (num.forall(_.isDigit)) => println(num)
  case "+"  => println("add")
  case _ => println("not known")
}




def evalOne(sym: String): CalcState[Int] =
  sym match {
    case num if (num.forall(_.isDigit)) =>
      State[List[Int], Int] { stack =>
        val newStack = num.toInt +: stack
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

//evalOne("1").run(Nil).value
//
//val program = for {
//  _ <- evalOne("1")
//  _ <- evalOne("2")
//  ans <- evalOne("+")
//} yield ans
//  program.runA(Nil).value


def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(State.pure[List[Int], Int](0)) { (st, sym) =>
    st.flatMap(_ => evalOne(sym))
  }

//evalAll(List("1", "2", "+")).run(Nil).value

val program2 = for {
  _ <- evalAll(List("1", "2", "+"))
  _ <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans

program2.runA(Nil).value