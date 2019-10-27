package sandbox

import cats.Eval
import sandbox.monads.EvalFold
//import functors.Printers.booleanPrintable._
//import functors.Printers.stringPrintable._
//import functors.Printers.boxPrintable._

object Main extends App {
  val res = EvalFold.foldRight((1 to 4000).toList, 0) { _ + _ }
  println(res)
}
