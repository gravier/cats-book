package sandbox

import cats.Eval
import sandbox.monads.EvalFold
import monads.WriteLogMonad._
//import functors.Printers.booleanPrintable._
//import functors.Printers.stringPrintable._
//import functors.Printers.boxPrintable._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
  val res = EvalFold.foldRight((1 to 4000).toList, 0) { _ + _ }
  println(res)

//  factorial(5)

  val Vector((logA, ansA), (logB, ansB)) =
    Await.result(Future.sequence(
                   Vector(
                     Future(factorial2(5).run),
                     Future(factorial2(5).run)
                   )),
                 5.seconds)

  println(s"logA: $logA lobB: $logB ansA: $ansA ansB: $ansB")
}
