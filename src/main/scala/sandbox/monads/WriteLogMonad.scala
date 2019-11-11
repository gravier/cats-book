package sandbox.monads
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.data.Writer

object WriteLogMonad {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val res = factorialWithWriter(n.pure[Logged])
    val (log, ans) = res.run
    log.foreach(println)
    ans
  }

  def factorialWithWriter(n: Logged[Int]): Logged[Int] = {
    val ans = slowly(
      if (n.value == 0) 1.writer(Vector(s"fact $n 0"))
      else n.map(_ * factorialWithWriter(n.map(_ - 1)).value))
    Vector(s"fact $n $ans").tell
    ans
  }

  def factorial2(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) { 1.pure[Logged] } else {
        slowly(factorial2(n - 1).map(n * _))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}
