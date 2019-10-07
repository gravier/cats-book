package sandbox
import cats._
import cats.Functor._
import cats.syntax.functor._
import functors._
import functors.CustomFunctor._
import functors.Printers._
import functors.Codecs._
//import functors.Printers.booleanPrintable._
//import functors.Printers.stringPrintable._
//import functors.Printers.boxPrintable._

object Main extends App {
  val someTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  println(someTree.map(_ * 2))

  val p: Printable[Int] = new Printable[Int] {
    def format(value: Int): String = value.toString
  }
  println("vals equal")
  println("1" == p.contramap((param: Float) => param.toInt).format(1.2f))

  format("hello")
  format(true)
  println("whats in box " + format(Box("hello box")))
  println("whats in box 2 " + format(Box(false)))
//  println("whats in box 2 " + format(Box(1)))

  println(
    s"encoding to string works - ${encode(123.4)} ${encode(123.4) == "123.4"}")
  println(
    s"decoding to double works - ${decode[Double]("123.4")} ${decode[Double]("123.4") == 123.4}")

  // res2: String = 123.4
  decode[Box[Double]]("123.4")
  // res3: Box[Double] = Box(123.4)
  println(
    s"box decoding works - ${encode(Box(123.4))} ${encode(Box(123.4)) == "123.4"}")

  println(s"box decoding works - ${decode[Box[Double]]("123.4")} ${decode[
    Box[Double]]("123.4") == Box(123.4)}")
}
