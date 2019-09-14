package sandbox

import cats.syntax.eq._
import cats.instances.option._
import sandbox.equality.CatEquality._
import sandbox.printable.{Cat}

object Main extends App {
//  println(Printable.format("alio"))
//  Show.print("alio 2")
//  "alio 2".print

//  catShow.show(Cat("johnny", 3, "black"))
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  println(cat1 === cat1)
  println(cat1 === cat2)

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 === optionCat1)
  println(optionCat1 === optionCat2)

}
