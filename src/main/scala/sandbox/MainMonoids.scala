package sandbox

import sandbox.monoids._
import cats.instances.boolean._
import cats.instances.set._
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.option._
import sandbox.monoids.CatsMonoid.orderMonoid

object MainMonoids extends App {
//  println(Printable.format("alio"))
//  Show.print("alio 2")
//  "alio 2".print

//  catShow.show(Cat("johnny", 3, "black"))

  println("or combine")
  println(OrBoolean.combine(true, false) === OrBoolean.combine(false, true))
  println(OrBoolean.combine(true, false) === OrBoolean.combine(false, true))

  println("or empty - id")
  println(OrBoolean.combine(true, OrBoolean.empty) === true)
  println(OrBoolean.combine(false, OrBoolean.empty) === false)

  println("and combine")
  println(AndBoolean.combine(true, false) === AndBoolean.combine(false, true))

  println("and empty - id")
  println(AndBoolean.combine(true, AndBoolean.empty) === true)
  println(AndBoolean.combine(false, AndBoolean.empty) === false)

  println("concat set")
  println(
    ConcatSet.combine(Set(1), Set(2)) === ConcatSet.combine(Set(2), Set(1)))
  println(ConcatSet.combine(Set(1), ConcatSet.empty) === Set(1))

  println("<<<<< cats monoid >>>>>>>>>>")

  println(s"super add 4!=10  ${CatsMonoid.add(List(1, 2, 3, 4)) == 10}")
  println(s"super option add 3!=6  ${CatsMonoid.add(
    List(Some(1), Some(2), None, Some(3))) == Some(6)}")

  println(s"super add orders 1,2+3,4!=4,6  ${CatsMonoid.add(
    List(Order(1, 2), Order(3, 4))) == Order(4, 6)}")

}
