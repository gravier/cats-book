package sandbox

import sandbox.monoids._
import cats.instances.boolean._
import cats.instances.set._
import cats.syntax.eq._

object Main extends App {
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

}
