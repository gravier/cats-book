package sandbox

import sandbox.check.{Check, CheckF, Predicate, Pure}
import cats.Semigroup
import cats.instances.list._
import cats.syntax.either._
// for Semigroup
import cats.syntax.semigroup._ // for |+|
import cats.syntax.validated._ // for |+|

object Main extends App {
  import cats.instances.list._ // for Semigroup
  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: CheckF[List[String], Int] =
    a and b

  println(check(5))
  println(check(0))
  println(check(-3))

  import cats.instances.list._ // for Semigroup
  val a2: Predicate[List[String], Int] =
    Pure { v =>
      if (v > 2) v.valid
      else List("Must be > 2").invalid
    }
  val b2: Predicate[List[String], Int] =
    Pure { v =>
      if (v < -2) v.valid
      else List("Must be < -2").invalid
    }

  val check2: Predicate[List[String], Int] =
    a2 and b2

  val check3: Predicate[List[String], Int] =
    a2 or b2

  println(check2(5))
  println(check2(0))
  println(check2(-3))

  println(check3(5))
  println(check3(0))
  println(check3(-3))

//  val a1: CheckF[Nothing, Int] =
//    CheckF(v => v.asRight)
//
//  val b2: CheckF[Nothing, Int] =
//    CheckF(v => v.asRight)
//
//  val check2 = a1 and b2

}
