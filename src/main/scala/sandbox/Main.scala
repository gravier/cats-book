package sandbox

import sandbox.check.{Check, CheckF, Pure}
import cats.Semigroup
import cats.instances.list._
import cats.syntax.either._
// for Semigroup
import cats.syntax.semigroup._ // for |+|

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
  val a2: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b2: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check2: Check[List[String], Int] =
    a2 and b2

  println(check2(5))
  println(check2(0))
  println(check2(-3))
//  val a1: CheckF[Nothing, Int] =
//    CheckF(v => v.asRight)
//
//  val b2: CheckF[Nothing, Int] =
//    CheckF(v => v.asRight)
//
//  val check2 = a1 and b2

}
