package sandbox.equality
import cats._
import cats.instances.string._
import cats.instances.int._
import cats.syntax.eq._

import sandbox.printable.Cat

object CatEquality {
  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
  }

}
