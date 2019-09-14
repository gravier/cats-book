package sandbox.printable

import cats.Show

object ShowInstances {
  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
}

object ShowSyntax {

  implicit class ShowOps[A](value: A) {
    def print(implicit p: Show[A]) = println(p.show(value))
  }
}
