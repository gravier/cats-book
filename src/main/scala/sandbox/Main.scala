package sandbox

import sandbox.crdt.GCounter

object Main extends App {

  val a = GCounter(Map("A" -> 10))
  val b = GCounter(Map("B" -> 2))

  import cats.instances.int._
  val a2 = a.increment("A", 1)

  val ab = a.merge(b)
  println(ab)
  println(a2)
  println(ab.merge(a2).total)
}
