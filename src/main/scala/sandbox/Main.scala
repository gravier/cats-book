package sandbox

import sandbox.crdt.GCounter

object Main extends App {

  val a = GCounter(Map("A" -> 10))
  val b = GCounter(Map("B" -> 2))

  import cats.instances.int._
  val a2 = a.increment("A", 1)

  import sandbox.crdt.BoundedSemiLattice._
  val ab = a.merge(b)
  println(ab)
  println(a2)
  val ab2 = ab.merge(a2)
  println(func.total(ab2))
}

object func {
  def total(c: GCounter[Int]) = {
    import cats.instances.int._
    c.total
  }
}
