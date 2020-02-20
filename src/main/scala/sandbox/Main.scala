package sandbox

import sandbox.crdt.{GCounter, GCounter2, KeyValueStore}

object Main extends App {

  val a = GCounter(Map("A" -> 10))
  val b = GCounter(Map("B" -> 2))

  import cats.instances.int._
  val a2 = a.increment("A", 1)

  val ab = a.merge(b)
  println(ab)
  println(a2)
  println(ab.merge(a2).total)

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

//  val counter = GCounter2[Map, String, Int]

//  val merged = counter.merge(g1, g2)
//  // merged: Map[String,Int] = Map(a -> 7, b -> 5)
//
//  val total = counter.total(merged)
  // total: Int = 12

//  implicit val store = KeyValueStore[Map, String, Int]
  val counter2 = GCounter2[KeyValueStore[Map], String, Int]
}
