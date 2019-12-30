import cats.{Applicative, Monoid}
import cats.syntax.monoid._
import cats.instances.future._
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.Foldable
import cats.Traverse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
//a sequence of type Vector[A];
//a function of type A => B, where there is a Monoid for B;

def foldMap[A, B: Monoid](items:Vector[A])(transform: A=>B): B =
  items.map(transform).foldLeft(Monoid[B].empty)(_ |+| _)

import cats.instances.int._ // for Monoid

foldMap(Vector(1, 2, 3))(identity)
// res2: Int = 6

import cats.instances.string._ // for Monoid

// Mapping to a String uses the concatenation monoid:
foldMap(Vector(1, 2, 3))(_.toString + "! ")
// res4: String = "1! 2! 3! "

// Mapping over a String to produce a String:
foldMap("Hello world!".toVector)(_.toString.toUpperCase)
// res6: String = HELLO WORLD!

Runtime.getRuntime.availableProcessors

def parallelFoldMap[A, B : Monoid]
(values: Vector[A])
(func: A => B): Future[B] = {
  val cores = Runtime.getRuntime.availableProcessors
  val divided = values.grouped(cores).toList
  val parallel = divided.map{ part =>
    Future{ foldMap(part)(func) }
  }
  Future.sequence(parallel).map(l=>foldMap[B, B](l.toVector)(identity))
}


// Mapping to a String uses the concatenation monoid:
//val start1 = System.nanoTime
//foldMap((1 to 10000000).toVector)(identity)
//val end1 = System.nanoTime
//val singleTime = (end1 - start1) / 1000000
//
//
//val start = System.nanoTime
//Await.result(parallelFoldMap((1 to 10000000).toVector)(identity), 10.seconds)
//val end = System.nanoTime
//val parallelTime = (end - start) / 1000000


def parallelForldMapCats[A, B : Monoid]
(values: Vector[A])
(func: A => B): Future[B] = {
  val cores = Runtime.getRuntime.availableProcessors
  values.grouped(cores)
    .toVector
    .traverse[Future, B]{ part => Future{ foldMap(part)(func) }}
    .map(_.combineAll)
}

val start = System.nanoTime
Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 2.seconds)
val end = System.nanoTime
val parallelTime = (end - start) / 1000000


// res4: String = "1! 2! 3! "

// Mapping over a String to produce a String:
//Await.result(parallelFoldMap("Hello world!".toVector)(_.toString.toUpperCase), 1.seconds)
// res6: String = HELLO WORLD!
