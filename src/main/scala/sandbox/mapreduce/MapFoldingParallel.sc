import cats.Monoid
import cats.syntax.monoid._
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