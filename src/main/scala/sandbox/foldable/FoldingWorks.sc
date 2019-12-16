//Try using foldLeft and foldRight with an empty list as the accumulator and :: as the binary
List(1, 2, 3).foldLeft(List[Int]())((acc, itm)=>itm :: acc)
List(1, 2).foldRight(List[Int]())((itm, acc)=>itm :: acc)

//List's map, flatMap, filter, and sum methods in terms of foldRight.


def map[A, B](list: List[A])(f: A => B): List[B] =
  list.foldRight(List.empty[B])((itm, acc) => f(itm) :: acc)

map(List(1, 2, 3))(_ * 2)

def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
  list.foldRight(List.empty[B])((itm, acc) => f(itm) ::: acc)

flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))

def filter[A](list: List[A])(f: A => Boolean): List[A] =
  list.foldRight(List.empty[A])((itm, acc) => if(f(itm)) itm :: acc else acc)

filter(List(1, 2, 3))(_ % 2 == 1)
//def sum[A](list: List[A]) =
//  list.foldRight(pure[A])(_ + _)
import cats.Monoid

def sumWithMonoid[A](list: List[A])
                    (implicit monoid: Monoid[A]): A =
  list.foldRight(monoid.empty)(monoid.combine)

import cats.instances.int._ // for Monoid
sumWithMonoid(List(1, 2, 3))


