package sandbox
import cats._
import cats.Functor._
import cats.syntax.functor._
import functors._
import functors.CustomFunctor._

object Main extends App {
  val someTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  println(someTree.map(_ * 2))
}
