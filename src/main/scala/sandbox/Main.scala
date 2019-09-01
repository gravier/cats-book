package sandbox

//import cats.instances.string._
//import cats.syntax.semigroup._
import sandbox.printable.Cat
import sandbox.printable.PrintableInstances._
import sandbox.printable.PrintableSyntax._

object Main extends App {
//  println(Printable.format("alio"))
//  Printable.print("alio 2")
  "alio 2".print

  Cat("johnny", 3, "black").print
}
