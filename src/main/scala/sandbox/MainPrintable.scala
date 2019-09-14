package sandbox

//import cats.syntax.show._
import sandbox.printable.Cat
import sandbox.printable.ShowInstances._
import sandbox.printable.ShowSyntax._

////import cats.syntax.semigroup._
//import sandbox.printable.PrintableInstances._
//import sandbox.printable.PrintableSyntax._

object MainPrintable extends App {
//  println(Printable.format("alio"))
//  Show.print("alio 2")
//  "alio 2".print

//  catShow.show(Cat("johnny", 3, "black"))
  Cat("johnny", 3, "black").print
}
