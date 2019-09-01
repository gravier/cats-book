package sandbox.printable

trait Printable[A] {
  def format(value: A): String
}
object PrintableInstances {
  implicit val printableString: Printable[String] = new Printable[String] {
    def format(value: String): String = value
  }

  implicit val printableInt: Printable[Int] = new Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    def format(cat: Cat): String =
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }
}

object Printable {
  def format[A](value: A)(implicit printer: Printable[A]): String = {
    printer.format(value)
  }

  def print[A](value: A)(implicit printer: Printable[A]): Unit = {
    println(printer.format(value))
  }
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def print(implicit p: Printable[A]) = println(p.format(value))
  }
}

final case class Cat(name: String, age: Int, color: String)
