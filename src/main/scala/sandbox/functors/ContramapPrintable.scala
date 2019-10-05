package sandbox.functors

trait Printable[A] {
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        Printable.this.format(func(value))
    }
}

object Printers {

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  implicit def boxPrintable[B](implicit p: Printable[B]): Printable[Box[B]] =
    p.contramap((b: Box[B]) => b.value)
}

final case class Box[A](value: A)
