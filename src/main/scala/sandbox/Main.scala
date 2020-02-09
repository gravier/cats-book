package sandbox

import sandbox.check.{Check, CheckF, Predicate}
import sandbox.check.Predicate._
import cats.Semigroup
import cats.data.Kleisli
import cats.instances.list._
import cats.syntax.either._
import sandbox.check.Check
// for Semigroup
import cats.syntax.semigroup._ // for |+|
import cats.syntax.validated._ // for |+|
import cats.data.{NonEmptyList, Validated}
import cats.syntax.apply._ // for mapN
import cats.syntax.validated._ // for valid and invalid
import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.instances.either._ // for Semigroupal
import cats.instances.list._ // for Monad

object Main extends App {
  import cats.instances.list._ // for Semigroup
  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: CheckF[List[String], Int] =
    a and b

  println(check(5))
  println(check(0))
  println(check(-3))

  import cats.instances.list._ // for Semigroup
  val a2: Predicate[List[String], Int] =
    Pure { v =>
      if (v > 2) v.valid
      else List("Must be > 2").invalid
    }
  val b2: Predicate[List[String], Int] =
    Pure { v =>
      if (v < -2) v.valid
      else List("Must be < -2").invalid
    }

  val check2: Predicate[List[String], Int] =
    a2 and b2

  val check3: Predicate[List[String], Int] =
    a2 or b2

  println(check2(5))
  println(check2(0))
  println(check2(-3))

  println(check3(5))
  println(check3(0))
  println(check3(-3))

  import cats.data.{NonEmptyList, Validated}

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"),
                   str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error(s"Must be all alphanumeric characters"),
                   str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"),
                   str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char only once"),
                   str => str.filter(c => c == char).size == 1)

  def isUserNameValid: Check[Errors, String, String] =
    Check(longerThan(4)) andThen Check(alphanumeric)

  def splitEmail: Check[Errors, String, (String, String)] = Check(
    _.split('@') match {
      case Array(name, domain) => (name, domain).validNel
      case _                   => "does not contain valid @".invalidNel
    }
  )

  def leftCheck = Check(longerThan(1))
  def rightCheck = Check(longerThan(3) and contains('.'))

  def joinEmail: Check[Errors, (String, String), String] = Check {
    case (l, r) => (leftCheck(l), rightCheck(r)).mapN(_ + "@" + _)
  }

  def isEmailAddressValid = splitEmail andThen joinEmail

  println(isUserNameValid("4lt"))
  println(isUserNameValid("5ltrs"))
  println(isUserNameValid("5lrs%"))

  println(isEmailAddressValid("abc@aaa.lt"))
  println(isEmailAddressValid("@aaa.lt"))
  println(isEmailAddressValid("abc@a.a"))
  println(isEmailAddressValid("abc@aaalt"))
  println(isEmailAddressValid("@abc@aaalt"))

  type Result[A] = Either[Errors, A]

  type CheckK[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): CheckK[A, B] =
    Kleisli(func)

  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): CheckK[A, A] =
    Kleisli[Result, A, A](pred.run)

  def isUserNameValid2: CheckK[String, String] =
    checkPred(longerThan(4)) andThen checkPred(alphanumeric)

  println(isUserNameValid2("4lt"))
  println(isUserNameValid2("5ltrs"))
  println(isUserNameValid2("5ltrs%"))

  final case class User(username: String, email: String)

  def createUser1(username: String, email: String): Validated[Errors, User] =
    (
      isUserNameValid(username),
      isUserNameValid(email)
    ).mapN(User)

  def createUser2(username: String, email: String): Either[Errors, User] =
    (
      isUserNameValid2.run(username),
      isUserNameValid2.run(email)
    ).mapN(User)

  println("usr 1 " + createUser1("Naa", "noaa%"))
  println("usr 2 " + createUser2("Nae", "noaa@"))

}
