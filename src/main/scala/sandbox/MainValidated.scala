package sandbox

import sandbox.monads.EvalFold
import sandbox.monads.WriteLogMonad._
//import functors.Printers.booleanPrintable._
//import functors.Printers.stringPrintable._
//import functors.Printers.boxPrintable._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object MainValidated extends App {
  import cats.data.Validated
  import cats.syntax.either._
  import cats.syntax.validated._
  import cats.syntax.apply._
  import cats.instances.list._
  import cats.instances.string._

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  case class User(name: String, age: Int)

  def getValue(field: String)(fieldVals: FormData): FailFast[String] = {
    //  fieldVals.get(field) match {
    //    case Some(fieldValue) => Right(fieldValue)
    //    case None => Left(s"field $field does not exist")
    //  }
    fieldVals.get(field).toRight(List(s"field $field does not exist"))
  }

  val getName = getValue("name") _
  getName(Map("name" -> "john"))
  getName(Map())

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either
      .catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List(s"$name must be valid int"))

  parseInt("count")("1")
  parseInt("count")("a")

  def nonBlank(name: String)(data: String): FailFast[String] =
    if (data.size > 0)
      Right(data)
    else Left(List(s"$name must not be empty"))

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    if (data > -1)
      Right(data)
    else Left(List(s"$name cannot be negative"))

  nonBlank("name")("jonas")
  nonBlank("name")("")

  nonNegative("age")(10)
  nonNegative("age")(-1)

  def readName(fieldVals: FormData) =
    getValue("name")(fieldVals)
      .flatMap(nonBlank("name"))

  def readAge(fieldVals: FormData) =
    getValue("age")(fieldVals)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))

  readName(Map())
  readName(Map("name" -> ""))
  readName(Map("name" -> "jonas"))

  readAge(Map())
  readAge(Map("age" -> ""))
  readAge(Map("age" -> "-1"))
  readAge(Map("age" -> "1"))

  ("Badness".invalid[Int], "Badness".invalid[Int]).tupled

  def readUser(fieldVals: FormData): FailSlow[User] =
    (readName(fieldVals).toValidated, readAge(fieldVals).toValidated)
      .mapN(User.apply)

  println(readUser(Map()))
  println(readUser(Map("name" -> "", "age" -> "")))
  println(readUser(Map("name" -> "jonas", "age" -> "")))
  println(readUser(Map("name" -> "jonas", "age" -> "1")))
  println(readUser(Map("name" -> "", "age" -> "-1")))
  println(readUser(Map("name" -> "", "age" -> "1")))

}
