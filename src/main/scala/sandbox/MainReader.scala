package sandbox

import sandbox.monads.Db
import sandbox.monads.LoginReader._

object MainReader extends App {
  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(
    s"dade has good pasword ${checkLogin(1, "zerocool").run(db) == false}")

  println(s"4th user not exists ${checkLogin(4, "davinci").run(db) == false}")
}
