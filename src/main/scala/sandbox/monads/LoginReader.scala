package sandbox.monads
import cats.data.Reader
import cats.syntax.applicative._

case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

object LoginReader {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      maybeUser <- findUsername(userId)
      isValid <- maybeUser
        .map(checkPassword(_, password))
        .getOrElse(false.pure[DbReader])
    } yield isValid
}
