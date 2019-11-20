import cats.data.EitherT
import cats.syntax._
import cats.implicits._
import scala.concurrent.Future
import cats.syntax.applicative._
import cats.instances._
import cats.instances.future._


val powerLevels = Map(
  "Jazz"-> 6,
  "Bumblebee" -> 8,
  "Hot Rod"-> 10
)

10.pure[Response]
type Response[A] = EitherT[Future, String, A]

def getPowerLevel(autobot: String): Response[Int] =
  EitherT.fromOption[Future](powerLevels.get(autobot), s"autobot: $autobot lost")

// defined type alias Response

