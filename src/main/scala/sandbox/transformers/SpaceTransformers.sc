import cats.syntax.applicative._
import cats.instances.future._
import cats.syntax.flatMap._

import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.EitherT

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

val powerLevels = Map(
  "Jazz"-> 6,
  "Bumblebee" -> 8,
  "Hot Rod"-> 10
)

type Response[A] = EitherT[Future, String, A]

def getPowerLevel(autobot: String): Response[Int] =
  powerLevels.get(autobot) match {
    case Some(level) =>  EitherT.right(Future(level))
    case None =>  EitherT.left(Future("msg uncreachable"))
  }

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] ={
  getPowerLevel(ally1).flatMap{ a1 =>
    getPowerLevel(ally2).map{ a2 =>
      (a1 + a2) > 15
    }
  }
}

def tacticalReport(ally1: String, ally2: String): String = {
  Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
    case Right(true) => "can"
    case _ => "cannot"
  }
}

tacticalReport("Jazz", "Bumblebee")

tacticalReport("Bumblebee", "Hot Rod")

tacticalReport("Jazz", "Ironhide")

