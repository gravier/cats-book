import cats.{Applicative, Id}

import scala.concurrent.Future

import cats.instances.future._ // for Applicative
import cats.instances.list._   // for Traverse
import cats.syntax.traverse._  // for traverse
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.functor._ // for map


trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_]](client: UptimeClient[F])(implicit app: Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}


trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

//trait TestUptimeClient extends UptimeClient[Id] {
//  def getUptime(hostname: String): Int
//}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}

def testTotalUptime() = {
  val hosts    = Map("host1" -> 10, "host2" -> 6)
  val client   = new TestUptimeClient(hosts)
  val service  = new UptimeService(client)
  val actual   = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  assert(actual == expected)
}

testTotalUptime()