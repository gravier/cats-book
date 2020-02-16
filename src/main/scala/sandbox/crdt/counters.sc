final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int) =
    GCounter(counters ++ Map( machine -> (counters.getOrElse(machine, 0) + amount)))

  def merge(that: GCounter): GCounter =
    GCounter(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v max that.counters.getOrElse(k, 0))
    })

  def total: Int =
    counters.values.sum
}

val a = GCounter(Map("A" -> 10))
val b = GCounter(Map("B" -> 2))
val ab = a.merge(b)
val a2 = a.increment("A", 1)
ab.merge(a2).total
