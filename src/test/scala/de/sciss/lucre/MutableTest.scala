package de.sciss.lucre

import scala.collection.mutable

trait MutableTest {
  def m: mutable.Map[String, Int]
  def s: mutable.Set[String]

  m.put("foo", 22)
  m.getOrElse("foo", 44)
  m.getOrElseUpdate("foo", 55)

  m.+=("foo" -> 22)

  s.add("foo")
  s += "bar"

  val ms: mutable.SortedMap[String, Int]

  ms.firstKey
  ms.lastKey

  val ss: mutable.SortedSet[String]
  ss.firstKey
  ss.lastKey

  val mq: mutable.Queue[String]
  mq.dequeue()

  val pq: mutable.PriorityQueue[String]

  pq.dequeue()
}
