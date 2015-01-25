package ru.vsu.cs.traffic.traffic

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.{Point, TrafficModel}

class IntersectionsTest extends FunSuite {

  val s1 = Point(2, 5)
  val e1 = Point(12, 5)
  val s2 = Point(3, 3) //(4,5)
  val e2 = Point(5, 7)
  val s3 = Point(11, 3) //(9,5)
  val e3 = Point(7, 7)
  val model = TrafficModel()
  model.addTrafficFlow(s1, e1, 1)
  model.addTrafficFlow(s2, e2, 1)
  model.addTrafficFlow(s3, e3, 1, isOneWay = true)

  test("Intersection count") {
    assert(model.intersections.length === 2)
  }

  test("Traffic lights count") {
    assert(model.trafficLights.length === 7)
  }

  test("One traffic light without opposite") {
    assert(model.trafficLights.count(_.opposite == null) == 1)
  }

  test("Intersections on a traffic flow") {
    assert(model.trafficFlows.find(_.start == Point(2, 5)).get.intersections.length === 2)
  }



}
