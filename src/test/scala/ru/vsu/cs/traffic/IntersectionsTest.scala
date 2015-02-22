package ru.vsu.cs.traffic

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.Direction._

class IntersectionsTest extends FunSuite {

  val s1 = Point(2, 5)
  val e1 = Point(12, 5)
  val s2 = Point(3, 3) //(4,5)
  val e2 = Point(5, 7)
  val s3 = Point(11, 3) //(9,5)
  val e3 = Point(7, 7)
  val model = TrafficModel()
  model.addFlow(s1, e1, 1)
  model.addFlow(s2, e2, 1)
  model.addFlow(s3, e3, 1, isOneWay = true)
  
  val f1 = findFlow(s1)
  val f2 = findFlow(s2)
  val f3 = findFlow(s3)
  
  val i1 = findIntersection(Point(4,5))
  val i2 = findIntersection(Point(9,5))
  
  def findFlow(start: Point) = {
    model.trafficFlows.find(_.start == start).get  
  }
  
  def findIntersection(location: Point) = {
    model.intersections.find(_.location == location).get
  }

  test("The intersections count") {
    assert(model.intersections.length === 2)
  }

  test("The traffic lights count") {
    assert(model.trafficLights.length === 7)
  }

  test("One traffic light without opposite") {
    assert(model.trafficLights.count(_.opposite == null) == 1)
  }

  test("Intersections on a traffic flow") {
    assert(f1.intersections.length === 2)
  }
  
  test("A traffic light on the first intersection 1") {
    assert(i1(f1)(BACK) == f1.neighbour)
  }

  test("A traffic light on the first intersection 2") {
    assert(i1(f1)(LEFT) == f2)
  }

  test("A traffic light on the first intersection 3") {
    assert(i1(f1)(RIGHT) == f2.neighbour)
  }

  test("A traffic light on the second intersection 1") {
    assert(i2(f1)(BACK) == f1.neighbour)
  }

  test("A traffic light on the second intersection 2") {
    assert(i2(f1)(LEFT) == f3)
  }

  test("A traffic light on the second intersection 3") {
    assert(i2(f1)(RIGHT) == null)
  }

  test("Next intersection 1") {
    assert(i1.next(f1) === i2)
  }

  test("Next intersection 2") {
    assert(i2.next(f1) === null)
  }

  test("Next intersection 3") {
    assert(i2.next(f1.neighbour) === i1)
  }

  test("Next intersection 4") {
    assert(i1.next(f1.neighbour) === null)
  }

}
