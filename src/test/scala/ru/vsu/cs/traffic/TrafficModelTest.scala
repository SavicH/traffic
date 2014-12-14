package ru.vsu.cs.traffic

import org.scalatest.FunSuite

class TrafficModelTest extends FunSuite {

  trait TestModel {
    val model: TrafficModel = TrafficModel()
    val start = Point(1, 1)
    val end = Point(2, 2)
    val prob = 0.5
    val lanes = 1
  }

  test("Add one way traffic flow") {
    new TestModel {
      assert(model.addTrafficFlow(start, end, lanes, prob, isOneWay = true).
        trafficFlows.length === 1)
    }
  }

  test("Add two ways traffic flow") {
    new TestModel {
      assert(model.addTrafficFlow(start, end, lanes, prob, isOneWay = false).
        trafficFlows.length === 2)
    }
  }
}
