package ru.vsu.cs.traffic

import org.scalatest.{BeforeAndAfter, FunSuite}

class TrafficModelTest extends FunSuite with BeforeAndAfter {

  var model: TrafficModel = _
  val start = Point(1, 1)
  val end = Point(2, 2)
  val lanes = 1

  before {
    model = TrafficModel(0.005)
  }

  after {
    model.stop()
  }

  test("Add one way traffic flow") {
    assert(model.addFlow(start, end, lanes, isOneWay = true).
      trafficFlows.length === 1)
  }

  test("Add two ways traffic flow") {
    assert(model.addFlow(start, end, lanes, isOneWay = false).
      trafficFlows.length === 2)
  }

  test("Run in real time") {
    model.run()
    model.stop()
    assert(!model.isRunning)
  }

  test("Run in real time twice") {
    intercept[IllegalStateException] {
      model.run()
      model.run()
    }
  }

  test("Run not in real time") {
    model.run(10)
    model.stop()
    assert(!model.isRunning)
  }

  ignore("Run not in real time twice") {
    //run() method isn't blocking
    model.run(10)
    model.run(10)
    assert(!model.isRunning)
  }
}
