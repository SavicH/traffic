package ru.vsu.cs.traffic.driver

import ru.vsu.cs.traffic.{Point, TrafficModel}

object ExampleDriver extends Driver {

  val model = {
    val model = TrafficModel()
    val prob = 0.4
    val lanesCount = 2
    model.addFlow(Point(200, 0), Point(200, 400), lanesCount, isOneWay = false, probability = _ => prob)
    model.addFlow(Point(500, 0), Point(500, 400), lanesCount, isOneWay = false, probability = _ => prob)
    model.addFlow(Point(0, 200), Point(750, 200), lanesCount, isOneWay = false, probability = _ => prob)
    model.run()
    model
  }
}
