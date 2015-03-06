package ru.vsu.cs.traffic.driver

import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction._
import ru.vsu.cs.traffic.{Point, TrafficModel}

object ExampleDriver extends Driver {

  val model = TrafficModel()

  val prob = 0.4
  val lanesCount = 2
  model.addFlow(Point(200, 0), Point(200, 400), lanesCount, isOneWay = false, probability = _ => prob)
  model.addFlow(Point(500, 0), Point(500, 400), lanesCount, isOneWay = false, probability = _ => prob)
  model.addFlow(Point(0, 200), Point(750, 200), lanesCount, isOneWay = false, probability = _ => prob)
  model.trafficLights.foreach(_.durations = Map(RED -> 15, GREEN -> 15, YELLOW -> 0))
  model.trafficLights.foreach(_.turnProbabilities = Map(FORWARD -> 0.7, RIGHT -> 0.3, LEFT -> 0, BACK -> 0))

  model.run()

  val panel = new TrafficModelPanel(model)

}
