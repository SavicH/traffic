package ru.vsu.cs.traffic.gui

import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction._
import ru.vsu.cs.traffic.{Point, TrafficModel}

object ExampleApp extends SwingApp {

  val model = TrafficModel(0.1, true)

  val prob = 0.4
  val lanesCount = 3
  model.addFlow(Point(200, 0), Point(200, 400), lanesCount, isOneWay = false, probability = _ => prob, secondProbability = _ => 0.8)
  model.addFlow(Point(500, 0), Point(500, 400), lanesCount, isOneWay = false, probability = _ => prob)
  model.addFlow(Point(0, 200), Point(750, 200), lanesCount, isOneWay = false, probability = _ => prob)
  model.trafficLights.foreach(_.durations = Map(RED -> 15, GREEN -> 15, YELLOW -> 0))
  model.trafficLights.foreach(_.turnProbabilities = Map(FORWARD -> 0.75, RIGHT -> 0.15, LEFT -> 0.08, BACK -> 0.02))

  model.run()

  val panel = new TrafficModelPanel(model)

}
