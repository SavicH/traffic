package ru.vsu.cs.traffic

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.Color.{GREEN, RED, YELLOW}
import ru.vsu.cs.traffic.events.{VehicleSpawnedEvent, ColorChangedEvent, BeforeColorChangedEvent}

class EventTest extends FunSuite {

  val s1 = Point(2, 5)
  val e1 = Point(12, 5)
  val s2 = Point(3, 3) //(4,5)
  val e2 = Point(5, 7)
  val s3 = Point(11, 3) //(9,5)
  val e3 = Point(7, 7)

  val model = TrafficModel(timeStep = 0.5)
  model.addTrafficFlow(s1, e1, 1, probability = 1)
  model.addTrafficFlow(s2, e2, 1, probability = 1)
  model.addTrafficFlow(s3, e3, 1, probability = 1)

  model.trafficLights.foreach(_.durations = Map(RED -> 10.0, GREEN -> 10.0, YELLOW -> 0.0))

  test("Count of ColorChanged events") {
    var count = 0
    model.trafficLightEventHandler = event => if (event.isInstanceOf[ColorChangedEvent]) count+=1
    model.trafficLights.foreach(_.act(15))
    assert(count == model.trafficLights.length)
  }

  test("Count of BeforeColorChanged events") {
    var count = 0
    model.trafficLightEventHandler = event => if (event.isInstanceOf[BeforeColorChangedEvent]) count+=1
    model.trafficLights.foreach(_.act(15))
    assert(count == model.trafficLights.length)
  }

  test("Count of VehicleSpawned events") {
    var count = 0
    model.vehicleEventHandler = event => if (event.isInstanceOf[VehicleSpawnedEvent]) count += 1
    model.trafficFlows(0).act(3)
    model.trafficFlows(0).act(3)
    model.trafficFlows(0).act(3)
    assert(count == model.trafficFlows(0).vehicles.length)
  }


}
