package ru.vsu.cs.traffic

import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.vsu.cs.traffic.Color.{GREEN, RED, YELLOW}
import ru.vsu.cs.traffic.Direction.{BACK, FORWARD, LEFT, RIGHT}
import ru.vsu.cs.traffic.event._
import ru.vsu.cs.traffic.vehicle.VehicleImpl

class EventTest extends FunSuite with BeforeAndAfter {

  val s1 = Point(200, 500)
  val e1 = Point(1200, 500)
  val s2 = Point(300, 300) //(4,5)
  val e2 = Point(500, 700)
  val s3 = Point(1100, 300) //(9,5)
  val e3 = Point(700, 700)

  var model: TrafficModel = _

  before {
    model = TrafficModel(timeStep = 0.5)
    model.addFlow(s1, e1, 2, probability = _ => 1)
    model.addFlow(s2, e2, 2, probability = _ => 1)
    model.addFlow(s3, e3, 2, probability = _ => 1)
    model.trafficLights.foreach(_.durations = Map(RED -> 10.0, GREEN -> 10.0, YELLOW -> 0.0))
  }

  test("Count of ColorChanged events") {
    var count = 0
    model.trafficLightEventHandler = event => if (event.isInstanceOf[ColorChanged]) count+=1
    model.trafficLights.foreach(_.act(15))
    assert(count == model.trafficLights.length)
  }

  test("Count of BeforeColorChanged events") {
    var count = 0
    model.trafficLightEventHandler = event => if (event.isInstanceOf[BeforeColorChanged]) count+=1
    model.trafficLights.foreach(_.act(15))
    assert(count == model.trafficLights.length)
  }

  test("Count of VehicleSpawned events") {
    var count = 0
    model.vehicleEventHandler = event => if (event.isInstanceOf[VehicleSpawned]) count += 1
    model.trafficFlows(0).act(3)
    assert(count == model.trafficFlows(0).vehicles.length)
  }

  test("LaneChanged event") {
    var isTriggered = false
    val f2 = model.trafficFlows(1)
    model.vehicleEventHandler = event => if (event.isInstanceOf[LaneChanged]) isTriggered = true
    val v1 = new VehicleImpl(f2, model) {
      override val speed = 10.0
      override val lane = 1
      override val distance = 0.0
    }
    val v2 = new VehicleImpl(f2, model) {
      override val speed = 5.0
      override val lane = 1
      override val distance = 10.0
    }
    f2 += v1
    f2 += v2
    v1.act(2)
    assert(isTriggered)
  }

  test("IntersectionPassed event") {
    var isTriggered = false
    model.vehicleEventHandler = event => if (event.isInstanceOf[IntersectionPassed]) isTriggered = true
    val flow = model.trafficFlows(0)
    val dist = model.intersections(0)(flow).distance + 1
    model.trafficLights.foreach(_.turnProbabilities = Map(FORWARD -> 1, BACK -> 0, LEFT -> 0, RIGHT -> 0))
    val v1 = new VehicleImpl(flow, model) {
      override val speed = 10.0
      override val distance = dist
    }
    v1.act(1)
    assert(isTriggered)
  }

  test("ModelActed event") {
    var isTriggered = false
    model.trafficModelEventHandler = () => isTriggered = true
    model.run(1)
    assert(isTriggered)
  }


}
