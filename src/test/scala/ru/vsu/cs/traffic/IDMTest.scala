package ru.vsu.cs.traffic

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.vehicles.{VirtualVehicle, VehicleImpl}

class IDMTest extends FunSuite {

  val model = TrafficModel()
  model.addTrafficFlow(Point(0, 0), Point(0, 500), 2, isOneWay = true)
  val flow = model.trafficFlows(0)

  val v1 = new VehicleImpl(flow) {
    override def lane = 1
    override def distance = 10
    override def speed = 15
  }

  val v2 = new VehicleImpl(flow) {
    override def lane = 1
    override def distance = 20
    override def speed = 10
  }

  val v3 = new VehicleImpl(flow) {
    override def lane = 1
    override def distance = 30
    override def speed = 1
  }

  val v4 = new VehicleImpl(flow) {
    override def lane = 2
    override def distance = 5
    override def speed = 10
  }

  val v5 = new VehicleImpl(flow) {
    override def lane = 2
    override def distance = 25
    override def speed = 10
  }

  flow += v1
  flow += v2
  flow += v3
  flow += v4
  flow += v5

  test("Head vehicle 1") {
    assert(v1.headVehicle(1) === v2)
  }

  test("Head vehicle 2") {
    assert(v1.headVehicle(2) === v5)
  }

  test("Back vehicle 1") {
    assert(v1.backVehicle(1).isInstanceOf[VirtualVehicle])
  }

  test("Back vehicle 2") {
    assert(v1.backVehicle(2) === v4)
  }

  test("Deceleration") {
    assert(v1.idm.acceleration < 0)
  }

  test("Acceleration") {
    assert(v3.idm.acceleration > 0)
  }

}
