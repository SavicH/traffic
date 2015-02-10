package ru.vsu.cs.traffic

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.vehicles.{VehicleImpl, IDMVehicle}

class MOBILTest extends FunSuite{

  val model = TrafficModel()
  model.addTrafficFlow(Point(200, 450), Point(600, 450), 2, isOneWay = true)
  val flow = model.trafficFlows(0)
  val v1 = new VehicleImpl(flow) {
    override def lane = 1
    override def distance = 55
    override def speed = 10
  }
  val v2 = new VehicleImpl(flow) {
    override def lane = 1
    override def distance = 45
    override def speed = 10
  }
  val v3 = new VehicleImpl(flow) {
    override def lane = 2
    override def distance = 44
    override def speed = 10
  }
  val v4 = new VehicleImpl(flow) {
    override def lane = 2
    override def distance = 47
    override def speed = 10
  }

  flow += v1
  flow += v2

  test("Changing a lane") {
    assert(v2.mobil.lane === 2)
  }

  test("Not changing a lane (a vehicle is back)") {
    flow += v3
    val lane = v2.mobil.lane
    flow -= v3
    assert(lane === 1)
  }

  test("Not changing a lane (a vehicle is ahead)") {
    flow += v4
    val lane = v2.mobil.lane
    flow -= v4
    assert(lane === 1)
  }


}
