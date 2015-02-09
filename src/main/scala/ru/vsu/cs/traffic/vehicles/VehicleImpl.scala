package ru.vsu.cs.traffic.vehicles

import scala.util.Random

import ru.vsu.cs.traffic.{Vehicle, TrafficFlow}

import scala.math._

class VehicleImpl (private var _trafficFlow: TrafficFlow)
  extends Vehicle {

  private class IDM {
    private val a = normalAcceleration
    private val b = brakeDeceleration
    private val v0 = desiredSpeed
    private val delta = accelerationExponent
    private val T = timeHeadway
    private val s0 = minimumGap

    private def v = speed

    private def dynamicGap(head: Vehicle) = {
      val dv = speed - head.speed
      s0 + max(0, v * T + v * dv / (2 * sqrt(a * b)))
    }

    def acceleration(head: Vehicle) = {
      val ss = dynamicGap(head)
      val s = head.distance - distance - head.length
      a * (1 - pow(v/v0, delta) - pow(ss/s, 2))
    }
  }

  override val normalAcceleration: Double = 0.3 //todo: low for test purpose (normal 1-2)
  override val brakeDeceleration: Double = 3 //todo: high for test purpose (normal 1-2)
  //todo: proper initialization
  private var _distance = 0.0
  private var _speed = 10 * random
  private var _lane = VehicleImpl.getRandomLane(_trafficFlow.lanes)
  private var _acceleration = 0.0

  val length = 5.0

  private val idm = new IDM

  private var endOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.end, 100)

  private def headVehicle = {
    val vehiclesMap = _trafficFlow.vehicles.filter(_.lane == _lane).map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ > _distance)
    if (distances.isEmpty) endOfFlow else vehiclesMap(distances.min)
  }

  override private[traffic] def act(timeStep: Double): Unit = {
    if (_distance > _trafficFlow.length) {
      //_trafficFlow -= TypedActor.self
      //TypedActor.context.stop(TypedActor.self)
      _trafficFlow -= this
    }
    _acceleration = idm.acceleration(headVehicle)
    _distance += _speed * timeStep + 0.5 * _acceleration * pow(timeStep, 2)
    _speed += _acceleration * timeStep
  }

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def speed: Double = _speed

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

}

object VehicleImpl {
  private val random = new Random(System.nanoTime())

  private def getRandomLane(lanes: Int) = random.nextInt(lanes) + 1
}

