package ru.vsu.cs.traffic.vehicles

import scala.util.Random

import ru.vsu.cs.traffic.{Vehicle, TrafficFlow}

import scala.math._

class VehicleImpl (private var _trafficFlow: TrafficFlow)
  extends MOBILVehicle {

  //todo: test values
//  override val thresholdAcceleration: Double = 0.1
//  override val normalAcceleration: Double = 0.1
//  override val brakeDeceleration: Double = 3
//  override val desiredSpeed: Double = if (random > 0.5) 15 else 7

  private var _distance = 0.0
  private var _speed = 10 * random
  private var _lane = VehicleImpl.getRandomLane(_trafficFlow.lanes)
  private var _acceleration = 0.0

  val length = 5.0

  private var endOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.end, 1000)
  private var startOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.start, -1000)

  def headVehicle(lane: Int = lane): Vehicle = {
    val vehiclesMap = _trafficFlow.vehicles.filter(_.lane == lane)
      .map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ > distance)
    if (distances.isEmpty) endOfFlow else vehiclesMap(distances.min)
  }

  def backVehicle(lane: Int = lane): Vehicle = {
    val vehiclesMap = _trafficFlow.vehicles.filter(_.lane == lane)
      .map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ < distance)
    if (distances.isEmpty) startOfFlow else vehiclesMap(distances.max).asInstanceOf[IDMVehicle]
  }

  override private[traffic] def act(timeStep: Double): Unit = {
    if (_distance > _trafficFlow.length) {
      _trafficFlow -= this
    }
    _lane = mobil.lane
    _acceleration = idm.acceleration
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

