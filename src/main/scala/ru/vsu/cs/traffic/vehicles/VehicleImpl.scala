package ru.vsu.cs.traffic.vehicles

import scala.util.Random

import ru.vsu.cs.traffic.{Intersection, Direction, Vehicle, TrafficFlow}
import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction._
import math._

import scala.math._

class VehicleImpl(private var _trafficFlow: TrafficFlow)
  extends MOBILVehicle {

  private var _distance = 0.0
  private var _speed = random * desiredSpeed
  private var _lane = VehicleImpl.getRandomLane(_trafficFlow.lanes)
  private var _acceleration = 0.0

  val length = 5.0

  private var direction: Direction = null
  private var movementStrategy: MovementStrategy = null

  private var nextIntersection: Intersection = null

  private var endOfFlow: VirtualVehicle = null
  private var startOfFlow: VirtualVehicle = null
  private var target: VirtualVehicle = null

  changeTrafficFlow(_trafficFlow)

  private def changeTrafficFlow(trafficFlow: TrafficFlow) = {
    _speed = 3
    _acceleration = 0
    _lane = direction match {
      case RIGHT => trafficFlow.lanes
      case BACK => trafficFlow.lanes
      case _ => VehicleImpl.getRandomLane(trafficFlow.lanes)
    }
    _distance = nextIntersection(trafficFlow).distance
    _trafficFlow = trafficFlow
    endOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.end, 1000)
    startOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.start, -1000)
    direction = getRandomDirection
    movementStrategy = MovementStrategy(direction)
    nextIntersection = {
      if (_trafficFlow.intersections.isEmpty) null
      else
        _trafficFlow.intersections
          .reduceLeft((i1, i2) => if (i1(_trafficFlow).distance < i2(_trafficFlow).distance) i1 else i2)
    }
    target = if (direction == FORWARD) endOfFlow else VirtualVehicle(_trafficFlow, nextIntersection.location, -minimumGap)
  }

  def headVehicle(lane: Int = lane): Vehicle = {
    val vehicles = target :: _trafficFlow.vehicles.filter(_.lane == lane).toList :::
      _trafficFlow.trafficLights.filter(_.color == RED)
        .map(l => VirtualVehicle(trafficFlow, l.location)).toList
    val vehiclesMap = vehicles.map(v => (v.distance, v)).toMap
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
    movementStrategy.move(timeStep)
  }

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def speed: Double = _speed

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

  private def getRandomDirection = {
    if (nextIntersection == null) FORWARD
    else {
      FORWARD  //todo
    }
  }

  private trait MovementStrategy {
    def move(timeStep: Double)
  }

  private object ForwardStrategy extends MovementStrategy {
    override def move(timeStep: Double): Unit = {
      if (nextIntersection != null && _distance > nextIntersection(_trafficFlow).distance) {
        //todo: debug
        nextIntersection = nextIntersection.next(_trafficFlow)
        movementStrategy = MovementStrategy(getRandomDirection)
      }
      _lane = mobil.lane
      _acceleration = idm.acceleration
      _distance += _speed * timeStep + 0.5 * _acceleration * pow(timeStep, 2)
      _speed += _acceleration * timeStep
    }
  }

  private object MovementStrategy {
    //todo
    private val strategies: Map[Direction, MovementStrategy] = Map(
      FORWARD -> ForwardStrategy,
      LEFT -> ForwardStrategy,
      RIGHT -> ForwardStrategy,
      BACK -> ForwardStrategy
    )

    def apply(direction: Direction) = strategies(direction)
  }

}

object VehicleImpl {
  private val random = new Random(System.nanoTime())

  private def getRandomLane(lanes: Int) = random.nextInt(lanes) + 1
}

