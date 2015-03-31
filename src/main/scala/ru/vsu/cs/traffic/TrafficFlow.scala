package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.event.{VehicleRemoved, VehicleSpawned}
import ru.vsu.cs.traffic.vehicle.VirtualVehicle

import scala.collection.mutable
import scala.util.Random

trait TrafficFlow extends TrafficActor {

  val start: Point

  val end: Point

  val lanes: Int

  val length: Double = start -- end

  private val VirtualOffset = 500.0

  private[traffic] val virtualStart = VirtualVehicle(this, start, -VirtualOffset)

  private[traffic] val virtualEnd = VirtualVehicle(this, end, VirtualOffset)

  def neighbour: TrafficFlow

  def vehicles: Seq[Vehicle]

  def intersections: Seq[Intersection]

  def trafficLights: Seq[TrafficLight] = intersections.map(_(this))

  private[traffic] def clear(): Unit

  private[traffic] def &(other: TrafficFlow): Point

  private[traffic] def &&(other: TrafficFlow): Intersection

  private[traffic] def <>(other: TrafficFlow): Direction = util.line.direction(start, end, other.start, other.end)

  private[traffic] def +=(v: Vehicle): Unit

  private[traffic] def -=(v: Vehicle): Unit

  private val random = new Random(System.nanoTime())

  private[traffic] def randomLane = random.nextInt(lanes) + 1

  protected def ++=(intersection: Intersection): Unit
}

object TrafficFlow {
  def apply(model: TrafficModel, start: Point, end: Point, lanes: Int, isOneWay: Boolean, probability: Probability, neighbourProbability: Probability = null): TrafficFlow = {
    new TrafficFlowImpl(model, start, end, lanes, isOneWay, probability, neighbourProbability)
  }

  private def apply(model: TrafficModel, start: Point, end: Point, lanes: Int, probability: Probability, neighbour: TrafficFlow): TrafficFlow = {
    new TrafficFlowImpl(model, start, end, lanes, probability, neighbour)
  }

  private class TrafficFlowImpl
  (
    m: TrafficModel,
    val start: Point,
    val end: Point,
    val lanes: Int,
    _isOneWay: Boolean,
    private val probability: Probability,
    secondProbability: Probability
    ) extends TrafficFlow {

    private var _vehicles = new mutable.HashSet[Vehicle]() //todo: WTF

    private var _neighbour: TrafficFlow = if (_isOneWay) null else TrafficFlow(m, end, start, lanes, if (secondProbability != null) secondProbability else probability, this)

    private var _intersections = mutable.MutableList[Intersection]()

    def this(model: TrafficModel, start: Point, end: Point, lanes: Int, probability: Probability, neighbour: TrafficFlow) = {
      this(model, start, end, lanes, _isOneWay = true, probability, null) //isOneWay = true to prevent recursion
      _neighbour = neighbour
    }

    def neighbour = _neighbour

    override def vehicles: Seq[Vehicle] = _vehicles.toList

    override def intersections: Seq[Intersection] = _intersections.toList

    override protected def ++=(intersection: Intersection): Unit = {
      _intersections += intersection
    }

    override private[traffic] def &(other: TrafficFlow): Point = {
      util.line.intersection(start, end, other.start, other.end)
    }

    override private[traffic] def &&(other: TrafficFlow): Intersection = {
      val point = this & other
      if (point == null) null
      else {
        val intersection = Intersection(model, this, other)
        this ++= intersection
        other ++= intersection
        if (this.neighbour != null) this.neighbour ++= intersection
        if (other.neighbour != null) other.neighbour ++= intersection
        intersection
      }
    }

    override private[traffic] def +=(v: Vehicle): Unit = {
      _vehicles += v
    }

    override private[traffic] def -=(v: Vehicle): Unit = {
      _vehicles -= v
      if (v.distance > length) {
        model ! VehicleRemoved(v)
      }
    }

    private val VehicleSpawnMinDelay = 2.0
    private var vehicleSpawnDelay = VehicleSpawnMinDelay
    private var time = 0.0


    override private[traffic] val model: TrafficModel = m

    override protected def onReceive(message: Any): Unit = message match {
      case Time(timeStep) => act(timeStep)
    }

    private val lastVehicles = mutable.Map[Int, Vehicle]()

    private[traffic] def clear(): Unit = {
      _vehicles.clear()
    }

    override def act(timeStep: Double) = {
      if (vehicleSpawnDelay >= VehicleSpawnMinDelay) {
        if (math.random < probability(time) * timeStep) {
          for (i <- 1 to math.ceil(probability(time) * timeStep).toInt) {
            val lane = randomLane
            val vehicle = Vehicle(model, this, lane, lastVehicles.getOrElse(lane, null))
            _vehicles += vehicle
            lastVehicles(lane) = vehicle
            model ! VehicleSpawned(vehicle)
            model ! Done()
          }
          vehicleSpawnDelay = 0
        }
      } else {
        vehicleSpawnDelay += timeStep
      }
      time += timeStep
      _vehicles.foreach(_ ! Time(timeStep))
    }

    override def toString: String = {
      "TrafficFlow: " + start + " " + end
    }
  }

}

