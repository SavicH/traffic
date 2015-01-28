package ru.vsu.cs.traffic

trait TrafficActor {

  val DefaultTimeStep = 1.0

  private[traffic] def act(timeStep: Double): Unit

}
