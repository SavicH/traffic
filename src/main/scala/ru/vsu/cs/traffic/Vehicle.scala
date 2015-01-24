package ru.vsu.cs.traffic

trait Vehicle {

  private[traffic] def move: Vehicle

  def trafficFlow: TrafficFlow

  def distance: Double

  def location: Point

  def lane: Int

  def length: Double

  def velocity: Double

  def acceleration: Double
}
