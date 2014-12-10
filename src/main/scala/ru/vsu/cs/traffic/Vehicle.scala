package ru.vsu.cs.traffic

trait Vehicle {

  def Move: Vehicle

  def trafficFlow: TrafficFlow

  def distance: Double

  def location: Point

  def lane: Int

  def length: Double

  def velocity: Double

  def acceleration: Double
}
