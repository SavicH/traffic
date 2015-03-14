package ru.vsu.cs.traffic.event

trait TrafficModelEvent

case class ModelActed(time: Double) extends TrafficModelEvent
