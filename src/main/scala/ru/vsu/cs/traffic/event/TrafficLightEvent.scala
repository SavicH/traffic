package ru.vsu.cs.traffic.event

import ru.vsu.cs.traffic.TrafficLight

trait TrafficLightEvent { }

case class ColorChanged(trafficLight: TrafficLight) extends TrafficLightEvent
case class BeforeColorChanged(trafficLight: TrafficLight) extends TrafficLightEvent
