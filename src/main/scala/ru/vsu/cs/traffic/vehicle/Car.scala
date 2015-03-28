package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.{TrafficFlow, TrafficModel}

class Car(trafficFlow: TrafficFlow, m: TrafficModel, lane: Int) extends MOBILVehicleImpl(trafficFlow, m, lane) {
}
